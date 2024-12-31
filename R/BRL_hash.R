#' BRL Hash: Bipartite Record Linkage via BK Sampling
#'
#' Implements a bipartite record linkage model on hashed comparison data
#' using a Gibbs sampler with several sampling modes (\code{"base", "efficient", "rejection"}).
#'
#' @param hash_data A list containing the hashed comparison data, typically
#'   with elements:
#'   \describe{
#'     \item{\code{n1}}{Number of records in first file.}
#'     \item{\code{n2}}{Number of records in second file.}
#'     \item{\code{field_marker}}{Integer vector indicating which field each column belongs to.}
#'     \item{\code{ohe}}{Matrix of size \code{P x L}, the one-hot patterns.}
#'     \item{\code{total_counts}}{A numeric vector of length \code{P}.}
#'     \item{\code{hash_count_list}}{A list of length \code{n2}, each is a numeric vector of length \code{P}.}
#'     \item{\code{hash_to_file_1}}{A nested list to map each \code{(j, pattern)} to the subset of df1 record indices.}
#'     \item{\code{pair_to_pattern}}{A list of length \code{n2}, each a vector that indexes patterns for each df1.}
#'   }
#' @param m_prior,u_prior Numeric, the prior hyperparameters for \eqn{m} and \eqn{u} (Dirichlet).
#' @param alpha,beta Numeric, hyperparameters for the Beta prior on the linkage probability.
#' @param S Integer, number of Gibbs iterations.
#' @param burn Integer, number of burn-in iterations (defaults to 10\% of S).
#' @param show_progress Logical, if \code{TRUE} display simulation progress.
#' @param seed An integer for RNG seeding.
#' @param reject_iter Integer, used in \code{"rejection"} mode to fallback after
#'   \code{reject_iter} attempts.
#' @param mode A string in \code{c("base", "efficient", "rejection")} specifying
#'   the sampling mechanism.
#'
#' @return A list with elements:
#' \describe{
#'   \item{\code{Z}}{A matrix of dimension \code{n2 x S}, the final link assignments
#'        for each iteration. \code{Z[j, s]} is the df1 record matched to df2 record j
#'        in iteration s (or \code{n1+1} if unlinked).}
#'   \item{\code{m}, \code{u}}{Matrices of dimension \code{L x S} containing the draws
#'        for each iteration of \eqn{m} and \eqn{u}.}
#'   \item{\code{overlap}}{A numeric vector of length \code{S}, the number of matches
#'        in each iteration.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- BRL_hash(hash_data, m_prior=1, u_prior=1,
#'                    alpha=1, beta=1, S=1000, mode="rejection")
#' }
#'
#' @export
BRL_hash <- function(hash_data,
                     m_prior = 1, u_prior = 1,
                     alpha = 1, beta = 1,
                     S = 1000, burn = round(S * 0.1),
                     show_progress = TRUE,
                     seed = 0,
                     reject_iter = 10,
                     mode = "rejection") {

  #-----------------------------------------------------------------
  # 1) Set RNG seed and extract data from 'hash_data'
  #-----------------------------------------------------------------
  set.seed(seed)

  n1            <- hash_data$n1
  n2            <- hash_data$n2
  field_marker  <- hash_data$field_marker

  unique_patterns <- hash_data$ohe
  pattern_counts  <- hash_data$total_counts
  P               <- nrow(unique_patterns)

  hash_count_list <- hash_data$hash_count_list
  hash_to_file_1  <- hash_data$hash_to_file_1
  pair_to_pattern <- hash_data$pair_to_pattern

  # We define a vector 0..P for sampling pattern indices (0 => unlinked).
  candidates_P <- 0:P

  #-----------------------------------------------------------------
  # 2) Allocate storage for the MCMC
  #-----------------------------------------------------------------
  Z_samps <- matrix(NA, nrow = n2, ncol = S)
  m_samps <- matrix(NA, nrow = length(field_marker), ncol = S)
  u_samps <- matrix(NA, nrow = length(field_marker), ncol = S)
  L_samps <- numeric(S)

  #-----------------------------------------------------------------
  # 3) Initialize
  #-----------------------------------------------------------------
  Z       <- rep(0, n2)        # link assignment for each j in df2
  Z_pat   <- rep(0, n2)        # pattern used for each j
  Z_inv   <- rep(0, n1)        # indicates if a df1 record is used or not
  L       <- 0                 # number of links
  matches <- rep(0, P)         # frequency of each pattern among matched pairs

  # m & u are updated each iteration, initialize them to zeros
  m <- u <- rep(0, length(field_marker))

  #-----------------------------------------------------------------
  # 4) Main Gibbs Loop
  #-----------------------------------------------------------------
  for (s in seq_len(S)) {

    # --- 4a) Update m and u using our helper 'update_m_u' ---
    res_mu <- update_m_u(unique_patterns, pattern_counts, matches,
                         field_marker, m_prior, u_prior)
    m <- res_mu$m
    u <- res_mu$u

    # --- 4b) Compute weights for each pattern using 'compute_unique_weights' ---
    unique_weights <- compute_unique_weights(m, u, unique_patterns)

    # --- 4c) Sample Z[j] for each record j in df2 ---
    for (j in seq_len(n2)) {
      # Compute the "empty_weight" = prior factor for choosing "no match"
      empty_weight <- (n1 - L) * (n2 - L - 1 + beta) / (L + alpha)

      # Use our helper 'sample_pair' to do mode-specific sampling
      out <- sample_pair(
        j = j,
        Z = Z,
        Z_inv = Z_inv,
        L = L,
        empty_weight = empty_weight,
        unique_weights = unique_weights,
        pair_to_pattern = pair_to_pattern,
        hash_count_list = hash_count_list,
        hash_to_file_1 = hash_to_file_1,
        candidates_P = candidates_P,
        mode = mode,
        reject_iter = reject_iter
      )
      # Update Z[j], Z_pat[j], and L from the helper’s result
      Z[j]      <- out$Z_j
      Z_pat[j]  <- out$Z_pat_j
      L         <- out$L_new
    }

    # --- 4d) Recompute 'matches' = how many times each pattern is used ---
    # Convert Z_pat to factor 0..P, count frequency (excluding 0)
    df <- data.frame(hash_matches = factor(Z_pat, levels = 0:P))
    matches_df <- df %>%
      dplyr::group_by(hash_matches, .drop=FALSE) %>%
      dplyr::count() %>%
      dplyr::filter(hash_matches != 0) %>%
      dplyr::pull(n)

    # If no patterns used, matches_df could be integer(0). Ensure length P
    if (length(matches_df) == 0) {
      matches <- rep(0, P)
    } else {
      matches <- matches_df
    }

    # --- 4e) Store iteration results ---
    Z_samps[, s] <- Z
    m_samps[, s] <- m
    u_samps[, s] <- u
    L_samps[s]   <- L

    # --- 4f) Show progress if desired ---
    if (show_progress && (s %% (S/100) == 0)) {
      flush.console()
      cat("\r", paste("Simulation: ", s/(S/100), "% complete", sep=""))
    }
  }

  #-----------------------------------------------------------------
  # 5) Post-process Z to mark unlinked with (n1+1) instead of 0
  #-----------------------------------------------------------------
  Z_samps[Z_samps == 0] <- n1 + 1

  #-----------------------------------------------------------------
  # 6) Return results
  #-----------------------------------------------------------------
  list(
    Z       = Z_samps,
    m       = m_samps,
    u       = u_samps,
    overlap = L_samps
  )
}
