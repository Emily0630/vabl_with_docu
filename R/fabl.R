#' Fast Bipartite Record Linkage (FABL)
#'
#' Implements bipartite record linkage with a sampling mechanism.
#' This function uses a Gibbs sampler to sample m/u parameters and
#' link states (Z) for records in the second data frame.
#'
#' This function relies on two helper functions:
#' \enumerate{
#'   \item \code{\link{fabl_compute_m_u}} to update m and u for fabl
#'   \item \code{\link{fabl_sample_Z}} to sample Z values for fabl
#' }
#'
#' @param hash A list containing:
#'   \describe{
#'     \item{\code{ohe}}{A \code{P x L} matrix of unique patterns.}
#'     \item{\code{total_counts}}{A numeric vector of length \code{P},
#'       indicating how many times each pattern occurs in total.}
#'     \item{\code{hash_count_list}}{A list of length \code{n2}, where each entry
#'       is a numeric vector of length \code{P}.}
#'     \item{\code{hash_to_file_1}}{A list used for sampling which \code{df1} record
#'       belongs to each pattern for each record in \code{df2}.}
#'     \item{\code{field_marker}}{An integer vector indicating which field each
#'       column belongs to.}
#'     \item{\code{n1}}{Number of records in the first data frame.}
#'     \item{\code{n2}}{Number of records in the second data frame.}
#'   }
#' @param m_prior, u_prior Numeric prior distribution parameters for m and u (Dirichlet).
#' @param alpha, beta Numeric shape parameters for the Beta prior on the linkage probability \code{\pi}.
#' @param S Integer number of Gibbs iterations.
#' @param burn Integer number of iterations to discard as burn-in (defaults to 10\% of \code{S}).
#' @param show_progress Logical; if \code{TRUE}, prints progress at intervals.
#'
#' @return A list with:
#'   \describe{
#'     \item{\code{Z}}{An \code{n2 x (S - burn)} matrix of sampled link states
#'       (record IDs in \code{df1}), with \code{n1+1} representing "no link".}
#'     \item{\code{m}}{A \code{length(field_marker) x (S - burn)} matrix of sampled M parameters.}
#'     \item{\code{u}}{A \code{length(field_marker) x (S - burn)} matrix of sampled U parameters.}
#'     \item{\code{overlap}}{A numeric vector of length \code{S - burn} giving
#'       the number of linked pairs after each iteration.}
#'     \item{\code{pi}}{A numeric vector of length \code{S - burn} giving the
#'       sampled linkage probability \code{\pi}.}
#'   }
#'
#' @examples
#' \dontrun{
#' # fabl_out <- fabl(hash_data, m_prior=1, u_prior=1, alpha=1, beta=1, S=1000)
#' }
#'
#' @export

fabl <- function(
      hash,
      m_prior = 1,
      u_prior = 1,
      alpha = 1,
      beta = 1,
      S = 1000,
      burn = round(S * 0.1),
      show_progress = TRUE
  ){
  #------------------------------------------------------------------
  # 1) Extract data from 'hash'
  #------------------------------------------------------------------
  num_records_df1 <- hash$n1
  num_records_df2 <- hash$n2
  field_marker_vec <- hash$field_marker

  unique_patterns <- hash$ohe           # (P x L)
  pattern_counts <- hash$total_counts  # length P
  hash_count_list <- hash$hash_count_list
  hash_to_file_1 <- hash$hash_to_file_1

  P <- nrow(unique_patterns)  # number of patterns
  L <- length(field_marker_vec)  # number of columns/features

  #------------------------------------------------------------------
  # 2) Allocate storage for MCMC output
  #------------------------------------------------------------------
  Z_samps <- matrix(0, nrow = num_records_df2, ncol = S)
  m_samps <- matrix(NA, nrow = L, ncol = S)
  u_samps <- matrix(NA, nrow = L, ncol = S)
  L_samps <- numeric(S)
  pi_samps <- numeric(S)

  #------------------------------------------------------------------
  # 3) Initialize
  #------------------------------------------------------------------
  # Z: pattern index (0..P) for each record in df2
  Z <- rep(0, num_records_df2)
  # Overlap = # matched pairs
  L_val <- 0
  # matches_vec: how many times each pattern (1..P) is chosen
  matches_vec <- rep(0, P)

  #------------------------------------------------------------------
  # 4) Gibbs Sampler
  #------------------------------------------------------------------
  for (s in seq_len(S)) {

    # (a) Matched & nonmatched counts => A, B
    AZ <- sweep(unique_patterns, 1, matches_vec, `*`) %>%
      colSums() %>%
      unname()
    nonmatches_vec <- pattern_counts - matches_vec
    BZ <- sweep(unique_patterns, 1, nonmatches_vec, `*`) %>%
      colSums() %>%
      unname()

    # (b) Update m & u using Dirichlet draws using the helper function fabl_compute_m_u()
    temp_mu <- fabl_compute_m_u(AZ, BZ, m_prior, u_prior, field_marker_vec)
    m_vec <- temp_mu$m_vec
    u_vec <- temp_mu$u_vec

    # (c) Compute pattern weights
    ratio <- (log(m_vec) - log(u_vec))
    ratio_mat <- matrix(rep(ratio, P), nrow = P, byrow = TRUE)
    unique_weights <- exp(rowSums(ratio_mat * unique_patterns, na.rm = TRUE))

    # (d) Build hash_weights = list of length n2 => each is hash_count_list[[j]] * unique_weights
    hash_weights <- lapply(hash_count_list, function(x) x * unique_weights)

    # (e) Sample pi from Beta(L_val+alpha, n2 - L_val + beta)
    pi_val <- rbeta(1, L_val + alpha, (num_records_df2 - L_val) + beta)

    # (f) Use the helper function fabl_sample_Z() to re-sample each record j
    samp_out <- fabl_sample_Z(
      Z = Z,
      L_val = L_val,
      n1 = num_records_df1,
      n2 = num_records_df2,
      P = P,
      hash_weights = hash_weights,
      pi_val = pi_val,
      hash_to_file_1 = hash_to_file_1
    )

    # Extract updated Z, L_val, and real_ids
    Z <- samp_out$Z
    L_val <- samp_out$L_val
    real_ids <- samp_out$real_ids  # length n2

    # Store real_ids in Z_samps (the "df1" ID or n1+1 for no link)
    Z_samps[, s] <- real_ids

    # (g) Count how many times each pattern is chosen => matches_vec
    #     (Z is 0..P, with 0 means "no link")
    Z_factor <- factor(Z, levels = 0:P)
    df_Z <- data.frame(Z_factor)
    matches_tab <- df_Z %>%
      dplyr::group_by(Z_factor, .drop = FALSE) %>%
      dplyr::count() %>%
      dplyr::filter(Z_factor != 0) %>%
      dplyr::pull(n)
    matches_vec <- matches_tab

    # (h) Store iteration results
    m_samps[, s] <- m_vec
    u_samps[, s] <- u_vec
    L_samps[s]   <- L_val
    pi_samps[s]  <- pi_val

    # (i) Optional progress
    if (show_progress && (s %% max(1, (S / 100)) == 0)) {
      flush.console()
      cat("\r", paste("Simulation:", s / (S / 100), "% complete"))
    }
  } # end for(s in seq_len(S))

  #------------------------------------------------------------------
  # 5) Post-processing: burn-in removal
  #------------------------------------------------------------------
  keep_iters <- seq.int(burn + 1, S)
  Z_final <- Z_samps[, keep_iters, drop = FALSE]
  m_final <- m_samps[, keep_iters, drop = FALSE]
  u_final <- u_samps[, keep_iters, drop = FALSE]
  L_final <- L_samps[keep_iters]
  pi_final <- pi_samps[keep_iters]

  #------------------------------------------------------------------
  # 6) Return final results
  #------------------------------------------------------------------
  list(
    Z = Z_final,
    m = m_final,
    u = u_final,
    overlap = L_final,
    pi = pi_final
  )
}
