#' Variational Algorithm for Bipartite Linkage
#'
#' This function implements a variational inference procedure for bipartite record
#' linkage, with the option to either run until convergence (based on a relative
#' ELBO threshold) or for a fixed number of iterations.
#'
#'This function relies on six helper functions:
#' \enumerate{
#'   \item \code{\link{svabl_setup}} to extract data from \code{hash} and initialize the
#'    parameters \code{a} and \code{b}
#'   \item \code{\link{vabl_svabl_compute_chunks}} to compute digamma chunks for vabl
#'   \item \code{\link{vabl_compute_m_u_p}} to compute m_p, u_p, and weights for vabl
#'   \item \code{\link{vabl_compute_phi_C}} to compute phi, single, and C for vabl
#'   \item \code{\link{vabl_compute_ABZ}} to compute AZ, BZ for vabl
#'   \item \code{\link{vabl_svabl_compute_elbo}} to compute elbo for vabl
#' }
#'
#' @param hash A list containing:
#'   \describe{
#'     \item{\code{ohe}}{A \code{P x L} matrix (0/1) of unique patterns.}
#'     \item{\code{total_counts}}{A numeric vector of length \code{P}, representing
#'       how many times each pattern occurs.}
#'     \item{\code{hash_count_list}}{A list of length \code{n2}, each element is
#'       a numeric vector of length \code{P} counting how many times each pattern
#'       is observed for that record.}
#'     \item{\code{field_marker}}{An integer vector of length \code{L}, labeling
#'       each column to a specific field index.}
#'     \item{\code{n1}}{Number of records in the first data set.}
#'     \item{\code{n2}}{Number of records in the second data set.}
#'   }
#' @param threshold A numeric threshold for checking convergence of the ELBO
#'   (relative change). Defaults to \code{1e-6}.
#' @param tmax An integer specifying the maximum number of iterations to run.
#'   Defaults to \code{1000}.
#' @param fixed_iterations An integer. If \code{NULL} (default), the algorithm
#'   uses \code{threshold} to check convergence. If a positive integer is supplied,
#'   the algorithm ignores \code{threshold} and runs exactly that many iterations
#'   (unless \code{tmax} is exceeded).
#' @param b_init Logical. If \code{TRUE}, initializes \code{b} by summing over
#'   patterns weighted by \code{total_counts}, else \code{b} is a vector of ones.
#'   Defaults to \code{TRUE}.
#' @param check_every An integer indicating how often to check for convergence
#'   (every \code{check_every} iterations). Defaults to \code{10}.
#' @param store_every An integer indicating how often to compute/store the
#'   Evidence Lower BOund (ELBO). Defaults to the same as \code{check_every}.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{pattern_weights}}{A numeric vector \code{phi} of length \code{P}
#'     representing the final weights for each pattern.}
#'   \item{\code{C}}{A numeric vector of length \code{n2} representing the
#'     normalization constants per record.}
#'   \item{\code{a,b}}{Numeric vectors of length \code{L}, the updated Dirichlet
#'     parameters for matching and non-matching.}
#'   \item{\code{a_pi,b_pi}}{Scalars for the Beta parameters controlling the
#'     probability of linking.}
#'   \item{\code{elbo_seq}}{A numeric vector storing the ELBO at each stored iteration.}
#'   \item{\code{t}}{An integer indicating the final iteration count upon exit.}
#' }
#'
#' @examples
#' \dontrun{
#' # Suppose 'hash_data' is a list with the correct fields (ohe, total_counts, etc.)
#' out <- vabl_original(hash_data, threshold=1e-6, tmax=1000)
#' }
#' @export
vabl <- function(hash, threshold = 1e-6, tmax = 1000, fixed_iterations = NULL,
                          b_init = TRUE, check_every = 10, store_every = check_every){

  # -----------------------------------------------------------------
  # 1) Extract data and define priors
  # -----------------------------------------------------------------
  setup <- vabl_setup(hash, b_init)
  a <- setup$a
  b <- setup$b
  a_pi <- 1
  b_pi <- 1

  alpha <- setup$alpha
  Beta <- setup$Beta
  alpha_pi <- 1
  beta_pi <- 1
  n1 <- setup$n1
  n2 <- setup$n2
  ohe <- setup$ohe
  total_counts <- setup$total_counts
  hash_count_list <- setup$hash_count_list
  field_marker <- setup$field_marker
  P <- setup$P

  # -----------------------------------------------------------------
  # 2) Main iteration loop
  # -----------------------------------------------------------------
  t <- 1
  ratio <- 1
  elbo_seq <- vector()

  while(t <= tmax){
    # (a) Compute the chunked digamma differences for a, b
    chunk_list <- vabl_svabl_compute_chunks(a, b, field_marker)
    a_chunk <- chunk_list$a_chunk
    b_chunk <- chunk_list$b_chunk

    # (b) Compute m_p, u_p, weights
    mu_list <- vabl_compute_m_u_p(ohe, a_chunk, b_chunk)
    m_p <- mu_list$m_p
    u_p <- mu_list$u_p
    weights <- mu_list$weights

    # (c) Compute phi, single, C
    c_list <- vabl_compute_phi_C(m_p, u_p, weights, a_pi, b_pi, n1, hash_count_list)
    phi <- c_list$phi
    single <- c_list$single
    C <- c_list$C
    total_nonmatch <- c_list$total_nonmatch

    # (d) Compute K (partial usage)
    K <- sapply(seq_len(n2), function(j){
      hash_count_list[[j]] / C[j]
    }) %>%
      rowSums()

    # (e) Compute AZ, BZ
    ab_list <- vabl_compute_ABZ(ohe, phi, K, total_counts)
    AZ <- ab_list$AZ
    BZ <- ab_list$BZ

    # (f) Update a, b, a_pi, b_pi
    a <- alpha + AZ
    b <- Beta + BZ
    a_pi <- alpha_pi + n2 - total_nonmatch
    b_pi <- beta_pi  + total_nonmatch

    # (g) Possibly compute ELBO
    if(t %% store_every == 0 || t == 1){
      elbo_now <- vabl_svabl_compute_elbo(
        n1, n2, hash_count_list, phi, weights, m_p, u_p, C, single,
        a_pi, b_pi, alpha_pi, beta_pi,
        a, b, alpha, Beta,
        a_chunk, b_chunk,
        field_marker
      )
      elbo_seq <- c(elbo_seq, elbo_now)
    }

    # (h) Check for convergence if not fixed_iterations
    if(is.null(fixed_iterations)){
      if(t %% check_every == 0 && length(elbo_seq) > 1){
        t_elbo <- length(elbo_seq)
        ratio <- abs((elbo_seq[t_elbo] - elbo_seq[t_elbo - 1]) /
                       elbo_seq[t_elbo - 1])
      }
      if(ratio < threshold){
        break
      }
    }

    t <- t + 1
    if(t > tmax){
      message("Max iterations have passed before convergence")
      break
    }

    if(!is.null(fixed_iterations)){
      if(t == fixed_iterations){
        break
      }
    }
  } # end while

  # -----------------------------------------------------------------
  # 3) Return final results
  # -----------------------------------------------------------------
  list(pattern_weights = phi,
       C = C,
       a = a,
       b = b,
       a_pi = a_pi,
       b_pi = b_pi,
       elbo_seq = elbo_seq,
       t = t)
}
