#' Stochastic Variational Algorithm for Bipartite Linkage (SVABL)
#'
#' This function implements a stochastic variational inference procedure for
#' bipartite record linkage, using minibatches of records and updating
#' the Dirichlet parameters \code{a} and \code{b} with a decreasing
#' step size \code{epsilon = (t + tau) ^ (-k)}. The function also supports
#' storing the evidence lower bound (ELBO) at given intervals and can stop
#' early based on a convergence threshold or run for a fixed number of iterations.
#'
#' @param hash A list containing:
#'   \describe{
#'     \item{\code{ohe}}{A \code{P x L} matrix (0/1) of unique patterns.}
#'     \item{\code{total_counts}}{A numeric vector of length \code{P}, how many
#'       times each pattern occurs in total.}
#'     \item{\code{hash_count_list}}{A list of length \code{n2}, each a numeric
#'       vector of length \code{P} indicating how many times each pattern is
#'       observed for that record.}
#'     \item{\code{field_marker}}{An integer vector labeling each column (feature)
#'       to a field index.}
#'     \item{\code{n1}}{Number of records in the first data set.}
#'     \item{\code{n2}}{Number of records in the second data set.}
#'   }
#' @param threshold Numeric convergence threshold for the relative change in
#'   ELBO. Defaults to \code{1e-6}.
#' @param tmax An integer specifying the maximum number of iterations. Defaults
#'   to \code{1000}.
#' @param fixed_iterations If \code{NULL} (default), the algorithm stops when
#'   the relative ELBO change is below \code{threshold}. Otherwise, it ignores
#'   \code{threshold} and runs exactly \code{fixed_iterations} steps (unless
#'   \code{tmax} is exceeded).
#' @param b_init Logical. If \code{TRUE}, initializes \code{b} using the pattern
#'   matrix \code{ohe} weighted by \code{total_counts}, otherwise \code{b} is
#'   a vector of ones. Defaults to \code{TRUE}.
#' @param B Integer minibatch size, defaults to \code{min(1000, hash$n2)}.
#' @param k Numeric exponent for the step size \code{(t + tau)^(-k)}. Defaults
#'   to \code{1}.
#' @param tau Numeric offset for the step size. Defaults to \code{1}.
#' @param seed An integer seed for reproducibility (default \code{0}).
#' @param check_every Integer, how often to check convergence (default \code{10}).
#' @param store_every Integer, how often to compute/store ELBO (default
#'   \code{check_every}).
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{pattern_weights}}{A length-\code{P} vector \code{phi} representing
#'     the final pattern weights.}
#'   \item{\code{C}}{A length-\code{n2} numeric vector of normalizing constants for
#'     each record in the second data set.}
#'   \item{\code{a,b}}{Length-\code{L} numeric vectors of Dirichlet parameters.}
#'   \item{\code{a_pi,b_pi}}{Scalars for the Beta distribution controlling the
#'     linking probability.}
#'   \item{\code{elbo_seq}}{A numeric vector of stored ELBO values over iterations.}
#'   \item{\code{t}}{An integer indicating the final iteration count.}
#' }
#'
#' @examples
#' \dontrun{
#' out <- svabl(hash_data, threshold=1e-6, tmax=1000, B=500, k=0.5, tau=1)
#' }
#'
#' @export
svabl <- function(hash, threshold = 1e-6, tmax = 1000, fixed_iterations = NULL,
                  b_init = TRUE, B = min(1000, hash$n2),
                  k = 1, tau = 1, seed = 0,
                  check_every = 10, store_every = check_every){

  set.seed(seed)

  # ----------------------------------------------------------------
  # 1) Extract data and define priors
  # ----------------------------------------------------------------
  setup_list  <- svabl_setup(hash, b_init)
  a           <- setup_list$a
  b           <- setup_list$b
  alpha       <- setup_list$alpha
  Beta        <- setup_list$Beta
  alpha_pi    <- 1
  beta_pi     <- 1

  n1                <- setup_list$n1
  n2                <- setup_list$n2
  ohe               <- setup_list$ohe
  field_marker      <- setup_list$field_marker
  hash_count_list   <- setup_list$hash_count_list
  P                 <- setup_list$P

  # Additional initialization for Beta distribution controlling links
  a_pi <- 1
  b_pi <- 1

  # ----------------------------------------------------------------
  # 2) Initialize iteration counters, storage
  # ----------------------------------------------------------------
  t           <- 1
  ratio       <- 1
  elbo_seq    <- numeric(0)
  adjustment  <- n2 / B  # factor to scale from minibatch to full data

  # ----------------------------------------------------------------
  # 3) Main loop
  # ----------------------------------------------------------------
  while(t <= tmax){

    # (a) Compute the chunked digamma for a, b
    chunk_list <- svabl_compute_chunks(a, b, field_marker)
    a_chunk <- chunk_list$a_chunk
    b_chunk <- chunk_list$b_chunk

    # (b) Compute m_p, u_p, and weights
    mu_list <- svabl_compute_m_u_p(ohe, a_chunk, b_chunk)
    m_p <- mu_list$m_p
    u_p <- mu_list$u_p
    weights <- mu_list$weights

    # (c) Compute phi, single
    phi <- exp(digamma(a_pi) - digamma(n1) + weights)
    single <- exp(digamma(b_pi))

    # (d) Sample a minibatch (size B) from the second data set
    batch_indices <- sample.int(n2, B, replace = FALSE)

    # (e) For each record j in the batch, compute C[j]
    #     then scale up by 'adjustment'
    C_batch <- sapply(batch_indices, function(j){
      hash_count_list[[j]] %*% phi + single
    })

    # (f) total_nonmatch => scaled sum(single / C_batch)
    total_nonmatch <- adjustment * sum(single / C_batch)

    # (g) Recompute total_counts over the batch => scale up
    #     Then N_p(B) => partial usage
    total_counts_batch <- svabl_compute_batch_counts(hash_count_list, batch_indices, adjustment)

    K <- svabl_compute_K(hash_count_list, batch_indices, C_batch, adjustment)

    # (h) Step size: epsilon = (t + tau)^(-k)
    epsilon <- (t + tau) ^ (-k)

    # (i) Update a, b
    #     AZ => ohe * (phi*K)
    #     BZ => ohe * (total_counts_batch - phi*K)
    update_list <- svabl_update_ab(ohe, phi, K, total_counts_batch,
                                   a, b, alpha, Beta, epsilon)
    a <- update_list$a
    b <- update_list$b

    # (j) Update a_pi, b_pi
    #     (1 - eps)*a_pi + eps*(alpha_pi + n2 - total_nonmatch)
    a_pi <- (1 - epsilon)*a_pi + epsilon*(1 + n2 - total_nonmatch)
    b_pi <- (1 - epsilon)*b_pi + epsilon*(1 + total_nonmatch)

    # (k) Possibly compute full-data ELBO
    if(t %% store_every == 0 || t == 1){
      # Compute full C for all records
      c_full <- sapply(seq_len(n2), function(j){
        hash_count_list[[j]] %*% phi + single
      })
      # Then compute full-data ELBO
      new_elbo <- svabl_compute_elbo(
        a, b, a_pi, b_pi,
        alpha, Beta, 1, 1, # alpha_pi,beta_pi = 1,1
        hash_count_list, n1, n2,
        ohe, field_marker, weights, phi, single, c_full, m_p, u_p, a_chunk, b_chunk
      )
      elbo_seq <- c(elbo_seq, new_elbo)
    }

    # (l) Check convergence if not fixed_iterations
    if(is.null(fixed_iterations)){
      if(t %% check_every == 0 && length(elbo_seq) > 1){
        idx   <- length(elbo_seq)
        ratio <- abs((elbo_seq[idx] - elbo_seq[idx-1]) / elbo_seq[idx-1])
      }
      if(ratio < threshold){
        break
      }
    }

    t <- t + 1
    if(t > tmax){
      message("Max iterations have passed before convergence.")
      break
    }

    if(!is.null(fixed_iterations) && t == fixed_iterations){
      break
    }
  } # end while

  # ----------------------------------------------------------------
  # 4) Final C over all records
  # ----------------------------------------------------------------
  C <- sapply(seq_len(n2), function(j){
    hash_count_list[[j]] %*% phi + single
  })

  # ----------------------------------------------------------------
  # 5) Return final results
  # ----------------------------------------------------------------
  list(
    pattern_weights = phi,
    C              = C,
    a              = a,
    b              = b,
    a_pi           = a_pi,
    b_pi           = b_pi,
    elbo_seq       = elbo_seq,
    t              = t
  )
}
