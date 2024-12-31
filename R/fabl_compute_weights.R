#' Compute Weights for Patterns for fabl
#'
#' Computes the ratio \eqn{\log(m) - \log(u)} for each column, replicates it for
#' each pattern, and then exponentiates the row sums to obtain pattern-specific
#' weights.
#'
#' @param m_vec A numeric vector for the current M parameters (one per column).
#' @param u_vec A numeric vector for the current U parameters (one per column).
#' @param unique_patterns A numeric matrix (\eqn{P \times L}) indicating which
#'   columns are active for each pattern.
#'
#' @return A numeric vector of length \code{P}, where \code{P} is the number of
#'   unique patterns, containing the weight for each pattern.
#'
#' @examples
#' m_vec <- c(0.8, 0.2)
#' u_vec <- c(0.5, 0.5)
#' unique_patterns <- matrix(c(1,0, 0,1), nrow=2, byrow=TRUE)
#' w <- compute_weights(m_vec, u_vec, unique_patterns)
#' @export
fabl_compute_weights <- function(m_vec, u_vec, unique_patterns) {
  # ratio: log(m) - log(u)
  ratio <- (log(m_vec) - log(u_vec))

  # replicate ratio P times (one row per pattern),
  # then multiply by unique_patterns rowwise
  # => rowSums => exponentiate => final pattern weights
  P <- nrow(unique_patterns)
  ratio_mat <- matrix(rep(ratio, P), nrow = P, byrow = TRUE)
  exp(rowSums(ratio_mat * unique_patterns, na.rm = TRUE))
}
