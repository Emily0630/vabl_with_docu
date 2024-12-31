#' Compute Unique Weights for Each Pattern for BRL hash
#'
#' This helper function calculates the weight for each pattern based on the
#' current \eqn{m} and \eqn{u} parameters.
#'
#' @param m A numeric vector of length \eqn{L} for the current \eqn{m} parameters.
#' @param u A numeric vector of length \eqn{L} for the current \eqn{u} parameters.
#' @param unique_patterns A \code{matrix} of size \eqn{P \times L} giving the
#'   one-hot encoding of each pattern.
#'
#' @return A numeric vector of length \eqn{P} where each entry is
#'   \eqn{\exp(\sum_j \log(m_j / u_j))} over the columns active in that pattern.
#'
#' @examples
#' \dontrun{
#' weights <- compute_unique_weights(m, u, unique_patterns)
#' }
#' @export
compute_unique_weights <- function(m, u, unique_patterns) {
  # ratio: log(m) - log(u)
  ratio_vec <- log(m) - log(u)

  # Repeat ratio in row-major order and multiply by unique_patterns
  # Sum across columns, then exponentiate
  ratio_mat <- matrix(ratio_vec, nrow = nrow(unique_patterns), ncol = length(ratio_vec),
                      byrow = TRUE)
  exp(rowSums(ratio_mat * unique_patterns, na.rm = TRUE))
}
