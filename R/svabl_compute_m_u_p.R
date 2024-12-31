#' Compute m_p, u_p, and Weights for SVABL
#'
#' For each pattern \eqn{p}, \code{m_p = rowSums(ohe * a_chunk)},
#' \code{u_p = rowSums(ohe * b_chunk)}, and \code{weights = m_p - u_p}.
#'
#' @param ohe A \code{P x L} 0/1 matrix of patterns.
#' @param a_chunk,b_chunk Length-\code{L} numeric vectors from digamma differences.
#'
#' @return A list containing \code{m_p}, \code{u_p}, and \code{weights} (each length \code{P}).
#'
#' @export
svabl_compute_m_u_p <- function(ohe, a_chunk, b_chunk) {
  temp_m <- sweep(ohe, 2, a_chunk, "*")
  if (!is.matrix(temp_m)) {
    temp_m <- matrix(temp_m, nrow=nrow(ohe), ncol=ncol(ohe))
  }
  m_p <- rowSums(temp_m)

  temp_u <- sweep(ohe, 2, b_chunk, "*")
  if (!is.matrix(temp_u)) {
    temp_u <- matrix(temp_u, nrow=nrow(ohe), ncol=ncol(ohe))
  }
  u_p <- rowSums(temp_u)

  weights <- m_p - u_p
  list(m_p = m_p, u_p = u_p, weights = weights)
}
