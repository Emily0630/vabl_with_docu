#' Compute m_p, u_p, and weights
#'
#' Given the digamma chunks for \code{a} and \code{b}, and the pattern matrix
#' \code{ohe}, this function computes:
#' \itemize{
#'   \item \code{m_p} = rowSums(ohe * a_chunk)
#'   \item \code{u_p} = rowSums(ohe * b_chunk)
#'   \item \code{weights} = m_p - u_p
#' }
#'
#' @param ohe A \code{P x L} matrix of patterns (0/1).
#' @param a_chunk A length-L numeric vector for the \code{a} part.
#' @param b_chunk A length-L numeric vector for the \code{b} part.
#'
#' @return A list with elements \code{m_p}, \code{u_p}, and \code{weights}.
#'
#' @export
vabl_compute_m_u_p <- function(ohe, a_chunk, b_chunk) {
  m_p <- ohe %>%
    sweep(., 2, a_chunk, "*") %>%
    rowSums()
  u_p <- ohe %>%
    sweep(., 2, b_chunk, "*") %>%
    rowSums()
  weights <- m_p - u_p
  list(m_p = m_p, u_p = u_p, weights = weights)
}
