#' Compute phi, single, and C
#'
#' Computes:
#' \itemize{
#'   \item \code{phi = exp(digamma(a_pi) - digamma(n1) + weights)}
#'   \item \code{single = exp(digamma(b_pi))}
#'   \item \code{C[j] = sum_{p} (hash_count_list[[j]][p] * phi[p]) + single}
#'   \item \code{total_nonmatch = sum(single / C)}
#' }
#'
#' @param m_p A numeric vector of length \code{P}.
#' @param u_p A numeric vector of length \code{P}.
#' @param weights A numeric vector of length \code{P}.
#' @param a_pi,b_pi Scalars for Beta distribution parameters.
#' @param n1 An integer, number of records in the first data set.
#' @param hash_count_list A list of length \code{n2}, each a numeric vector (length \code{P}).
#'
#' @return A list with \code{phi}, \code{single}, \code{C}, and \code{total_nonmatch}.
#'
#' @export
vabl_compute_phi_C <- function(m_p, u_p, weights,
                               a_pi, b_pi, n1,
                               hash_count_list) {
  phi <- exp(digamma(a_pi) - digamma(n1) + weights)
  single <- exp(digamma(b_pi))

  C <- sapply(hash_count_list, function(x){
    x %*% phi + single
  })

  total_nonmatch <- sum(single / C)

  list(phi = phi, single = single, C = C, total_nonmatch = total_nonmatch)
}
