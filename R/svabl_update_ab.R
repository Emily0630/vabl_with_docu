#' Update a, b for SVABL
#'
#' Uses the expression:
#' \deqn{
#'   a = (1 - \epsilon) * a + \epsilon * ( \alpha + AZ )
#' }
#' and similarly for \code{b}.
#'
#' @param ohe A \code{P x L} matrix.
#' @param phi A numeric vector of length \code{P}.
#' @param K A numeric vector of length \code{P}.
#' @param total_counts_batch A numeric vector of length \code{P}.
#' @param a,b Vectors of length \code{L}.
#' @param alpha,Beta Vectors of length \code{L}, priors.
#' @param epsilon The step size.
#'
#' @return A list with updated \code{a,b}.
#'
#' @export
svabl_update_ab <- function(ohe, phi, K, total_counts_batch,
                            a, b, alpha, Beta, epsilon) {
  temp_AZ <- sweep(ohe, 1, phi*K, "*")
  if (!is.matrix(temp_AZ)) {
    temp_AZ <- matrix(temp_AZ, nrow=nrow(ohe), ncol=ncol(ohe))
  }
  AZ <- colSums(temp_AZ)

  temp_BZ <- sweep(ohe, 1, total_counts_batch - phi*K, "*")
  if (!is.matrix(temp_BZ)) {
    temp_BZ <- matrix(temp_BZ, nrow=nrow(ohe), ncol=ncol(ohe))
  }
  BZ <- colSums(temp_BZ)

  a_new <- (1 - epsilon) * a + epsilon * (alpha + AZ)
  b_new <- (1 - epsilon) * b + epsilon * (Beta  + BZ)

  list(a = a_new, b = b_new)
}
