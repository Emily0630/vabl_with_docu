#' Compute AZ, BZ
#'
#' Uses the expression:
#' \itemize{
#'   \item \code{AZ = colSums( ohe * (phi * K) )}
#'   \item \code{BZ = colSums( ohe * (total_counts - phi*K) )}
#' }
#'
#' @param ohe A \code{P x L} pattern matrix.
#' @param phi A length-P numeric vector.
#' @param K A length-P numeric vector (partial usage).
#' @param total_counts A length-P numeric vector with total pattern counts.
#'
#' @return A list with \code{AZ} and \code{BZ}, both length-L.
#'
#' @export
vabl_compute_ABZ <- function(ohe, phi, K, total_counts) {
  AZ <- ohe %>%
    sweep(., 1, phi * K, "*") %>%
    colSums()
  BZ <- ohe %>%
    sweep(., 1, total_counts - (phi * K), "*") %>%
    colSums()
  list(AZ = AZ, BZ = BZ)
}
