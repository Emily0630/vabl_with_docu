#' Subsample or Return Entire Vector
#'
#' This function returns the input vector \code{x} as is if its length
#' does not exceed \code{R}. Otherwise, it returns a random subsample
#' of length \code{R}.
#'
#' @param x A vector (numeric, character, etc.).
#' @param R A non-negative integer indicating the maximum size of the returned sample.
#'
#' @return A vector:
#'   \itemize{
#'     \item If \code{length(x) <= R}, then \code{x} is returned in full.
#'     \item Otherwise, a random subsample of size \code{R} is returned.
#'   }
#'
#' @examples
#' # If x has length 5 and R=3, we get a random subset of 3 elements:
#' sei(1:5, 3)
#'
#' # If x has length 5 and R=10, we get x untouched:
#' sei(1:5, 10)
#'
#' @export
sei <- function(x, R) {
  # If x's length is at most R, return x unmodified
  if (length(x) <= R) {
    return(x)
  }
  # Otherwise, randomly sample R elements without replacement
  sample(x, R, replace = FALSE)
}
