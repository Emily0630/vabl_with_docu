#' One-Hot Encode a Factor
#'
#' Converts a factor into a one-hot (dummy) encoded matrix. The resulting matrix
#' has one row per element of the factor and one column per factor level.
#'
#' @param fct A factor whose levels will be used to create dummy variables.
#'
#' @return A numeric matrix of size \code{length(fct)} rows by
#'   \code{nlevels(fct)} columns, where each column corresponds to one factor
#'   level. The matrix entries are either 0 or 1, indicating the absence or
#'   presence of that particular level for that row.
#'
#' @examples
#' f <- factor(c("A", "B", "B", "C"), levels = c("A", "B", "C"))
#' one_hot_encode_factor(f)
#'
#' @export
one_hot_encode_factor <- function(fct) {
  num_rows <- length(fct)
  num_levels <- nlevels(fct)

  # Initialize a matrix of zeros, then set 1 in the appropriate spots
  mat <- matrix(0, nrow = num_rows, ncol = num_levels)
  for (lev_idx in seq_len(num_levels)) {
    mat[fct == lev_idx, lev_idx] <- 1
  }
  mat
}
