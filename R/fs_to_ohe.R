#' Convert Field-Specific Indices to One-Hot Encoding
#'
#' Given a vector of field-specific indices (where a \code{0} indicates "no level selected"
#' and a positive integer \code{z} indicates the \code{z}-th level is active),
#' this function produces a concatenated one-hot vector for all fields.
#'
#' @param row_values An integer vector of length \code{length(field_levels)},
#'   where each element is either \code{0} or in \code{1:field_levels[i]}.
#' @param field_levels An integer vector indicating how many levels each field has.
#'
#' @return A numeric vector of length \code{sum(field_levels)}, which is the
#'   concatenation of all fields' one-hot sub-vectors. Each sub-vector is of
#'   length \code{field_levels[i]}, with exactly one position set to \code{1}
#'   if \code{row_values[i] > 0}, or all zeros if \code{row_values[i] == 0}.
#'
#' @examples
#' # Suppose we have two fields:
#' #   - field1 has 2 levels
#' #   - field2 has 3 levels
#' # Then field_levels = c(2, 3).
#' # If row_values = c(1, 2), it means field1=1, field2=2.
#' #   => fs_to_ohe(c(1,2), c(2,3)) = c(1,0, 0,1,0)
#'
#' row_values <- c(1, 2)
#' field_levels <- c(2, 3)
#' fs_to_ohe(row_values, field_levels)
#'
#' @export
fs_to_ohe <- function(row_values, field_levels) {
  unname(
    unlist(
      mapply(
        FUN = function(z, y) {
          # Create a zero vector for this field
          gamma_f <- numeric(y)

          # If z==0 => keep all zeros
          # If z>0 => set the z-th position to 1
          if (z != 0) {
            gamma_f[z] <- 1
          }
          gamma_f
        },
        z = row_values,
        y = field_levels
      )
    )
  )
}
