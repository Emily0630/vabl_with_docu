#' Generate All Possible One-Hot Encoded Patterns
#'
#' Given a vector specifying how many levels each field has, this function:
#' \enumerate{
#'   \item Creates a list of possible values for each field, including \code{0}
#'     and all integer levels \code{1} to \code{field_levels[field]}.
#'   \item Takes the Cartesian product (expand.grid) across these lists to form
#'     every possible combination of values across fields.
#'   \item Converts each combination to a one-hot encoding via \code{fs_to_ohe()},
#'     where each row becomes a one-hot vector of length \code{sum(field_levels)}.
#' }
#'
#' @param field_levels An integer vector, where each element indicates how many
#'   levels a given field has.
#'
#' @return A \code{data.frame} whose rows correspond to all possible one-hot
#'   patterns. The number of rows is
#'   \eqn{\prod_{f} (1 + field\_levels[f])}
#'   (because each field contributes \code{field_levels[f]} actual levels plus
#'   the \code{0} "extra" level, as defined by this function).
#'
#' @examples
#' # Suppose we have 2 fields, the first with 2 levels and the second with 3.
#' # Then 'field_levels = c(2, 3)'. The function will generate
#' # (2+1)*(3+1) = 12 combinations, and each combination is turned into a
#' # one-hot vector.
#'
#' # Note: 'fs_to_ohe()' must be defined or available in the environment.
#'
#' # possible_patterns_ohe(c(2,3))
#'
#' @export
possible_patterns_ohe <- function(field_levels) {
  # For each field, build a vector of possible values:
  # c(0, 1, 2, ..., field_levels[field]).
  field_possible_values <- lapply(field_levels, function(x) {
    c(0, seq_len(x))
  })

  # Expand these lists into every possible combination across fields.
  # Each row in 'field_level_combos' is one combination of values,
  # something like (field1=0, field2=2, field3=0, ...).
  field_level_combos <- do.call(expand.grid, field_possible_values)

  # Convert each combination (row) into a one-hot vector by calling fs_to_ohe().
  # 'apply(..., 1, ...)' iterates row by row, returning a matrix, which we
  # then transpose before wrapping in data.frame.
  ohe_matrix <- t(apply(field_level_combos, 1, function(row_values) {
    fs_to_ohe(row_values, field_levels)
  }))

  # Wrap the transposed matrix in a data.frame for convenience.
  data.frame(ohe_matrix)
}
