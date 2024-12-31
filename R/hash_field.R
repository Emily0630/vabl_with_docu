#' Hash a Single Field
#'
#' Computes a hash vector for each level of a given field. The hash uses
#' \eqn{2^{(level\_index + offset)}} as its core. The offset is \eqn{0} when
#' \code{field_index = 1}, and is \code{cum_levels[field_index]} otherwise.
#'
#' @param num_levels An integer indicating how many levels this field has.
#' @param field_index An integer indicating which field (1-based) we are hashing.
#' @param cum_levels A numeric vector representing some form of cumulative
#'   offsets. For field \code{i}, \code{cum_levels[i]} is used if
#'   \code{field_index > 1}.
#'
#' @return A numeric vector of length \code{num_levels}, each element being:
#' \deqn{2^{\bigl(\text{level\_seq}[j] + \text{offset}\bigr)},}
#' where
#' \deqn{\text{offset} = \text{as.numeric}(field\_index > 1) \times
#'       \text{cum\_levels}[field\_index].}
#'
#' @examples
#' # Suppose we have 3 fields, with c(2, 3, 2) levels each. Then a
#' # "cumulative" approach could be:
#' # cum_levels = c(0, 2, 5, 7).
#' # For field_index = 1, offset=0; for field_index=2, offset=2; etc.
#' hash_field(3, 2, c(0, 2, 5, 7))
#'
#' @export
hash_field <- function(num_levels, field_index, cum_levels) {
  # Create a sequence from 1 to num_levels
  level_seq <- seq_len(num_levels)

  # as.numeric(level_seq > 0) is always 1 if level_seq starts at 1
  # The offset = as.numeric(field_index > 1) * cum_levels[field_index].
  #    - If field_index=1, offset = 0
  #    - If field_index>1, offset = cum_levels[field_index]
  # So each hash entry is 2^(level_seq + offset).
  # offset <- cum_levels[field_index]
  # 2 ^ (offset + level_seq)
  as.numeric(level_seq > 0) * 2^(
    level_seq + (as.numeric(field_index > 1) * cum_levels[field_index])
  )
}
