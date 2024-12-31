#' Hash a Single Field
#'
#' Computes a hash vector for each level of a given field. The hash uses
#' \eqn{2^{(level\_index + offset)}} as its core. The offset is \eqn{0} when
#' \code{field_index = 1}, and is \code{cum_levels[field_index]} otherwise.
#'
#' This version preserves the original approach, including:
#' \itemize{
#'   \item \code{as.numeric(level_seq > 0)}, which is always \code{1} for a
#'     \code{level_seq} starting at \code{1}, but is kept for completeness.
#'   \item The offset \eqn{(\text{as.numeric}(field\_index > 1) \times
#'     cum\_levels[field\_index])}.
#' }
#'
#' @param num_levels Integer. The number of levels in the field.
#' @param field_index Integer. The index of the field (1-based).
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
  as.numeric(level_seq > 0) * 2^(
    level_seq + (as.numeric(field_index > 1) * cum_levels[field_index])
  )
}


#' Hash a Single Field
#'
#' Computes a vector of hash offsets (powers of two) for each level in a field,
#' potentially offset by the cumulative levels from prior fields. The returned
#' numeric vector is often used in record linkage or hashing algorithms.
#'
#' @param num_levels An integer indicating how many levels this field has.
#' @param field_index An integer indicating which field (1-based) we are hashing.
#' @param cum_levels A numeric vector (or integer vector) representing the
#'   cumulative sum of levels across fields. Its \code{field_index}-th element
#'   gives the offset to apply for this field.
#'
#' @return A numeric vector of length \code{num_levels}. Each entry is
#'   \eqn{2^{(\text{offset} + i)}}, where \eqn{i} is the level index
#'   \eqn{1, 2, \dots, num_levels}, and \eqn{\text{offset} = cum_levels[field_index]}.
#'
#' @examples
#' # Suppose we have 3 fields with c(2,3,2) levels each. Then a cumulative sum might be:
#' # cum_levels = c(0, 2, 5, 7)
#' # field_index = 2 => offset = 2 (i.e., the sum of the 1st field's 2 levels)
#' # num_levels = 3 for the second field
#' hash_field(num_levels = 3, field_index = 2, cum_levels = c(0, 2, 5, 7))
#'
#' @export
hash_field <- function(num_levels, field_index, cum_levels) {
  # 1) Determine the offset for this field based on cum_levels.
  #    cum_levels is typically something like c(0, L1, L1+L2, L1+L2+L3, ...),
  #    so cum_levels[field_index] gives the sum of levels for all prior fields.
  offset <- cum_levels[field_index]

  # 2) Create a sequence of levels (1..num_levels).
  level_seq <- seq_len(num_levels)

  # 3) For each level, compute 2^(offset + level_index).
  #    This yields a distinct power-of-two "hash" for each level of each field.
  2 ^ (offset + level_seq)
}

