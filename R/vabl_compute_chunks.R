#' Compute Digamma Chunks for a, b
#'
#' Computes the field-wise sum of \code{a} and \code{b}, applies \code{digamma},
#' and returns the \code{a_chunk} and \code{b_chunk} vectors used later.
#'
#' @param a A numeric vector of length \code{L}.
#' @param b A numeric vector of length \code{L}.
#' @param field_marker An integer vector of length \code{L}, labeling each column
#'   (feature) to a field.
#'
#' @return A list with \code{a_chunk} and \code{b_chunk} numeric vectors of length \code{L}.
#'
#' @export
vabl_compute_chunks <- function(a, b, field_marker) {
  a_sum <- a %>%
    split(., field_marker) %>%
    sapply(sum) %>%
    digamma() %>%
    .[field_marker]
  a_chunk <- digamma(a) - a_sum

  b_sum <- b %>%
    split(., field_marker) %>%
    sapply(sum) %>%
    digamma() %>%
    .[field_marker]
  b_chunk <- digamma(b) - b_sum

  list(a_chunk = a_chunk, b_chunk = b_chunk)
}
