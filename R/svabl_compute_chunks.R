#' Compute Digamma Chunks for SVABL
#'
#' Computes \code{a_chunk} and \code{b_chunk} as
#' \deqn{
#'   a\_chunk = \digamma(a) - \digamma( \sum_{i \in \text{field}} a[i] )
#' }
#'
#' @param a A numeric vector (Dirichlet parameter).
#' @param b A numeric vector (Dirichlet parameter).
#' @param field_marker An integer vector labeling columns to fields.
#'
#' @return A list with \code{a_chunk} and \code{b_chunk}, both length \code{length(a)}.
#'
#' @export
svabl_compute_chunks <- function(a, b, field_marker) {
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
