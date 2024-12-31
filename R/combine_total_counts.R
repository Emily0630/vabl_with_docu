#' Combine Total Counts Across Multiple Hash Outputs
#'
#' Given a list of hash outputs, each containing \code{total_counts}, this
#' function merges them by summing across rows and then computing column sums.
#'
#' @param hash_list A list of objects (typically output from \code{hash_comparisons()}),
#'   each of which has a \code{total_counts} element (a numeric vector).
#'
#' @return A numeric vector representing the combined (summed) total counts
#'   across all hash outputs in \code{hash_list}.
#'
#' @examples
#' \dontrun{
#' combined_counts <- combine_total_counts(list_of_hash_outputs)
#' }
#' @export
combine_total_counts <- function(hash_list) {
  # 1) Extract 'total_counts' from each hash output
  # 2) Row-bind them into a matrix
  # 3) Sum each column
  purrr::map(hash_list, ~.x$total_counts) %>%
    do.call(rbind, .) %>%
    colSums()
}
