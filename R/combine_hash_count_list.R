#' Combine Hash Count Lists
#'
#' Merges the \code{hash_count_list} elements across multiple hash outputs into
#' a single list. Each \code{hash_count_list} is assumed to be a list of numeric
#' vectors.
#'
#' @param hash_list A list of objects from \code{hash_comparisons()}
#'   that each contain a \code{hash_count_list} element.
#'
#' @return A single list of numeric vectors, created by flattening all
#'   \code{hash_count_list} elements in \code{hash_list}.
#'
#' @examples
#' \dontrun{
#' combined_hash_counts <- combine_hash_count_list(list_of_hash_outputs)
#' }
#' @export
combine_hash_count_list <- function(hash_list) {
  hash_list %>%
    purrr::map(`[[`, "hash_count_list") %>%
    purrr::flatten()
}
