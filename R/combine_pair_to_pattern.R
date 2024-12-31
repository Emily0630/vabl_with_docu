#' Combine Pair-to-Pattern Mappings
#'
#' Flattens \code{pair_to_pattern} entries across multiple hash outputs.
#'
#' @param hash_list A list of objects, each containing a \code{pair_to_pattern}
#'   element (often a list of numeric or integer vectors).
#'
#' @return A list created by flattening all \code{pair_to_pattern} elements
#'   from \code{hash_list}.
#'
#' @examples
#' \dontrun{
#' combined_pair_to_pattern <- combine_pair_to_pattern(list_of_hash_outputs)
#' }
#' @export
combine_pair_to_pattern <- function(hash_list) {
  hash_list %>%
    purrr::map(`[[`, "pair_to_pattern") %>%
    purrr::flatten()
}
