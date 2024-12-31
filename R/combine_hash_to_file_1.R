#' Combine Hash-to-File Mappings
#'
#' Merges the \code{hash_to_file_1} objects across multiple hash outputs
#' into a single list (via flattening).
#'
#' @param hash_list A list of objects, each containing a \code{hash_to_file_1}
#'   element (often a nested list structure).
#'
#' @return A list created by flattening all \code{hash_to_file_1} elements from
#'   \code{hash_list}.
#'
#' @examples
#' \dontrun{
#' combined_hash_to_file <- combine_hash_to_file_1(list_of_hash_outputs)
#' }
#' @export
combine_hash_to_file_1 <- function(hash_list) {
  hash_list %>%
    purrr::map(`[[`, "hash_to_file_1") %>%
    purrr::flatten()
}
