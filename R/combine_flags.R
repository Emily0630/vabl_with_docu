#' Combine Flags
#'
#' Merges the \code{flags} objects across multiple hash outputs into a single
#' flattened list.
#'
#' @param hash_list A list of objects that each contain a \code{flags} element.
#'
#' @return A list created by flattening all \code{flags} from \code{hash_list}.
#'
#' @examples
#' \dontrun{
#' combined_flags <- combine_flags(list_of_hash_outputs)
#' }
#' @export
combine_flags <- function(hash_list) {
  hash_list %>%
    purrr::map(`[[`, "flags") %>%
    purrr::flatten()
}
