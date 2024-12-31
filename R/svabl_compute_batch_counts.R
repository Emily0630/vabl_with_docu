#' Compute Batch Counts
#'
#' Sums \code{hash_count_list} rows for a given batch of indices, then multiplies
#' by an \code{adjustment} factor to approximate full-data counts.
#'
#' @param hash_count_list A list of length \code{n2}, each a vector of length \code{P}.
#' @param batch_indices A numeric vector of sampled record indices.
#' @param adjustment A scaling factor = \code{n2 / B}.
#'
#' @return A numeric vector of length \code{P} representing approximate total counts.
#'
#' @export
svabl_compute_batch_counts <- function(hash_count_list, batch_indices, adjustment) {
  do.call(rbind, lapply(batch_indices, function(j) hash_count_list[[j]])) %>%
    colSums() * adjustment
}
