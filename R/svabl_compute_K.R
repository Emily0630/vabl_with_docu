#' Compute K from Minibatch
#'
#' K is the row sum of \eqn{(hash_count_list[[j]] / C[j])} over the sampled records,
#' scaled by \code{adjustment}.
#'
#' @param hash_count_list A list of length \code{n2}.
#' @param batch_indices A numeric vector of sampled record indices.
#' @param C_batch A numeric vector of length \code{B}, where \code{C_batch[i]} is
#'   \code{C} for record \code{batch_indices[i]}.
#' @param adjustment A scaling factor = \code{n2 / B}.
#'
#' @return A numeric vector of length \code{P} for partial usage in updates.
#'
#' @export
svabl_compute_K <- function(hash_count_list, batch_indices, C_batch, adjustment) {
  # for i in 1..B => hash_count_list[[batch[i]]] / C_batch[i]
  partial_mat <- sapply(seq_along(batch_indices), function(i){
    hash_count_list[[batch_indices[i]]] / C_batch[i]
  })
  if (!is.matrix(partial_mat)) {
    partial_mat <- matrix(partial_mat, nrow=length(hash_count_list[[1]]))
  }
  rowSums(partial_mat) * adjustment
}
