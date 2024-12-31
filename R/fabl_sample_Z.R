#' Sample Z Values in FABL
#'
#' Given the current link states (\code{Z}) and overlap (\code{L_val}), this helper
#' function updates the link states for each record in \code{df2}. Specifically:
#' \enumerate{
#'   \item For each record \code{j}, if \code{Z[j] > 0} we decrement the overlap
#'         \code{L_val} since we are about to re-sample.
#'   \item We construct a probability vector of length \code{P+1}, corresponding
#'         to "no link" (pattern 0) or patterns \code{1..P}.
#'   \item We pick a pattern index \code{0..P}, then if that pattern index is
#'         nonzero, we also pick a specific record from \code{df1} by indexing
#'         \code{hash_to_file_1[[j]][[pattern]]}.
#'   \item We increment the overlap \code{L_val} if \code{Z[j] > 0}.
#' }
#'
#' @param Z A numeric vector of length \code{n2}, the current pattern indices
#'   (0..P) for each record in \code{df2}.
#' @param L_val A numeric value indicating the current overlap (number of matched pairs).
#' @param n1 The number of records in the first data frame.
#' @param n2 The number of records in the second data frame.
#' @param P An integer specifying the number of patterns (rows in the \code{ohe} matrix).
#' @param hash_weights A list of length \code{n2}, where
#'   \code{hash_weights[[j]]} is a numeric vector of length \code{P} giving
#'   the weights for each pattern \code{1..P} for record \code{j}.
#' @param pi_val A numeric value in \code{(0,1)} specifying the probability of linking.
#' @param hash_to_file_1 A list of length \code{n2}, where each element is
#'   itself a list of length \code{P} giving possible \code{df1} record IDs for
#'   each pattern.
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{\code{Z}}{An updated numeric vector of pattern indices (0..P).}
#'     \item{\code{L_val}}{An updated overlap value after re-sampling.}
#'     \item{\code{real_ids}}{A numeric vector of length \code{n2}, indicating
#'       which \code{df1} record each record \code{j} in \code{df2} is linked to.
#'       If \code{Z[j] = 0}, we set \code{real_ids[j] = n1+1} (i.e., "no link").}
#'   }
#'
#' @examples
#' \dontrun{
#' out <- fabl_sample_Z(Z, L_val, n1=10, n2=5, P=3,
#'                      hash_weights, pi_val=0.3, hash_to_file_1)
#' # out$Z, out$L_val, out$real_ids
#' }
#'
#' @export
fabl_sample_Z <- function(Z, L_val, n1, n2, P,
                          hash_weights, pi_val,
                          hash_to_file_1) {
  # Prepare vector of "df1" IDs that we will fill in (default = n1+1 => no link)
  real_ids <- rep(n1 + 1, n2)

  # For each record j in df2:
  for (j in seq_len(n2)) {

    # 1) Remove old link from overlap, if any
    if (Z[j] > 0) {
      L_val <- L_val - 1
    }

    # 2) Probability vector of length (P+1):
    #    index 1 => pattern 0 ("no link"), index 2..(P+1) => patterns 1..P
    prob_j <- c(
      1 - pi_val,  # prob of "no link"
      hash_weights[[j]] * (pi_val / n1)
    )

    # sample.int picks from 1..(P+1)
    # subtract 1 => 0..P
    new_pattern <- sample.int(P + 1, size = 1, prob = prob_j) - 1
    Z[j] <- new_pattern

    # 3) If new_pattern>0 => we have a match => pick one record from df1
    if (new_pattern > 0) {
      chosen_vec <- hash_to_file_1[[j]][[ new_pattern ]]
      idx <- ceiling(runif(1) * length(chosen_vec))
      real_id <- chosen_vec[idx]
      real_ids[j] <- real_id

      # update overlap
      L_val <- L_val + 1
    } else {
      # "no link"
      real_ids[j] <- n1 + 1
    }
  }

  list(Z = Z, L_val = L_val, real_ids = real_ids)
}
