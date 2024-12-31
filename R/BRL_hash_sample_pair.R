#' Sample a Single Record's Link for BRL hash
#'
#' For a given record \eqn{j}, sample whether it links to an existing record
#' (1..n1) or remains unlinked (0). Supports multiple modes:
#' \itemize{
#'   \item \code{"base"}: direct sampling from \code{pair_to_pattern}.
#'   \item \code{"efficient"}: uses \code{hash_count_list} but subtracts matches from them.
#'   \item \code{"rejection"}: attempts to sample within \code{reject_iter} tries,
#'         otherwise falls back to \code{"base"}-like sampling.
#' }
#'
#' @param j Integer index of the record in df2 to be sampled.
#' @param Z An integer vector of length \eqn{n2} indicating current links for each record in df2.
#' @param Z_inv An integer vector of length \eqn{n1} indicating whether the record in df1 is taken or not.
#' @param L Current number of matches.
#' @param empty_weight Numeric value for the unlinked option: \eqn{(n1 - L)*(n2-L-1 + beta)/(L + alpha)}.
#' @param unique_weights A numeric vector of length \eqn{P} giving the pattern weights from \code{compute_unique_weights()}.
#' @param pair_to_pattern A list of length \eqn{n2}, where \code{pair_to_pattern[[j]]}
#'   is a vector of pattern indices that record \eqn{j} can have for each df1 match option.
#' @param hash_count_list A list of length \eqn{n2}, each entry is a numeric vector of length \eqn{P}.
#' @param hash_to_file_1 A nested structure \code{[[j]][[pattern]]}, containing the
#'   subset of df1 record indices that produce that pattern with df2 record \eqn{j}.
#' @param candidates_P An integer vector \code{0:P}, used when sampling patterns.
#' @param mode A string: \code{"base"}, \code{"efficient"}, or \code{"rejection"}.
#' @param reject_iter Integer, the maximum attempts before fallback in \code{"rejection"} mode.
#'
#' @return A list with elements:
#'   \itemize{
#'     \item \code{Z_j}: The chosen df1 record ID (or 0 if unlinked).
#'     \item \code{Z_pat_j}: The pattern index chosen (or 0 if unlinked).
#'     \item \code{L_new}: The updated number of matches (L+1 if Z_j>0 else L).
#'   }
#'
#' @examples
#' \dontrun{
#' result <- sample_pair(
#'   j, Z, Z_inv, L, empty_weight, unique_weights,
#'   pair_to_pattern, hash_count_list, hash_to_file_1,
#'   candidates_P, mode="rejection", reject_iter=5
#' )
#' }
#' @export
sample_pair <- function(j, Z, Z_inv, L,
                        empty_weight,
                        unique_weights,
                        pair_to_pattern,
                        hash_count_list,
                        hash_to_file_1,
                        candidates_P,
                        mode,
                        reject_iter = 1) {
  # If record j was previously linked, remove that link
  if (Z[j] > 0) {
    L <- L - 1
    Z_inv[Z[j]] <- 0
  }
  # Reset to no link
  Z_j      <- 0
  Z_pat_j  <- 0

  if (mode == "base") {
    # Simple approach:
    available <- which(Z_inv == 0) # df1 records that are not matched
    temp_weights <- c(empty_weight,
                      unique_weights[pair_to_pattern[[j]][available]])

    # Sample either 0 or one from 'available'
    chosen <- sample(c(0, available), 1, prob=temp_weights)
    Z_j     <- chosen
    Z_pat_j <- if (chosen > 0) pair_to_pattern[[j]][chosen] else 0

  } else if (mode == "efficient") {
    # Adjust hash_count_list[[j]] by subtracting patterns used by current matches
    n_current <- hash_count_list[[j]]

    # Subtract patterns for all currently matched records
    for (k in seq_along(Z)) {
      if (Z[k] > 0) {
        ind <- pair_to_pattern[[k]][Z[k]]
        n_current[ind] <- n_current[ind] - 1
      }
    }
    temp_weights <- n_current * unique_weights
    probs <- c(empty_weight, temp_weights)

    # Sample a pattern from 0..P
    pat <- sample(candidates_P, 1, prob=probs)
    if (pat == 0) {
      Z_j     <- 0
      Z_pat_j <- 0
    } else {
      # We then randomly pick a df1 record among those that produce 'pat'
      # for record j
      npj   <- n_current[pat]
      flag2 <- 1
      while (flag2 == 1) {
        index <- ceiling(runif(1) * npj)
        i     <- hash_to_file_1[[j]][[pat]][index]
        if (Z_inv[i] == 0) {
          Z_j     <- i
          Z_pat_j <- pat
          flag2   <- 0
        }
      }
    }

  } else if (mode == "rejection") {
    # Repeatedly try picking a pattern from 0..P
    hash_weights <- hash_count_list[[j]] * unique_weights
    probs        <- c(empty_weight, hash_weights)
    flag         <- 1
    iter         <- 0
    while (flag == 1) {
      pat <- sample(candidates_P, 1, prob=probs)
      if (pat == 0) {
        Z_j     <- 0
        Z_pat_j <- 0
        flag <- 0
      } else {
        idx   <- ceiling(runif(1) * hash_count_list[[j]][pat])
        i     <- hash_to_file_1[[j]][[pat]][idx]
        if (Z_inv[i] == 0) {
          Z_j     <- i
          Z_pat_j <- pat
          flag    <- 0
        }
      }
      iter <- iter + 1
      # If we fail after reject_iter attempts, fallback to simpler approach
      if (iter == reject_iter && flag == 1) {
        available <- which(Z_inv == 0)
        temp_weights <- c(empty_weight,
                          unique_weights[pair_to_pattern[[j]][available]])
        chosen <- sample(c(0, available), 1, prob=temp_weights)
        Z_j     <- chosen
        Z_pat_j <- if (chosen > 0) pair_to_pattern[[j]][chosen] else 0
        flag    <- 0
      }
    }
  }

  # If we found a positive match, update L and Z_inv
  if (Z_j > 0) {
    L <- L + 1
    Z_inv[Z_j] <- 1
  }

  list(Z_j = Z_j, Z_pat_j = Z_pat_j, L_new = L)
}
