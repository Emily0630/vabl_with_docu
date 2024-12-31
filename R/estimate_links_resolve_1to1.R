#' Resolve Double Matches for One-to-One
#'
#' If \code{resolve=TRUE}, we enforce that each record in data set 1 is matched
#' to at most one record in data set 2. If multiple j's point to the same record,
#' we keep the one with the highest \code{prob_best_match}, and unmatch the others
#' (label them 0 if \code{l_R=Inf} or -1 if \code{l_R < Inf}).
#'
#' @param Z_hat A numeric vector of length \code{n2}.
#' @param l_R Numeric penalty for reject. If \code{l_R=Inf}, unmatch => 0, else => -1.
#' @param n1,n2 Integers.
#' @param prob_best_match A numeric vector of length \code{n2}.
#'
#' @return Modified \code{Z_hat}.
#'
#' @export
estimate_links_resolve_1to1 <- function(Z_hat, l_R, n1, n2, prob_best_match) {
  # Identify the record IDs that appear multiple times in Z_hat
  # but only if those record IDs > 0
  repeated_vals <- Z_hat[duplicated(Z_hat) & Z_hat > 0]

  if (length(repeated_vals) == 0) {
    return(Z_hat)
  }

  # For each repeated i
  for (i_val in unique(repeated_vals)) {
    # which j's => Z_hat==i_val
    j_candidates <- which(Z_hat == i_val)
    # pick the j with highest prob
    idx_max <- j_candidates[ which.max(prob_best_match[j_candidates]) ]
    # the rest we label as 0 (if l_R=Inf) or -1 (if l_R<Inf)
    non_winners <- setdiff(j_candidates, idx_max)
    if (is.infinite(l_R)) {
      Z_hat[non_winners] <- 0
    } else {
      Z_hat[non_winners] <- -1
    }
  }

  Z_hat
}
