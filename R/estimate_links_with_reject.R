#' Labeling Logic When l_R < Inf (Reject Option)
#'
#' We label everything as -1 (reject) by default, compute a threshold including
#' the reject penalty, accept a match only if \code{prob_best_match > threshold}
#' and label non-match if \code{prob_no_link > 1 - l_R / l_FNM}.
#'
#' @param Z_hat,n1,n2,prob_no_link,prob_best_match,link_indicator As above.
#' @param l_FNM,l_FM1,l_FM2,l_R Numeric penalties. \code{l_R < Inf}.
#' @param nonmatch_label "zero" or "n_1 + j".
#'
#' @return Modified \code{Z_hat}.
#'
#' @export
estimate_links_with_reject <- function(Z_hat, n1, n2,
                                       prob_no_link, prob_best_match, link_indicator,
                                       l_FNM, l_FM1, l_FM2, l_R,
                                       nonmatch_label) {
  # 1) Start all as -1 => reject
  Z_hat[] <- -1

  # 2) threshold => 1 - l_R/l_FM1 + (l_FM2-l_FM1)*(1 - prob_no_link - prob_best_match)/l_FM1
  threshold <- 1 - (l_R / l_FM1) +
    (l_FM2 - l_FM1) * (1 - prob_no_link - prob_best_match) / l_FM1

  best_match <- get("best_match", parent.frame())

  # Accept a match if link_indicator & prob_best_match > threshold
  match_condition <- link_indicator & (prob_best_match > threshold)
  Z_hat[match_condition] <- best_match[match_condition]

  # If prob_no_link > 1 - (l_R/l_FNM), label as non-match => 0 or n1+j
  nonlink_condition <- (prob_no_link > (1 - l_R / l_FNM))

  if (nonmatch_label == "n_1 + j") {
    # j => n1+j
    Z_hat[nonlink_condition] <- seq(n1+1, n1+n2)[nonlink_condition]
  } else {
    Z_hat[nonlink_condition] <- 0
  }

  Z_hat
}
