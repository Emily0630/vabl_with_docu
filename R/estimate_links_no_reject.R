#' Labeling Logic When l_R = Inf
#'
#' If no reject option (\code{l_R = Inf}), we compute the threshold from Theorem 1
#' and label a record as matched only if \code{prob_best_match > threshold}.
#'
#' @param Z_hat Numeric vector of length \code{n2} (initial guesses).
#' @param n1,n2 Integers for data sets.
#' @param prob_no_link,prob_best_match Numeric vectors of length \code{n2}.
#' @param link_indicator Logical vector of length \code{n2}.
#' @param l_FNM,l_FM1,l_FM2 Numeric penalties used in threshold formula.
#' @param nonmatch_label String, either \code{"zero"} or \code{"n_1 + j"}.
#'
#' @return Modified \code{Z_hat}.
#'
#' @export
estimate_links_no_reject <- function(Z_hat, n1, n2,
                                     prob_no_link, prob_best_match, link_indicator,
                                     l_FNM, l_FM1, l_FM2,
                                     nonmatch_label) {
  # If we want nonmatch_label => "n_1 + j"
  # we label those j => n1+j
  if (nonmatch_label == "n_1 + j") {
    Z_hat <- seq(n1+1, n1+n2)
  } else {
    # default => 0
    Z_hat <- rep(0, n2)
  }

  # threshold => l_FM1/(l_FM1+l_FNM) +
  #              (l_FM2-l_FM1-l_FNM)*(1 - prob_no_link - prob_best_match)/(l_FM1+l_FNM)
  threshold <- (l_FM1 / (l_FM1 + l_FNM)) +
    (l_FM2 - l_FM1 - l_FNM) * (1 - prob_no_link - prob_best_match) / (l_FM1 + l_FNM)

  # best_match => from parent's environment
  best_match <- get("best_match", parent.frame())

  matchable <- link_indicator & (prob_best_match > threshold)
  Z_hat[matchable] <- best_match[matchable]
  Z_hat
}
