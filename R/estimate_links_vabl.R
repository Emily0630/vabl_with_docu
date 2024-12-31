#' Process VABL Output for Link Estimation
#'
#' Extracts \code{pattern_weights}, \code{C}, \code{b_pi}, etc. from VABL output,
#' uses \code{hash$flags} to form candidate records, and computes the best match
#' for each record in data set 2.
#'
#' @param out The VABL output, assumed to have \code{pattern_weights}, \code{C},
#'   \code{b_pi}, etc.
#' @param hash The associated hash list, containing \code{flags}, \code{n2}, etc.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{best_match}}{Integer vector of length \code{n2} for the
#'     highest-probable match (with an added 0 for no-link).}
#'   \item{\code{prob_best_match}}{Numeric vector of length \code{n2} for
#'     probability of that best match.}
#'   \item{\code{prob_no_link}}{Probability of no link.}
#'   \item{\code{link_indicator}}{Logical vector.}
#'   \item{\code{n2}}{Number of records in data set 2.}
#'   \item{\code{Z_hat}}{An initial labeling vector (all 0).}
#' }
#'
#' @export
estimate_links_vabl <- function(out, hash) {
  n2 <- hash$n2

  # pattern_weights => length P
  # out$C => length n2
  # out$b_pi => scalar
  # We also need hash$flags => for each j, a list => (eligible_records, eligible_patterns)

  # For each j => pattern_probs[j,p] = pattern_weights[p] / out$C[j]
  pattern_probs <- lapply(seq_len(n2), function(j) {
    out$pattern_weights / out$C[j]
  })

  # Build a data.frame for each j => columns: record, prob
  # record => c(eligible_records, 0)
  # prob   => c(pattern_probs[j, eligible_patterns], single/no-link prob)
  # single => exp(digamma(out$b_pi)) / out$C[j]
  single_val <- exp(digamma(out$b_pi))

  possible_records_list <- lapply(seq_len(n2), function(j){
    el_recs <- hash$flags[[j]]$eligible_records
    el_pats <- hash$flags[[j]]$eligible_patterns

    # If they're different length, we throw error or handle gracefully
    if (length(el_recs) != length(el_pats)) {
      stop(paste0("Mismatch in VABL code: #eligible_records (", length(el_recs),
                  ") != #eligible_patterns (", length(el_pats), ") for j=", j))
    }

    # record => c(..., 0)
    rec_ids <- c(el_recs, 0)

    # prob => c(pattern_probs for the patterns, single_val / out$C[j])
    pat_probs <- pattern_probs[[j]][ el_pats ]
    no_link_prob <- single_val / out$C[j]
    rec_probs <- c(pat_probs, no_link_prob) %>% unname()

    # Now we can safely build data.frame
    data.frame(
      record = rec_ids,
      prob   = rec_probs
    )
  })

  # For each j => pick row with max prob
  best_info <- lapply(possible_records_list, function(df_j){
    df_j[ which.max(df_j$prob), ]
  }) %>% do.call(rbind, .)

  # best_match => record from best row
  best_match <- best_info$record
  prob_best_match <- best_info$prob

  # Probability of no link => b_pi / C[j]
  prob_no_link <- out$b_pi / out$C
  link_indicator <- (best_match > 0)

  # Initialize Z_hat => 0
  Z_hat <- rep(0, n2)

  list(
    best_match      = best_match,
    prob_best_match = prob_best_match,
    prob_no_link    = prob_no_link,
    link_indicator  = link_indicator,
    n2             = n2,
    Z_hat          = Z_hat
  )
}
