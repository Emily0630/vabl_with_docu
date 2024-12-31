#' Estimate Links for VABL/SVABL
#'
#' This function computes the most likely match (or no-link) for each record in
#' \code{df2}, given the output of a variational or stochastic variational algorithm
#' (stored in \code{out}) and a \code{hash} object containing record linkage information.
#'
#' @param out A list containing posterior estimates and other parameters from VABL/SVABL.
#'   Must include at least:
#'   \itemize{
#'     \item \code{pattern_weights}: Vector of pattern weights.
#'     \item \code{C}: Vector of denominators for each record in \code{df2}.
#'     \item \code{b_pi}: Scalar or vector controlling the no-link term.
#'   }
#' @param hash A list containing linkage metadata, which must include:
#'   \itemize{
#'     \item \code{n2}: Number of records in \code{df2}.
#'     \item \code{flags}: A list of length \code{n2}, where each element \code{flags[[j]]}
#'       contains:
#'       \itemize{
#'         \item \code{eligible_records}: The set of record indices in \code{df1} that
#'           record \code{j} in \code{df2} could possibly match.
#'         \item \code{eligible_patterns}: The vector of pattern indices for those
#'           eligible records.
#'       }
#'   }
#'
#' @return A named list with the following elements:
#'   \itemize{
#'     \item \code{best_match}: An integer vector of length \code{n2}, indicating
#'       the most likely match for each record in \code{df2} (or \code{0} if no-link).
#'     \item \code{prob_best_match}: A numeric vector of length \code{n2}, giving
#'       the probability of the chosen \code{best_match}.
#'     \item \code{prob_no_link}: A numeric vector of length \code{n2}, giving
#'       the probability that each record in \code{df2} is unlinked.
#'     \item \code{link_indicator}: A logical vector of length \code{n2} indicating
#'       \code{TRUE} if a match was found (\code{best_match} > 0) and \code{FALSE}
#'       otherwise.
#'     \item \code{n2}: Number of records in \code{df2}.
#'     \item \code{Z_hat}: A vector of length \code{n2} (identical to \code{best_match}
#'       by default) but can be used differently if desired.
#'   }
#'
#' @examples
#' \dontrun{
#' # Suppose 'out' is a list returned by a VABL or SVABL procedure, with
#' # elements 'pattern_weights', 'C', and 'b_pi'.
#'
#' # 'hash' contains metadata about record flags, including 'eligible_records'
#' # and 'eligible_patterns' for each of the n2 records in df2.
#'
#' # Then we call:
#' vabl_info <- estimate_links_vabl_svabl(out = out, hash = hash)
#'
#' # Extract results:
#' best_match <- vabl_info$best_match
#' prob_best_match <- vabl_info$prob_best_match
#' prob_no_link <- vabl_info$prob_no_link
#' link_indicator <- vabl_info$link_indicator
#' n2 <- vabl_info$n2
#' Z_hat <- vabl_info$Z_hat
#' }
#'
#' @export
estimate_links_vabl_svabl <- function(out, hash) {
  n2 <- hash$n2

  # Compute probabilities for each pattern
  pattern_probs <- lapply(seq_len(n2), function(j) {
    out$pattern_weights / out$C[j]
  })

  # Determine possible records for each j, including the "no-link" scenario (0)
  possible_records <- lapply(seq_len(n2), function(j) {
    record <- c(hash$flags[[j]]$eligible_records, 0)
    prob <- c(
      pattern_probs[[j]][hash$flags[[j]]$eligible_patterns],
      exp(digamma(out$b_pi)) / out$C[j]  # Probability for no-link
    )

    data.frame(
      record = record,
      prob   = unname(prob)
    )
  })

  # Select record (or no-link) with highest probability
  max_prob <- lapply(possible_records, function(x) {
    x[which.max(x$prob), ]
  }) %>%
    do.call(rbind, .)

  best_match <- max_prob$record
  prob_best_match <- max_prob$prob
  prob_no_link <- out$b_pi / out$C
  link_indicator <- best_match > 0

  # Define Z_hat as best_match (can be replaced with other logic if needed)
  Z_hat <- best_match

  list(
    best_match = best_match,
    prob_best_match = prob_best_match,
    prob_no_link = prob_no_link,
    link_indicator = link_indicator,
    n2 = n2,
    Z_hat = Z_hat
  )
}
