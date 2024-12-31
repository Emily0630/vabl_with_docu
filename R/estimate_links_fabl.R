#' Process FABL Output for Link Estimation
#'
#' Extracts posterior samples of \code{Z} from a FABL run, computes
#' the probability of no link, the best match, and returns intermediate
#' info for thresholding.
#'
#' @param out The FABL output, assumed to have an element \code{Z} of size
#'   \code{(n2 x num_samples)}. \code{n2} is the number of records in data set 2,
#'   and \code{num_samples} is the number of posterior draws.
#' @param n1 An integer, number of records in the first data set.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{best_match}}{A numeric vector of length \code{n2}, with the
#'     most likely matched record for each row.}
#'   \item{\code{prob_best_match}}{A numeric vector of length \code{n2} with the
#'     probability of that best match.}
#'   \item{\code{prob_no_link}}{A numeric vector of length \code{n2} giving the
#'     probability that the record has no link.}
#'   \item{\code{link_indicator}}{A logical vector indicating whether
#'     \code{best_match < n1+1}.}
#'   \item{\code{n2}}{The number of rows in \code{Z_samps}.}
#'   \item{\code{Z_hat}}{An initial numeric vector of length \code{n2} (e.g., 0)
#'     that can be updated later.}
#' }
#'
#' @export
estimate_links_fabl <- function(out, n1) {
  Z_samps <- out$Z
  n2 <- nrow(Z_samps)

  # Ensure no references exceed n1+1
  # e.g. if some sample says "6" when n1=5 => clamp it to 6 => no link
  Z_samps[Z_samps > n1+1] <- n1+1

  num_samples <- ncol(Z_samps)

  # For each row j, build a frequency table of observed Z-values
  # Then compute distribution (table / num_samples)
  # We'll store these in a list
  probs_list <- apply(Z_samps, 1, function(z_row) {
    # If z_row is numeric, table(...) yields named counts e.g. c("1"=..., "5"=...)
    # We then divide by num_samples
    if (length(z_row) == 0) {
      return(numeric(0))  # no samples
    }
    dist <- table(z_row) / num_samples
    dist
  })

  # Probability of no link
  # We interpret "n1+1" as the no-link label
  prob_no_link <- sapply(probs_list, function(dist) {
    # find distribution mass for name == n1+1
    # if dist is empty or no name matches, sum(...)=0
    val <- dist[as.character(n1+1)]
    if (is.na(val)) val <- 0
    val
  })

  # best_match => label with max distribution
  best_match <- sapply(probs_list, function(dist) {
    if (length(dist) == 0) {
      return(0)  # if no data => best match=0 or something
    }
    idx <- which.max(dist)
    nm  <- names(dist)[idx]  # character string
    # parse it as numeric
    val <- suppressWarnings(as.numeric(nm))
    if (is.na(val)) {
      # If the name is not numeric, define a fallback (0 means no match)
      val <- 0
    }
    val
  })

  # Probability of that best match
  prob_best_match <- sapply(probs_list, function(dist){
    if (length(dist) == 0) {
      return(0)
    }
    max(dist)
  })

  # link_indicator => best_match < n1+1
  link_indicator <- (best_match < (n1+1)) & (best_match > 0)

  # Start with a default Z_hat of 0
  Z_hat <- rep(0, n2)

  list(
    best_match = best_match,
    prob_best_match= prob_best_match,
    prob_no_link = prob_no_link,
    link_indicator = link_indicator,
    n2 = n2,
    Z_hat = Z_hat
  )
}
