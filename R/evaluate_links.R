#' Evaluate Linkage Performance
#'
#' This function compares an estimated linkage structure \code{Z_hat} to a
#' known ground-truth linkage structure \code{Z_true}. It computes standard
#' metrics for bipartite matching, including recall, precision, and the
#' F-measure.
#'
#' @param estimated_links An integer vector of length \code{n2} representing
#'   the estimated linkage for each record in the second data set. Records
#'   linked to a record \code{i \le n1} in the first data set means
#'   \code{estimated_links[j] = i}, and \code{estimated_links[j] = 0} indicates
#'   a non-match (or no link).
#' @param true_links An integer vector of the same length as \code{estimated_links},
#'   representing the ground-truth linkage. The format is the same as \code{estimated_links}.
#' @param size_file1 A positive integer indicating the number of records in the
#'   first data set (\code{n1}). Typically, \code{n1 \ge n2}.
#'
#' @return A named numeric vector of length 3, containing:
#'   \describe{
#'     \item{\code{Recall}}{The fraction of actual matches (in \code{true_links})
#'       that were correctly identified in \code{estimated_links}.}
#'     \item{\code{Precision}}{The fraction of predicted matches (in \code{estimated_links})
#'       that are truly correct according to \code{true_links}.}
#'     \item{\code{Fmeasure}}{The harmonic mean of recall and precision.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Suppose we have an estimated linkage from a bipartite matching approach:
#' est_links <- c(1, 0, 2, 2)
#' # And the true linkage is:
#' true_lnk <- c(1, 0, 2, 3)
#' # size_file1 = 3
#' evaluate_links_original(est_links, true_lnk, 3)
#' }
#'
#' @export
evaluate_links <- function(estimated_links, true_links, size_file1){
  # Calculate how many predicted links (i) are within the range 1..size_file1
  # and also greater than 0 i.e., a predicted match
  num_predicted_links <- sum(estimated_links <= size_file1 & estimated_links > 0)

  # Calculate how many true matches are within 1..size_file1 and > 0 i.e., actual matches
  num_true_matches <- sum(true_links <= size_file1 & true_links > 0)

  # Calculate how many predicted links are exactly correct
  # for those positions j where estimated_links[j] is 1..size_file1
  # we check if estimated_links[j] == true_links[j].
  num_correct_links <- sum(
    estimated_links[estimated_links <= size_file1 & estimated_links > 0] ==
      true_links[estimated_links <= size_file1 & estimated_links > 0]
  )

  # Recall = correct matches / total actual matches
  recall <- num_correct_links / num_true_matches

  # Precision = correct matches / total predicted matches
  precision <- num_correct_links / num_predicted_links

  # F-measure = harmonic mean of recall & precision
  fmeasure <- 2 * (recall * precision) / (recall + precision)

  # Package them into a named vector
  eval_metrics <- c(recall, precision, fmeasure)
  names(eval_metrics) <- c("Recall", "Precision", "Fmeasure")

  return(eval_metrics)
}
