#' Compute String Distance
#'
#' Computes pairwise string distances between two character vectors. Currently
#' supports the \code{"Levenshtein"} or \code{"Damerau-Levenshtein"} metrics.
#'
#' The returned values are actually \code{1 - similarity} when the metric is
#' Levenshtein-based. For example, if \code{distance_metric = "Levenshtein"},
#' then the distance is calculated as \code{1 - levenshteinSim(...)}, so higher
#' values mean more dissimilar strings.
#'
#' @param vec1 A character vector.
#' @param vec2 A character vector of the same length as \code{vec1}.
#' @param distance_metric A string specifying the metric to use, either
#'   \code{"Levenshtein"} or \code{"Damerau-Levenshtein"}.
#'
#' @return A numeric vector of length \code{length(vec1)} representing the
#'   pairwise distances for each index.
#'
#' @examples
#' v1 <- c("hello", "world")
#' v2 <- c("helo",  "word")
#'
#' # Using Levenshtein distance
#' compute_string_distance(v1, v2, "Levenshtein")
#'
#' # Using Damerau-Levenshtein distance
#' # (requires the `levitate` package)
#' \dontrun{
#' compute_string_distance(v1, v2, "Damerau-Levenshtein")
#' }
#'
#' @export
compute_string_distance <- function(vec1, vec2, distance_metric) {
  if (distance_metric == "Levenshtein") {
    # 1 - sim => the disagreement measure
    return(1 - RecordLinkage::levenshteinSim(as.character(vec1),
                                             as.character(vec2)))
  } else if (distance_metric == "Damerau-Levenshtein") {
    # 1 - ratio from 'levitate::lev_ratio'
    # NB: Transpositions count as 1 instead of 2 in Damerau-Levenshtein
    return(1 - levitate::lev_ratio(as.character(vec1),
                                   as.character(vec2),
                                   useNames = FALSE))
  }
  stop("Unsupported string distance metric. Use 'Levenshtein' or 'Damerau-Levenshtein'.")
}
