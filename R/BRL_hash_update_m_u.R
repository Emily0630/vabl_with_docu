#' Update M and U Parameters for BRL hash
#'
#' Given the matched counts for each pattern, this helper function updates
#' the \eqn{m} and \eqn{u} parameters by sampling from Dirichlet distributions.
#'
#' @param unique_patterns A \code{matrix} of size \eqn{P \times L},
#'   where \eqn{P} is the number of unique patterns and \eqn{L} is the
#'   total number of one-hot columns (sum of levels).
#' @param pattern_counts A numeric vector of length \eqn{P} giving how many times
#'   each pattern appears.
#' @param matches A numeric vector of length \eqn{P} indicating how many times
#'   each pattern is matched.
#' @param field_marker An integer vector of length \eqn{L} indicating which field
#'   each column of \code{unique_patterns} belongs to.
#' @param m_prior,u_prior Numeric scalars or vectors representing the prior
#'   hyperparameters for \eqn{m} and \eqn{u}.
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item \code{m}: A numeric vector of length \eqn{L} with updated m parameters.
#'     \item \code{u}: A numeric vector of length \eqn{L} with updated u parameters.
#'   }
#'
#' @examples
#' \dontrun{
#' res <- update_m_u(unique_patterns, pattern_counts, matches,
#'                   field_marker, m_prior = 1, u_prior = 1)
#' }
#' @export
update_m_u <- function(unique_patterns, pattern_counts, matches,
                       field_marker, m_prior = 1, u_prior = 1) {
  # 1) Compute the matched array (AZ): sum of unique_patterns * matches
  # 2) Compute the unmatched array (BZ): sum of unique_patterns * (pattern_counts - matches)
  # 3) For each field, sample from a Dirichlet and normalize.

  # Matched portion for each column
  matched_array <- sweep(unique_patterns, 1, matches, `*`) %>%
    colSums() %>%
    unname()

  # Unmatched portion for each column
  unmatched_array <- sweep(unique_patterns, 1, (pattern_counts - matches), `*`) %>%
    colSums() %>%
    unname()

  # Posteriors
  m_post <- m_prior + matched_array
  u_post <- u_prior + unmatched_array

  # Split by field, sample Dirichlet, then combine
  m_post_list <- split(m_post, field_marker)
  sampled_m <- as.vector(unlist(lapply(m_post_list, function(x) {
    # Each field's columns are a Dirichlet draw
    prob <- MCMCpack::rdirichlet(1, x)
    prob / sum(prob)
  })))

  u_post_list <- split(u_post, field_marker)
  sampled_u <- as.vector(unlist(lapply(u_post_list, function(x) {
    prob <- MCMCpack::rdirichlet(1, x)
    prob / sum(prob)
  })))

  list(m = sampled_m, u = sampled_u)
}
