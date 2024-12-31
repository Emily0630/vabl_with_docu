#' Compute M and U Parameters for fabl
#'
#' Given the current match/nonmatch counts (A and B) for each feature, plus
#' field markers, this helper function samples the M and U parameters by drawing
#' from Dirichlet distributions.
#'
#' @param matches_vec A numeric vector representing \eqn{A_Z} (the counts of matches
#'   for each column/feature).
#' @param nonmatches_vec A numeric vector representing \eqn{B_Z} (the counts of
#'   nonmatches for each column/feature).
#' @param m_prior A numeric scalar or vector used as the prior for M parameters.
#' @param u_prior A numeric scalar or vector used as the prior for U parameters.
#' @param field_marker An integer vector indicating which field each column
#'   belongs to. For example, if field 1 has 2 levels, field 2 has 3 levels,
#'   etc.
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item \code{m_vec}: The updated M parameters (one entry per column).
#'     \item \code{u_vec}: The updated U parameters (one entry per column).
#'   }
#' @examples
#' matches_vec <- c(5, 10, 2)
#' nonmatches_vec <- c(3, 1, 4)
#' field_marker <- c(1, 2, 2)
#' out <- compute_m_u(matches_vec, nonmatches_vec, m_prior = 1, u_prior = 1, field_marker)
#' @export
fabl_compute_m_u <- function(matches_vec, nonmatches_vec, m_prior, u_prior, field_marker) {
  # Add prior to match counts and nonmatch counts
  m_post <- m_prior + matches_vec
  u_post <- u_prior + nonmatches_vec

  # Split according to field_marker, so each field has its own sub-vector
  m_list <- split(m_post, field_marker)
  u_list <- split(u_post, field_marker)

  # For each field: draw Dirichlet, normalize
  m_vec <- as.vector(unlist(
    sapply(m_list, function(x) {
      prob <- MCMCpack::rdirichlet(1, x)
      prob / sum(prob)
    })
  ))
  u_vec <- as.vector(unlist(
    sapply(u_list, function(x) {
      prob <- MCMCpack::rdirichlet(1, x)
      prob / sum(prob)
    })
  ))

  list(m_vec = m_vec, u_vec = u_vec)
}
