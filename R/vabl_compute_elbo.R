#' Compute ELBO Pieces
#'
#' Replicates the six pieces of the ELBO calculation from the original code.
#'
#' @param n2 The number of records in the second data set.
#' @param hash_count_list List of length \code{n2}, each numeric vector of length \code{P}.
#' @param phi,weights,m_p,u_p,C,single, etc. Numeric vectors used in the formula.
#' @param n1 Scalar, number of records in first data set.
#' @param a_pi,b_pi Scalars for Beta distribution parameters.
#' @param alpha_pi,beta_pi The original prior for \code{a_pi,b_pi}.
#' @param a,b,alpha,Beta Numeric vectors, updated and prior Dirichlet parameters.
#' @param a_chunk,b_chunk Numeric vectors from \code{digamma(a)-...}, etc.
#' @param field_marker An integer vector labeling columns to fields.
#'
#' @return A numeric scalar, the sum of 6 pieces (the ELBO).
#'
#' @export
vabl_compute_elbo <- function(
    n2, hash_count_list, phi, weights, m_p, u_p, C, single, n1,
    a_pi, b_pi, alpha_pi, beta_pi,
    a, b, alpha, Beta,
    a_chunk, b_chunk,
    field_marker
) {
  # 1) part
  elbo_part1 <- sapply(1:n2, function(j){
    sum(hash_count_list[[j]] *
          ( phi*(weights - log(phi) + log(C[j]))/ C[j] + u_p ))
  }) %>%
    sum()

  # 2) part
  total_nonmatch <- sum(single / C)
  elbo_part2 <- single * sum((1/C) * log(C)) +
    total_nonmatch * (log(n1) - log(single)) - log(n1)*n2

  # 3) part
  elbo_part3 <- lbeta(a_pi, b_pi) - lbeta(alpha_pi, beta_pi)

  # 4) part
  # sum( split(a, field_marker), sum( lgamma(...) ) - lgamma(sum(...)) )
  posterior_ab <- sapply(list(a, b), function(y){
    split(y, field_marker) %>%
      sapply(., function(x){
        sum(lgamma(x)) - lgamma(sum(x))
      }) %>%
      sum()
  }) %>%
    sum()

  # 5) part
  # Negative of prior piece
  prior_ab <- sapply(list(alpha, Beta), function(y){
    split(y, field_marker) %>%
      sapply(function(x){
        sum(lgamma(x)) - lgamma(sum(x))
      }) %>%
      sum()
  }) %>%
    sum()
  elbo_part5 <- - prior_ab

  # 6) part
  elbo_part6 <- sum((alpha - a) * a_chunk + (Beta - b) * b_chunk)

  # sum all
  sum(c(elbo_part1, elbo_part2, elbo_part3, posterior_ab, elbo_part5, elbo_part6))
}
