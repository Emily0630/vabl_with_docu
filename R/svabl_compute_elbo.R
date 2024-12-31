#' Compute ELBO for SVABL
#'
#' Replicates the six pieces of the ELBO calculation from your code,
#' but for the full data scenario (no batching).
#'
#' @param a,b,a_pi,b_pi Current parameters.
#' @param alpha,Beta,alpha_pi,beta_pi Original priors (assuming alpha_pi,beta_pi = 1).
#' @param hash_count_list A list of length \code{n2}.
#' @param n1,n2 Integers for record counts.
#' @param ohe A \code{P x L} pattern matrix.
#' @param field_marker An integer vector labeling columns to fields.
#' @param weights,phi,single,c_full,m_p,u_p Intermediate computations.
#'
#' @return A numeric scalar representing the ELBO.
#'
#' @export
svabl_compute_elbo <- function(
    a, b, a_pi, b_pi,
    alpha, Beta, alpha_pi, beta_pi,
    hash_count_list, n1, n2,
    ohe, field_marker,
    weights, phi, single, c_full,
    m_p, u_p, a_chunk, b_chunk
) {

  # 1) part
  elbo_part1 <- sapply(seq_len(n2), function(j){
    sum(hash_count_list[[j]] *
          ( phi*(weights - log(phi) + log(c_full[j]))/ c_full[j] + u_p ))
  }) %>%
    sum()

  # 2) part
  full_nonmatch <- sum(single / c_full)
  elbo_part2 <- single * sum((1/c_full) * log(c_full)) +
    full_nonmatch*(log(n1) - log(single)) - log(n1)*n2

  # 3) part
  elbo_part3 <- lbeta(a_pi, b_pi) - lbeta(alpha_pi, beta_pi)

  # 4) part => posterior over a,b
  post_ab <- sapply(list(a, b), function(y){
    split(y, field_marker) %>%
      sapply(function(x){
        sum(lgamma(x)) - lgamma(sum(x))
      }) %>%
      sum()
  }) %>% sum()

  # 5) part => prior over a,b
  prior_ab <- sapply(list(alpha, Beta), function(y){
    split(y, field_marker) %>%
      sapply(function(x){
        sum(lgamma(x)) - lgamma(sum(x))
      }) %>%
      sum()
  }) %>% sum()
  elbo_part5 <- -prior_ab

  # 6) part
  elbo_part6 <- sum((alpha - a) * a_chunk + (Beta - b) * b_chunk)


  sum(c(elbo_part1, elbo_part2, elbo_part3, post_ab, elbo_part5, elbo_part6))
}
