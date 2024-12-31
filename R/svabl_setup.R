#' Setup and Initialize SVABL
#'
#' This function extracts data from \code{hash} and initializes the
#' parameters \code{a} and \code{b} depending on whether \code{b_init} is \code{TRUE}.
#'
#' @param hash A list containing at least:
#'   \itemize{
#'     \item \code{ohe, total_counts, hash_count_list, field_marker, n1, n2}
#'   }
#' @param b_init Logical. If \code{TRUE}, \code{b} is initialized by summing over
#'   \code{ohe} weighted by \code{total_counts}, otherwise \code{b} is a vector
#'   of ones.
#'
#' @return A list with elements:
#' \describe{
#'   \item{\code{a}}{A numeric vector of ones (Dirichlet parameter).}
#'   \item{\code{b}}{Either a numeric vector computed from \code{ohe} or all ones.}
#'   \item{\code{alpha}}{A numeric vector of ones (prior).}
#'   \item{\code{Beta}}{A numeric vector of ones (prior).}
#'   \item{\code{ohe}, \code{field_marker}, \code{hash_count_list}, \code{n1}, \code{n2}, etc.}
#' }
#'
#' @export
svabl_setup <- function(hash, b_init) {
  ohe <- hash$ohe
  P <- nrow(ohe)
  L <- ncol(ohe)
  total_counts <- hash$total_counts
  hash_count_list <- hash$hash_count_list
  field_marker <- hash$field_marker
  n1 <- hash$n1
  n2 <- hash$n2

  alpha <- rep(1, length(field_marker))
  Beta  <- rep(1, length(field_marker))
  a     <- rep(1, length(field_marker))

  if(b_init){
    b <- hash$ohe %>%
      sweep(., 1, total_counts, "*") %>%
      colSums() + Beta
  } else {
    b <- rep(1, length(field_marker))
  }

  list(
    a = a,
    b = b,
    alpha = alpha,
    Beta = Beta,
    ohe = ohe,
    field_marker = field_marker,
    hash_count_list = hash_count_list,
    n1 = n1,
    n2 = n2,
    P  = P
  )
}
