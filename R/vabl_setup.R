#' Setup and Initialize VABL
#'
#' Extracts data from \code{hash} and initializes parameters \code{a,b} depending
#' on \code{b_init}.
#'
#' @param hash A list containing elements \code{ohe, total_counts, hash_count_list,
#'   field_marker, n1, n2}.
#' @param b_init Logical. If \code{TRUE}, initialize \code{b} by summing over
#'   patterns weighted by \code{total_counts}.
#'
#' @return A list with:
#'   \itemize{
#'     \item \code{ohe}, \code{total_counts}, \code{hash_count_list}, etc. (same as input)
#'     \item \code{a} A numeric vector of ones.
#'     \item \code{b} A numeric vector either from summation or ones, depending on \code{b_init}.
#'     \item \code{alpha}, \code{Beta} The prior vectors, both ones here.
#'   }
#'
#' @export
vabl_setup <- function(hash, b_init) {
  ohe <- hash$ohe
  P <- dim(ohe)[1]
  L <- dim(ohe)[2]
  total_counts <- hash$total_counts
  hash_count_list <- hash$hash_count_list
  field_marker <- hash$field_marker
  n1 <- hash$n1
  n2 <- hash$n2

  alpha <- rep(1, length(field_marker))
  Beta  <- rep(1, length(field_marker))
  a     <- rep(1, length(field_marker))

  if(b_init){
    # identical to your inline approach
    b <- hash$ohe %>%
      sweep(1, hash$total_counts, "*") %>%
      colSums() + Beta
  } else {
    b <- rep(1, length(field_marker))
  }

  list(ohe = ohe, P = P, L = L,
       total_counts = total_counts,
       hash_count_list = hash_count_list,
       field_marker = field_marker,
       n1 = n1, n2 = n2,
       alpha = alpha, Beta = Beta,
       a = a, b = b)
}
