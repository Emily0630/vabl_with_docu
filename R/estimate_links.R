#' Estimate Record Linkage from FABL/VABL Output
#'
#' This function takes the output from a FABL or VABL run (i.e., a set of posterior
#' samples or pattern weights) and computes the final linkage estimates for each
#' record in the second data set. It supports multiple labeling conventions for
#' non-matches, threshold-based reject options, and an optional resolution pass
#' to enforce one-to-one matching.
#'
#' @param out The output from either FABL or VABL. If the first named element is
#'   \code{"Z"}, it is assumed to be from FABL (with posterior samples of \code{Z}).
#'   If the first element is \code{"pattern_weights"}, it is assumed to be from
#'   VABL (with pattern-based representation).
#' @param hash A list containing at least:
#'   \itemize{
#'     \item \code{n1}: number of records in the first data set
#'     \item \code{n2}: number of records in the second data set
#'     \item \code{ohe}, \code{total_counts}, \code{hash_count_list}, etc. (if needed)
#'     \item \code{flags} (if relevant for VABL's linking logic)
#'   }
#' @param l_FNM A numeric penalty for false non-match. Defaults to \code{1}.
#' @param l_FM1 A numeric penalty for false match scenario 1. Defaults to \code{1}.
#' @param l_FM2 A numeric penalty for false match scenario 2. Defaults to \code{2}.
#' @param l_R A numeric penalty for a "reject" decision. Defaults to \code{Inf} (no reject).
#' @param nonmatch_label A string indicating how non-matches are labeled.
#'   Options might be \code{"zero"} or \code{"n_1 + j"}. Defaults to \code{"zero"}.
#' @param resolve A logical indicating whether a final pass should enforce
#'   one-to-one matching by resolving double matches. Defaults to \code{TRUE}.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{Z_hat}}{An integer vector of length \code{n2} containing the
#'     final estimated matches for each record in the second data set. \code{0}
#'     or \code{n1+j} indicates a non-match, and \code{-1} may represent a
#'     "reject" option if \code{l_R < Inf}.}
#'   \item{\code{prob}}{A numeric vector of length \code{n2} indicating the
#'     probability associated with the best match (or best pattern).}
#' }
#'
#' @examples
#' \dontrun{
#'   # Suppose `out_fabl` is the output from a FABL run
#'   # Suppose `out_vabl` is the output from a VABL run
#'   # `hash_data` is the associated hash list with n1, n2, etc.
#'
#'   links_fabl <- estimate_links(out_fabl, hash_data)
#'   links_vabl <- estimate_links(out_vabl, hash_data, l_R=2, resolve=FALSE)
#' }
#'
#' @export
estimate_links <- function(out, hash,
                           l_FNM=1, l_FM1=1, l_FM2=2, l_R=Inf,
                           nonmatch_label = "zero", resolve = TRUE){

  # -------------------------------------------------------------------------
  # 1) Extract data from 'hash'
  # -------------------------------------------------------------------------
  n1 <- hash$n1
  n2 <- hash$n2

  # -------------------------------------------------------------------------
  # 2) If the first named element is "Z", we assume the FABL approach
  # -------------------------------------------------------------------------
  if(names(out)[1] == "Z"){
    fabl_info <- estimate_links_fabl(
      out = out,
      n1 = n1
    )
    best_match     <- fabl_info$best_match
    prob_best_match <- fabl_info$prob_best_match
    prob_no_link    <- fabl_info$prob_no_link
    link_indicator  <- fabl_info$link_indicator
    n2              <- fabl_info$n2
    Z_hat           <- fabl_info$Z_hat
  }

  # -------------------------------------------------------------------------
  # 3) If the first named element is "pattern_weights", assume the VABL approach
  # -------------------------------------------------------------------------
  else if(names(out)[1] == "pattern_weights"){
    vabl_info <- estimate_links_vabl(
      out         = out,
      hash        = hash
    )
    best_match     <- vabl_info$best_match
    prob_best_match <- vabl_info$prob_best_match
    prob_no_link    <- vabl_info$prob_no_link
    link_indicator  <- vabl_info$link_indicator
    n2              <- vabl_info$n2
    Z_hat           <- vabl_info$Z_hat
  }

  # -------------------------------------------------------------------------
  # 4) If l_R == Inf, use threshold from Theorem 1
  # -------------------------------------------------------------------------
  if(l_R == Inf){
    Z_hat <- estimate_links_no_reject(
      Z_hat, n1, n2,
      prob_no_link, prob_best_match, link_indicator,
      l_FNM, l_FM1, l_FM2,
      nonmatch_label
    )
  }

  # -------------------------------------------------------------------------
  # 5) If l_R < Inf, we have a reject option
  # -------------------------------------------------------------------------
  else {
    Z_hat <- estimate_links_with_reject(
      Z_hat, n1, n2,
      prob_no_link, prob_best_match, link_indicator,
      l_FNM, l_FM1, l_FM2, l_R,
      nonmatch_label
    )
  }

  # -------------------------------------------------------------------------
  # 6) Optionally enforce one-to-one matching
  # -------------------------------------------------------------------------
  if(resolve){
    Z_hat <- estimate_links_resolve_1to1(
      Z_hat, l_R, n1, n2,
      prob_best_match
    )
  }

  return(list(
    Z_hat = Z_hat,
    prob  = prob_best_match
  ))
}
