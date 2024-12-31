#' Simulate Comparison Vectors for Bipartite Matching
#'
#' This function simulates comparison vectors (in one-hot form) between
#' \code{n1} records in the first data set and \code{n2} records in the second data set,
#' assuming certain matching patterns and mismatch patterns dictated by
#' parameters \code{m}, \code{u}, \code{levels}, and the specified overlap.
#'
#' @param match_probs A numeric vector \code{m}, representing probability weights
#'   for each disagreement level when two records \emph{do} match. The length of
#'   \code{match_probs} should match \code{sum(levels)} once grouped by field.
#' @param nonmatch_probs A numeric vector \code{u}, representing probability weights
#'   for each disagreement level when two records \emph{do not} match. The length
#'   of \code{nonmatch_probs} should match \code{sum(levels)} once grouped by field.
#' @param field_levels A numeric vector \code{levels}, where \code{levels[f]}
#'   indicates how many disagreement levels exist for field \code{f}.
#' @param size_file1 An integer \code{n1}, representing how many records are in
#'   the first data set.
#' @param size_file2 An integer \code{n2}, representing how many records are in
#'   the second data set.
#' @param overlap An integer indicating how many records in the second data set
#'   are true matches with the first data set (out of \code{n2}).
#' @param previous_matches An integer indicating how many matches in the first data
#'   set come before these \code{overlap} records. This is used to offset indices
#'   so that record \code{j} in the second data set may match record
#'   \code{j + previous_matches} in the first data set.
#'
#' @return A list containing:
#'   \describe{
#'     \item{\code{comparisons}}{A \eqn{(n1 \times n2) \times \sum(\text{field_levels})}
#'       matrix of one-hot encoded comparisons. Each row corresponds to a pair
#'       of records (one from data set 1, one from data set 2).}
#'     \item{\code{n1}}{The same \code{n1} provided as input.}
#'     \item{\code{n2}}{The same \code{n2} provided as input.}
#'     \item{\code{nDisagLevs}}{The same \code{field_levels} provided as input.}
#'     \item{\code{Ztrue}}{A numeric vector of length \code{n2} indicating
#'       the true matching record in data set 1 for each record in data set 2.
#'       Non-matches are labeled \code{n1+1}.}
#'   }
#'
#' @examples
#' \dontrun{
#' m_probs <- c(0.2, 0.8, 0.1, 0.9)   # Example match probs
#' u_probs <- c(0.7, 0.3, 0.8, 0.2)   # Example non-match probs
#' field_levs <- c(2, 2)             # Two fields with 2 levels each
#' out <- simulate_comparisons(m_probs, u_probs, field_levs,
#'                             n1=5, n2=4, overlap=2, previous_matches=0)
#' }
#'
#' @export
simulate_comparisons <- function(match_probs, nonmatch_probs, field_levels,
                                 size_file1, size_file2, overlap,
                                 previous_matches = 0){

  # ---------------------------------------------------------------------
  # 1) Construct a 'field_marker' vector indicating which field each level belongs to
  #    For example, if field_levels = c(2,3), then field_marker might be c(1,1,2,2,2).
  # ---------------------------------------------------------------------
  field_marker <- unlist(lapply(seq_along(field_levels), function(f){
    rep(f, field_levels[f])
  }))
  num_fields <- length(field_levels)

  # ---------------------------------------------------------------------
  # 2) Set up total number of pairs and the grid of pair indices
  # ---------------------------------------------------------------------
  N <- size_file1 * size_file2
  pair_indices <- expand.grid(
    record1 = seq_len(size_file1),
    record2 = seq_len(size_file2)
  )

  # 'indicators' will store the discrete "disagreement level" (1..levels[f]) for each pair, each field
  indicators <- matrix(NA, nrow = N, ncol = num_fields)

  # ---------------------------------------------------------------------
  # 3) Identify which records in the second data set are truly matched
  #    and to which record in the first data set they match
  # ---------------------------------------------------------------------
  # E.g., df2matches = 1..overlap
  #       df1matches = df2matches + previous_matches
  df2matches <- seq_len(overlap)
  df1matches <- df2matches + previous_matches

  # Build a 'Ztrue' vector with length n2
  # Non-matches are labeled n1+1
  Ztrue <- rep(size_file1 + 1, size_file2)
  Ztrue[df2matches] <- df1matches

  # match_index indicates which row in pair_indices has record1 == record2+previous_matches
  # We pick the first 'overlap' of them
  # e.g. if overlap=2, we want the first 2 that satisfy record1 = record2 + previous_matches
  matched_rows <- which(
    pair_indices$record1 == (pair_indices$record2 + previous_matches)
  )[seq_len(overlap)]

  # ---------------------------------------------------------------------
  # 4) Split 'match_probs' & 'nonmatch_probs' by field_marker
  # ---------------------------------------------------------------------
  match_list <- split(match_probs, field_marker)
  nonmatch_list <- split(nonmatch_probs, field_marker)

  # ---------------------------------------------------------------------
  # 5) Generate random discrete levels for matched pairs and non-matched pairs
  # ---------------------------------------------------------------------
  # gamma_match is a matrix #overlap x num_fields
  gamma_match <- sapply(match_list, function(vec){
    # 'vec' is a probability vector to sample field_levels[f] categories
    sample.int(length(vec), overlap, replace=TRUE, prob=vec)
  })

  # gamma_nonmatch is a matrix (N-overlap) x num_fields
  gamma_nonmatch <- sapply(nonmatch_list, function(vec){
    sample.int(length(vec), N - overlap, replace=TRUE, prob=vec)
  })

  # ---------------------------------------------------------------------
  # 6) Place the matched levels into 'indicators' for 'matched_rows',
  #    non-matched levels for all other rows.
  # ---------------------------------------------------------------------
  if(overlap == 0){
    # No matches => everything is nonmatch
    indicators <- gamma_nonmatch
  } else {
    # fill matched rows
    indicators[matched_rows, ] <- gamma_match
    # fill all the rest
    indicators[-matched_rows, ] <- gamma_nonmatch
  }

  # ---------------------------------------------------------------------
  # 7) Convert 'indicators' into a big one-hot matrix 'gamma'
  # ---------------------------------------------------------------------
  ohe_list <- vector("list", num_fields)
  for(f in seq_len(num_fields)){
    field_num_levels <- field_levels[f]
    ohe_list[[f]] <- matrix(0, nrow = N, ncol = field_num_levels)

    # For each level ell in 1..field_num_levels
    # set that column to 1 where indicators[,f] == ell
    for(ell in seq_len(field_num_levels)){
      ohe_list[[f]][indicators[, f] == ell, ell] <- 1
    }
  }
  # combine horizontally
  gamma <- do.call(cbind, ohe_list)

  # ---------------------------------------------------------------------
  # 8) Return the final list
  # ---------------------------------------------------------------------
  list(
    comparisons = gamma,     # one-hot matrix (N x sum(field_levels))
    n1 = size_file1,
    n2 = size_file2,
    nDisagLevs = field_levels,  # same as input
    Ztrue = Ztrue          # length n2, the ground-truth matching
  )
}
