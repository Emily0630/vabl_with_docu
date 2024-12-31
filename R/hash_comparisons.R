#' Hash the Comparison Data
#'
#' This function takes as input a comparison data list from
#' \code{\link{compare_records}} and computes various hashing structures used
#' in record linkage.
#'
#' This function relies on four helper functions:
#' \enumerate{
#'   \item \code{\link{hash_field}} to computes a hash vector for each level of a given field
#'   \item \code{\link{fs_to_ohe}} to convert field-specific indices to one-hot encoding
#'   \item \code{\link{possible_patterns_ohe}} to generate all possible one-hot encoded patterns,
#'   using \code{\link{fs_to_ohe}}
#'   \item \code{\link{sei}}
#' }
#'
#' @param comparison_data A list containing the output from \code{compare_records()}. Typically:
#'   \enumerate{
#'     \item An indicator (one-hot) matrix \code{(N x L)} of comparisons.
#'     \item The number of records in the first data frame (\code{n1}).
#'     \item The number of records in the second data frame (\code{n2}).
#'     \item A vector of level counts per field.
#'   }
#' @param algorithm A character vector specifying which algorithms to consider.
#'   Options include \code{"vabl"}, \code{"fabl"}, and \code{"BRL_hash"}.
#'   Defaults to \code{c("vabl", "fabl", "BRL_hash")}.
#' @param R A numeric value used by certain algorithms (e.g., \code{"fabl"}).
#'   Defaults to 0. If \code{"BRL_hash"} is included in \code{algorithm}, then
#'   \code{R} is forced to 0.
#' @param all_patterns Logical. If \code{TRUE}, the function enumerates
#'   \emph{all} possible one-hot patterns (via \code{\link{possible_patterns_ohe}})
#'   based on the \code{field_levels}, rather than only those observed in
#'   \code{comparison_data}.
#' @param store_pair_to_pattern Logical. It indicates whether to store pair-to-pattern
#'   mappings for the \code{"BRL_hash"} algorithm. Defaults to \code{TRUE}.
#'
#' @details
#' Internally, this function:
#' \enumerate{
#'   \item Retrieves the one-hot indicator matrix from \code{comparison_data}.
#'   \item Computes a "hash value" for each row by multiplying each column by a
#'         field-specific offset (using \code{\link{hash_field}}) and then summing.
#'   \item Groups these hashed rows to derive a variety of summary objects
#'         (\code{hash_count_list}, \code{total_counts}, \code{hash_to_file_1}, etc.).
#'   \item Optionally enumerates all possible patterns if \code{all_patterns = TRUE}.
#'   \item If \code{"fabl"} or \code{"BRL_hash"} is in \code{algorithm},
#'         applies \code{\link{sei}} to the subsets if \code{R > 0}.
#' }
#'
#' @return A list with components:
#'   \describe{
#'     \item{\code{ohe}}{Either the unique patterns \emph{observed} in the data
#'       (if \code{all_patterns = FALSE}) or \emph{all} possible one-hot patterns
#'       (if \code{all_patterns = TRUE}).}
#'     \item{\code{total_counts}}{The frequency of each pattern across
#'       all record pairs.}
#'     \item{\code{hash_count_list}}{A list of counts by record in the second
#'       data frame (\code{rec2}).}
#'     \item{\code{hash_to_file_1}}{For \code{"fabl"} or \code{"BRL_hash"},
#'       a nested structure organizing which \code{rec1} belong to each pattern
#'       for each \code{rec2}.}
#'     \item{\code{flags}}{If \code{"vabl"} is in \code{algorithm}, a list of
#'       patterns that appear \emph{exactly once} for each \code{rec2}.}
#'     \item{\code{field_marker}}{A vector indicating which field each column of
#'       the indicator matrix belongs to.}
#'     \item{\code{n1}, \code{n2}}{The number of records in each data frame.}
#'     \item{\code{pair_to_pattern}}{If \code{"BRL_hash"} is in \code{algorithm}
#'       and \code{store_pair_to_pattern = TRUE}, a list where each element
#'       contains the pattern IDs for that \code{rec2} across all \code{rec1}.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Suppose 'cd' is the result of compare_records(...)
#' hash_results <- hash_comparisons(cd, algorithm = "vabl", R = 0)
#'
#' # If we want to use "BRL_hash":
#' hash_results_brl <- hash_comparisons(cd, algorithm = "BRL_hash")
#' }
#'
#' @export

hash_comparisons <- function(
    comparison_data,
    algorithm = c("vabl", "fabl", "BRL_hash"),
    R = 0,
    all_patterns = FALSE,
    store_pair_to_pattern = TRUE
) {
  #----------------------------------------------------------------
  # 1) If "BRL_hash" is in algorithm, force R = 0
  #----------------------------------------------------------------
  if ("BRL_hash" %in% algorithm) {
    R <- 0
  }

  #----------------------------------------------------------------
  # 2) Extract elements from comparison_data
  #----------------------------------------------------------------
  indicator_matrix <- comparison_data[[1]]  # NxL one-hot matrix
  num_records_df1 <- comparison_data[[2]]
  num_records_df2 <- comparison_data[[3]]
  field_levels <- comparison_data[[4]]  # c(field1_levels, field2_levels, ...)

  # Basic quantities
  total_pairs <- nrow(indicator_matrix)  # N
  num_fields <- length(field_levels)
  field_indices <- seq_len(num_fields)

  # field_marker: for each column, record which field it belongs to
  field_marker <- unlist(
    lapply(field_indices, function(f) rep(f, field_levels[f]))
  )

  # A data frame of all (rec1, rec2) pairs
  pair_grid <- expand.grid(
    rec1 = seq_len(num_records_df1),
    rec2 = seq_len(num_records_df2)
  )

  #----------------------------------------------------------------
  # 3) Compute hash offsets for each column, using cumsum to build "offsets"
  #----------------------------------------------------------------
  level_cumsum <- cumsum(c(0, field_levels))
  # For field f, offset = level_cumsum[f], used by hash_field()

  # For each field, build a vector of size = field_levels[f] with the offsets
  # Then unlist them into a single vector for all columns
  hash_offsets <- purrr::imap(field_levels, ~ hash_field(.x, .y, level_cumsum)) %>%
    unlist()

  # Multiply each column by its offset, then sum across columns
  # Add 1 so hash values start at 1 (not 0)
  row_hash <- rowSums(sweep(indicator_matrix, 2, hash_offsets, `*`)) + 1

  #----------------------------------------------------------------
  # 4) Identify unique patterns (observed or possible)
  #----------------------------------------------------------------
  if (all_patterns) {
    # Enumerate ALL possible patterns via possible_patterns_ohe()
    all_possible_patterns <- possible_patterns_ohe(field_levels)

    # Compute hashed values for these possible patterns
    hashed_possible <- rowSums(
      sweep(all_possible_patterns, 2, hash_offsets, `*`)
    ) + 1

    num_patterns <- nrow(all_possible_patterns)

    # Match each row in the data to one of the hashed_possible
    hash_id <- factor(
      match(row_hash, hashed_possible),
      levels = seq_len(num_patterns)
    )

    # 'ohe' is the matrix of all possible patterns
    unique_pattern_matrix <- all_possible_patterns

  } else {
    # Only unique hashes observed in the data
    hashed_observed <- unique(row_hash)
    num_patterns <- length(hashed_observed)

    hash_id <- factor(
      match(row_hash, hashed_observed),
      levels = seq_len(num_patterns)
    )

    # The unique patterns are those rows in indicator_matrix
    # that correspond to distinct hash IDs.
    unique_pattern_matrix <- indicator_matrix[!duplicated(hash_id), ]
  }

  # Combine pair info with their assigned hash_id
  temp <- data.frame(
    rec1 = pair_grid$rec1,
    rec2 = pair_grid$rec2,
    hash_id
  )

  #----------------------------------------------------------------
  # 5) Build summary counts
  #----------------------------------------------------------------

  # For each rec2, how often does each hash_id occur?
  # hash_count_list => list of length num_records_df2
  # each entry is a column vector of pattern counts
  hash_count_list <- temp %>%
    group_by(rec2, hash_id, .drop = F) %>%
    count() %>%
    ungroup() %>%
    group_split(rec2) %>%
    purrr::map(~.x %>%
                 select(n) %>%
                 pull()
    )

  # total_counts => how many times each pattern occurs across all pairs
  total_counts <- temp %>%
    group_by(hash_id, .drop = F) %>%
    count() %>%
    pull()

  # pattern_lookup => all combos of (hash_id, rec2)
  pattern_lookup <- expand.grid(
    hash_id = seq_len(num_patterns),
    rec2    = seq_len(num_records_df2)
  ) %>%
    data.frame() %>%
    stats::setNames(c("hash_id", "rec2"))

  #----------------------------------------------------------------
  # 6) pair_to_pattern => If "BRL_hash" in algorithm & store_pair_to_pattern
  #----------------------------------------------------------------
  pair_to_pattern <- NULL
  if ("BRL_hash" %in% algorithm && store_pair_to_pattern) {
    pair_to_pattern <- temp %>%
      dplyr::select(hash_id, rec2) %>%
      dplyr::group_split(rec2, .keep = FALSE) %>%
      lapply(dplyr::pull, var = hash_id) %>%
      lapply(as.double)
  }

  #----------------------------------------------------------------
  # 7) Build 'hash_to_file_1'
  #    A nested structure grouping pairs by (rec2, hash_id).
  #    This is used by "fabl" or "BRL_hash" to do additional steps, applying sei().
  #----------------------------------------------------------------
  hash_to_file_1 <- temp %>%
    select(rec1, rec2, hash_id) %>%
    nest_by(rec2, hash_id, .keep = F) %>%
    mutate(hash_id = as.integer(hash_id)) %>%
    rowwise() %>%
    mutate(N = nrow(data))

  # Left join to ensure all combos of rec2/hash_id exist
  hash_to_file_1 <- dplyr::left_join(
    x  = pattern_lookup,
    y  = hash_to_file_1,
    by = c("hash_id", "rec2")
  )

  # Fill missing N with 0
  hash_to_file_1$N[is.na(hash_to_file_1$N)] <- 0

  #----------------------------------------------------------------
  # 8) 'flags' => if "vabl" is in algorithm,
  #    identify patterns with exactly one occurrence for each rec2
  #----------------------------------------------------------------
  flags <- NULL
  if ("vabl" %in% algorithm) {
    flags <- hash_to_file_1 %>%
      dplyr::filter(N == 1) %>%
      tidyr::unnest(data) %>%
      tidyr::complete(rec2 = unique(hash_to_file_1$rec2)) %>%
      dplyr::select(-N) %>%
      stats::setNames(c("rec2", "eligible_patterns", "eligible_records")) %>%
      dplyr::group_split(rec2, .keep = FALSE)
  }

  #----------------------------------------------------------------
  # 9) If "fabl" or "BRL_hash" => build nested list + optional sei() usage
  #----------------------------------------------------------------
  if ("fabl" %in% algorithm || "BRL_hash" %in% algorithm) {
    hash_to_file_1 <- hash_to_file_1 %>%
      dplyr::group_split(rec2) %>%
      purrr::map(~ .x %>% dplyr::group_split(hash_id)) %>%
      purrr::map(~ purrr::map(.x, `[[`, "data")) %>%
      purrr::map(~ purrr::map(.x, ~ unname(unlist(.x))))

    if (R > 0) {
      hash_to_file_1 <- lapply(hash_to_file_1, function(rec2_list) {
        purrr::map(rec2_list, ~ sei(.x, R))
      })
    }
  } else {
    # If neither "fabl" nor "BRL_hash", we don't store hash_to_file_1
    hash_to_file_1 <- NULL
  }

  #----------------------------------------------------------------
  # 10) Return final results
  #----------------------------------------------------------------
  list(
    ohe             = unique_pattern_matrix,  # unique or all possible patterns
    total_counts    = total_counts,
    hash_count_list = hash_count_list,
    hash_to_file_1  = hash_to_file_1,
    flags           = flags,
    field_marker    = field_marker,
    n1              = num_records_df1,
    n2              = num_records_df2,
    pair_to_pattern = pair_to_pattern
  )
}
