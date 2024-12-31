#' Combine Multiple Hash Outputs
#'
#' This function merges the outputs of multiple \code{hash_comparisons()}
#' calls into a single unified list. For instance, if you parallelized
#' \code{hash_comparisons} across subsets of data, you can recombine
#' them using this function.
#'
#' This function relies on five helper functions:
#' \enumerate{
#'   \item \code{\link{combine_total_counts}} to combine total counts across multiple hash outputs
#'   \item \code{\link{combine_hash_count_list}} to merge the \code{hash_count_list} elements across
#'    multiple hash outputs into a single list.
#'   \item \code{\link{combine_hash_to_file_1}} to merge the \code{hash_to_file_1} objects across
#'    multiple hash outputs into a single list
#'    \item \code{\link{combine_flags}} to merge the \code{flags} objects across multiple hash outputs
#'    into a single flattened list
#'    \item \code{\link{combine_pair_to_pattern}} to flatten \code{pair_to_pattern} entries across
#'    multiple hash outputs
#' }
#'
#' @param hash_list A list of objects returned by \code{hash_comparisons()}.
#'   Each element in \code{hash_list} should be a named list containing at
#'   least:
#'   \itemize{
#'     \item \code{ohe}
#'     \item \code{total_counts}
#'     \item \code{hash_count_list}
#'     \item \code{hash_to_file_1}
#'     \item \code{flags}
#'     \item \code{field_marker}
#'     \item \code{pair_to_pattern}
#'   }
#' @param num_records_df1 The total number of records in the first data frame
#'   (across all subsets, if split).
#' @param num_records_df2 The total number of records in the second data frame
#'   (across all subsets).
#'
#' @return A single named list of the same structure, but with combined
#'   information:
#'   \describe{
#'     \item{\code{ohe}}{The one-hot pattern matrix from the \strong{first} element
#'       of \code{hash_list}. (Assumed identical across all elements.)}
#'     \item{\code{total_counts}}{Combined column sums of \code{total_counts}
#'       across all elements.}
#'     \item{\code{hash_count_list}}{Flattened concatenation of all
#'       \code{hash_count_list} items.}
#'     \item{\code{hash_to_file_1}}{Flattened concatenation of all
#'       \code{hash_to_file_1} items.}
#'     \item{\code{flags}}{Flattened concatenation of all \code{flags}.}
#'     \item{\code{field_marker}}{The field marker vector from the first element
#'       of \code{hash_list}.}
#'     \item{\code{n1}}{The total number of records in the first data frame (\code{num_records_df1}).}
#'     \item{\code{n2}}{The total number of records in the second data frame (\code{num_records_df2}).}
#'     \item{\code{pair_to_pattern}}{Flattened concatenation of all
#'       \code{pair_to_pattern}.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Suppose you ran hash_comparisons() in parallel, and have a list:
#' # parallel_results <- list(result1, result2, result3)
#' # Now combine:
#' combined <- combine_hash(parallel_results, n1 = 1000, n2 = 800)
#' }
#'
#' @export
combine_hash <- function(hash_list,
                         num_records_df1,
                         num_records_df2) {
  # 1) Combine total_counts by summing them column-wise
  combined_total_counts <- combine_total_counts(hash_list)

  # 2) Flatten hash_count_list across all elements
  combined_hash_count_list <- combine_hash_count_list(hash_list)

  # 3) Flatten hash_to_file_1
  combined_hash_to_file_1 <- combine_hash_to_file_1(hash_list)

  # 4) Flatten flags
  combined_flags <- combine_flags(hash_list)

  # 5) Flatten pair_to_pattern
  combined_pair_to_pattern <- combine_pair_to_pattern(hash_list)

  # 6) Construct output
  #     - We assume each element in hash_list has an identical 'ohe'
  #       and 'field_marker', so we just take them from the first
  merged_output <- list(
    ohe             = hash_list[[1]]$ohe,
    total_counts    = combined_total_counts,
    hash_count_list = combined_hash_count_list,
    hash_to_file_1  = combined_hash_to_file_1,
    flags           = combined_flags,
    field_marker    = hash_list[[1]]$field_marker,
    n1              = num_records_df1,
    n2              = num_records_df2,
    pair_to_pattern = combined_pair_to_pattern
  )

  merged_output
}
