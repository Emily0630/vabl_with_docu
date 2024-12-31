#' Compare Records Between Two Data Frames
#'
#' This function takes two data frames and computes comparisons across specified
#' fields. It returns a one-hot encoded matrix of disagreement levels (comparisons)
#' and other metadata (number of records in each data frame and a vector of the number
#' of disagreement levels per field.).
#'
#' This function relies on three helper functions:
#' \enumerate{
#'   \item \code{\link{create_breaklist}} to generate numeric cut points.
#'   \item \code{\link{compute_string_distance}} for string-based distance calculations.
#'   \item \code{\link{one_hot_encode_factor}} to convert factor comparisons to one-hot encoded form.
#' }
#'
#' @param df1 A data frame containing the first file of records.
#' @param df2 A data frame containing the second file of records.
#' @param fields A character vector of field names that exist in both \code{df1} and \code{df2}.
#' @param fields_1 A character vector of field names for \code{df1}. Defaults to \code{fields}.
#' @param fields_2 A character vector of field names for \code{df2}. Defaults to \code{fields}.
#' @param types A character vector specifying the comparison type for each field:
#'   \code{"bi"} (binary comparison), \code{"lv"} (string distance), \code{"nu"} (numeric).
#' @param breaks Either a numeric vector or a list of numeric vectors giving cut
#'   points for discretizing numeric or string distances. Defaults to \code{c(0, 0.25, 0.5)}.
#'   \itemize{
#'     \item If \code{breaks} is a numeric vector, the same breakpoints
#'       are applied to all fields.
#'     \item If \code{breaks} is a list, each element of the list corresponds
#'       to each field in \code{fields}.
#'   }
#' @param distance_metric A string specifying how to compute string distance,
#'   either \code{"Levenshtein"} or \code{"Damerau-Levenshtein"}. Defaults to
#'   \code{"Levenshtein"}. Ignored for binary and numeric fields.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{comparisons}}{A matrix of one-hot encoded comparisons (disagreement levels).}
#'   \item{\code{n1}}{Number of rows in \code{df1}.}
#'   \item{\code{n2}}{Number of rows in \code{df2}.}
#'   \item{\code{nDisagLevs}}{A vector indicating the number of disagreement levels per field.}
#' }
#'
#' @examples
#' # Example: two datasets with fields name (string distance, "lv") and birth_year (numeric, "nu")
#' df1 <- data.frame(name = c("Alice", "Bob"), birth_year  = c(1980, 1990))
#' df2 <- data.frame(name = c("Alice", "John"), birth_year  = c(1981, 1992))
#' # Using default breaks for numeric (c(0, 0.25, 0.5))
#' compare_records(df1, df2, fields = c("name", "birth_year"), types = c("lv", "nu"))
#' @export

compare_records <- function(df1, df2, fields,
                            fields_1 = fields,
                            fields_2 = fields,
                            types = rep("bi", length(fields)),
                            breaks = c(0, 0.25, 0.5),
                            distance_metric = "Levenshtein") {
  # Number of records in each data frame
  n1 <- nrow(df1)
  n2 <- nrow(df2)

  # Number of fields to compare
  num_fields <- length(fields)

  # Create a list of breakpoints for each field
  breaklist <- create_breaklist(breaks, types)

  # Generate all pairwise combinations of rows (df1) x rows (df2)
  all_pairs <- expand.grid(idx_df1 = seq_len(n1),
                           idx_df2 = seq_len(n2))
  idx_df1 <- all_pairs$idx_df1
  idx_df2 <- all_pairs$idx_df2

  # Store factor comparisons in a list
  comparisons_list <- vector("list", length = num_fields)

  # For each field, compute the comparison factor
  for (f in seq_len(num_fields)) {
    field_type <- types[f]
    vec1 <- df1[idx_df1, fields_1[f]]
    vec2 <- df2[idx_df2, fields_2[f]]

    if (field_type == "bi") {
      # Binary comparison:
      # comp = 1 if mismatch, 2 if match
      comp <- as.integer(!(vec1 == vec2)) + 1
      comparisons_list[[f]] <- factor(comp)

    } else if (field_type == "lv") {
      # String distance (Levenshtein or Damerau-Levenshtein)
      dist_vals <- compute_string_distance(vec1, vec2, distance_metric)
      comp <- cut(dist_vals,
                  breaks = breaklist[[f]],
                  include.lowest = TRUE) |>
        as.integer() |>
        factor(levels = seq_len(length(breaklist[[f]]) - 1))

      comparisons_list[[f]] <- comp

    } else if (field_type == "nu") {
      # Numeric distance
      dist_vals <- abs(as.numeric(vec1) - as.numeric(vec2))
      comp <- cut(dist_vals,
                  breaks = breaklist[[f]],
                  include.lowest = TRUE) |>
        as.integer() |>
        factor(levels = seq_len(length(breaklist[[f]]) - 1))

      comparisons_list[[f]] <- comp

    } else {
      stop("Unsupported field type: must be 'bi', 'lv', or 'nu'.")
    }
  }

  # Compute how many levels each field produces
  n_levels <- sapply(comparisons_list, nlevels)

  # Build one-hot encoding for each field and combine
  ohe_list <- lapply(comparisons_list, one_hot_encode_factor)
  gamma_matrix <- do.call(cbind, ohe_list)

  # Return results in a named list
  list(comparisons = gamma_matrix,
       n1 = n1,
       n2 = n2,
       nDisagLevs = n_levels)
}
