# tests/testthat/test_hash_comparisons.R

library(testthat)
library(dplyr)
library(tidyr)
source("hash_field.R")
source("possible_patterns_ohe.R")
source("fs_to_ohe.R")
source("sei.R")
source("hash_comparisons.R")
source("hash_field_original.R")
source("possible_patterns_ohe_original.R")
source("fs_to_ohe_original.R")
source("sei_original.R")
source("hash_comparisons_original.R")


# If you’re in a non-package setup, you may need to source the old code:
# source("original_hash_comparisons.R")
# source("hash_comparisons.R")

# -------------------------------------------------------------------------
# 1. Minimal Test: Single-Field Comparisons
#    Suppose comparison_data has only 1 field, with 2 levels, and a small set of pairs
# -------------------------------------------------------------------------
test_that("hash_comparisons matches for single-field data", {

  # Create a minimal 'comparison_data' list that the functions expect
  # Typically: [[1]] -> indicator_matrix, [[2]] -> n1, [[3]] -> n2, [[4]] -> field_levels
  # Suppose we have 2 records in df1, 2 in df2 => total 2*2=4 pairs
  # Single field with 2 levels => one-hot matrix of size (4 rows, 2 columns).

  indicator_mat <- matrix(c(
    1, 0,
    0, 1,
    1, 0,
    0, 1
  ), nrow = 4, byrow = TRUE)

  comparison_data <- list(
    indicator_mat,  # NxL = 4x2
    2,              # n1
    2,              # n2
    c(2)            # field_levels = c(2) => just 1 field with 2 levels
  )

  # Compare results from the original and refactored versions
  old_out <- hash_comparisons_original(comparison_data)
  new_out <- hash_comparisons          (comparison_data)

  # Compare all elements of the returned lists (some might be lists/vectors)
  expect_equal(old_out$ohe,             new_out$ohe)
  expect_equal(old_out$total_counts,    new_out$total_counts)
  expect_equal(old_out$hash_count_list, new_out$hash_count_list)
  expect_equal(old_out$hash_to_file_1,  new_out$hash_to_file_1)
  expect_equal(old_out$flags,          new_out$flags)
  expect_equal(old_out$field_marker,    new_out$field_marker)
  expect_equal(old_out$n1,             new_out$n1)
  expect_equal(old_out$n2,             new_out$n2)
  expect_equal(old_out$pair_to_pattern, new_out$pair_to_pattern)
})

# -------------------------------------------------------------------------
# 2. Multiple Fields, 'all_patterns = FALSE'
# -------------------------------------------------------------------------
test_that("hash_comparisons matches for multiple fields without enumerating all patterns", {
  # Simulate a scenario with 2 fields:
  #   field1 -> 2 levels
  #   field2 -> 3 levels
  # Suppose we have 3 records in df1, 2 in df2 => 6 pairs total
  # We'll craft an indicator matrix with 6 rows, 5 columns (since 2+3 = 5).

  indicator_mat <- matrix(c(
    # Row1
    1,0,  1,0,0,
    # Row2
    0,1,  0,1,0,
    # Row3
    1,0,  0,0,1,
    # Row4
    0,1,  1,0,0,
    # Row5
    1,0,  0,1,0,
    # Row6
    0,1,  0,0,1
  ), nrow = 6, byrow = TRUE)

  comparison_data <- list(
    indicator_mat,  # NxL = 6x5
    3,              # n1
    2,              # n2
    c(2,3)          # field_levels = c(2,3)
  )

  old_out <- hash_comparisons_original(comparison_data,
                                       algorithm = c("vabl","fabl"),
                                       R = 0,
                                       all_patterns = FALSE,
                                       store_pair_to_pattern = FALSE)

  new_out <- hash_comparisons(comparison_data,
                                         algorithm = c("vabl","fabl"),
                                         R = 0,
                                         all_patterns = FALSE,
                                         store_pair_to_pattern = FALSE)

  # Compare
  expect_equal(old_out$ohe,             new_out$ohe)
  expect_equal(old_out$total_counts,    new_out$total_counts)
  expect_equal(old_out$hash_count_list, new_out$hash_count_list)
  expect_equal(old_out$hash_to_file_1,  new_out$hash_to_file_1)
  expect_equal(old_out$flags,          new_out$flags)
  expect_equal(old_out$field_marker,    new_out$field_marker)
  expect_equal(old_out$n1,             new_out$n1)
  expect_equal(old_out$n2,             new_out$n2)
  expect_equal(old_out$pair_to_pattern, new_out$pair_to_pattern)
})

# -------------------------------------------------------------------------
# 3. Multiple Fields, 'all_patterns = TRUE'
#    Tests enumerating every possible pattern
# -------------------------------------------------------------------------
test_that("hash_comparisons matches for multiple fields with all_patterns = TRUE", {
  # Same scenario as above, but now we set all_patterns=TRUE
  indicator_mat <- matrix(c(
    1,0,  1,0,0,
    0,1,  0,1,0,
    1,0,  0,0,1,
    0,1,  1,0,0,
    1,0,  0,1,0,
    0,1,  0,0,1
  ), nrow = 6, byrow = TRUE)

  comparison_data <- list(
    indicator_mat,
    3,
    2,
    c(2,3)
  )

  old_out <- hash_comparisons_original(comparison_data,
                                       algorithm = c("vabl","fabl"),
                                       R = 0,
                                       all_patterns = TRUE)

  new_out <- hash_comparisons(comparison_data,
                                         algorithm = c("vabl","fabl"),
                                         R = 0,
                                         all_patterns = TRUE)

  expect_equal(old_out$ohe,             new_out$ohe)
  expect_equal(old_out$total_counts,    new_out$total_counts)
  expect_equal(old_out$hash_count_list, new_out$hash_count_list)
  expect_equal(old_out$hash_to_file_1,  new_out$hash_to_file_1)
  expect_equal(old_out$flags,          new_out$flags)
  expect_equal(old_out$field_marker,    new_out$field_marker)
  expect_equal(old_out$n1,             new_out$n1)
  expect_equal(old_out$n2,             new_out$n2)
  expect_equal(old_out$pair_to_pattern, new_out$pair_to_pattern)
})

# -------------------------------------------------------------------------
# 4. Testing "BRL_hash" scenario with store_pair_to_pattern
# -------------------------------------------------------------------------
test_that("hash_comparisons matches with 'BRL_hash' and store_pair_to_pattern=TRUE", {
  # Minimal example with a small indicator matrix:
  indicator_mat <- matrix(c(
    1,0,
    0,1,
    1,0
  ), nrow = 3, byrow = TRUE)

  comparison_data <- list(
    indicator_mat,  # 3x2
    3,              # n1
    1,              # n2
    c(2)            # field_levels => single field with 2 levels
  )

  # 'BRL_hash' => R forced to 0, store_pair_to_pattern => will produce a non-null pair_to_pattern
  old_out <- hash_comparisons_original(comparison_data,
                                       algorithm = "BRL_hash",
                                       R = 100,  # even if we pass a big R, it should be forced to 0
                                       all_patterns = FALSE,
                                       store_pair_to_pattern = TRUE)

  new_out <- hash_comparisons(comparison_data,
                                         algorithm = "BRL_hash",
                                         R = 100,
                                         all_patterns = FALSE,
                                         store_pair_to_pattern = TRUE)

  expect_equal(old_out$ohe,             new_out$ohe)
  expect_equal(old_out$total_counts,    new_out$total_counts)
  expect_equal(old_out$hash_count_list, new_out$hash_count_list)
  expect_equal(old_out$hash_to_file_1,  new_out$hash_to_file_1)
  expect_equal(old_out$flags,          new_out$flags)
  expect_equal(old_out$field_marker,    new_out$field_marker)
  expect_equal(old_out$n1,             new_out$n1)
  expect_equal(old_out$n2,             new_out$n2)
  expect_equal(old_out$pair_to_pattern, new_out$pair_to_pattern)
})

# -------------------------------------------------------------------------
# 5. Testing an Edge Case with an Empty Indicator Matrix
#    (e.g., n1=0 or n2=0 => no pairs).
#    If your code doesn't handle zero pairs gracefully, consider skipping this.
# -------------------------------------------------------------------------
# test_that("hash_comparisons matches for empty indicator matrix (no pairs)", {
#   # Suppose n1=0, n2=2 => 0 pairs
#   # Then indicator matrix is 0 x L for some L
#   indicator_mat <- matrix(nrow = 0, ncol = 3)
#
#   comparison_data <- list(
#     indicator_mat,
#     0,   # n1
#     2,   # n2
#     c(3) # single field with 3 levels
#   )
#
#   old_out <- hash_comparisons_original(comparison_data)
#   new_out <- hash_comparisons(comparison_data)
#
#   # We may need to check how each version handles empties.
#   # If original code fails, you'd see an error or mismatch.
#   # If both handle empties, we compare all relevant elements.
#   expect_equal(old_out$ohe,             new_out$ohe)
#   expect_equal(old_out$total_counts,    new_out$total_counts)
#   expect_equal(old_out$hash_count_list, new_out$hash_count_list)
#   expect_equal(old_out$hash_to_file_1,  new_out$hash_to_file_1)
#   expect_equal(old_out$flags,          new_out$flags)
#   expect_equal(old_out$field_marker,    new_out$field_marker)
#   expect_equal(old_out$n1,             new_out$n1)
#   expect_equal(old_out$n2,             new_out$n2)
#   expect_equal(old_out$pair_to_pattern, new_out$pair_to_pattern)
# })
