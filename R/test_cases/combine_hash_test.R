# tests/testthat/test_combine_hash.R

library(testthat)
library(dplyr)
library(tidyr)

source("combine_hash_original.R")
source("combine_hash.R")
source("combine_flags.R")
source("combine_total_counts.R")
source("combine_hash_count_list.R")
source("combine_hash_to_file_1.R")
source("combine_pair_to_pattern.R")
test_that("combine_hash matches for minimal hash_list input", {
  # Suppose each hash_list element is a small named list with the required structure:
  #   $ohe
  #   $total_counts
  #   $hash_count_list
  #   $hash_to_file_1
  #   $flags
  #   $field_marker
  #   $pair_to_pattern
  # We'll create a minimal example.

  mock_hash_1 <- list(
    ohe             = matrix(c(1, 0, 0, 1), nrow = 2),
    total_counts    = c(10, 5),
    hash_count_list = list(c(3, 7), c(2, 3)),
    hash_to_file_1  = list(c(1, 2), c(3, 4)),
    flags           = list(c("A"), c("B")),
    field_marker    = c(1, 2),
    pair_to_pattern = list(c(2, 2), c(1, 1))
  )

  # Make a second element in hash_list
  mock_hash_2 <- list(
    ohe             = matrix(c(1, 0, 0, 1), nrow = 2),  # same structure
    total_counts    = c(5, 10),    # reversed
    hash_count_list = list(c(1, 1), c(6, 4)),
    hash_to_file_1  = list(c(5, 6), c(7, 8)),
    flags           = list(c("X"), c("Y")),
    field_marker    = c(1, 2),
    pair_to_pattern = list(c(3, 3), c(2, 2))
  )

  hash_list  <- list(mock_hash_1, mock_hash_2)
  n1         <- 5
  n2         <- 4

  # Run original vs. refactored
  old_out <- combine_hash_original(hash_list, n1, n2)
  new_out <- combine_hash(hash_list, num_records_df1 = n1, num_records_df2 = n2)

  # Check each element of the returned list
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

test_that("combine_hash matches for multiple objects in hash_list", {
  # We'll create 3 objects in hash_list.
  # If your function adds them column-wise or flattens them, let's test that.

  mock_hash_1 <- list(
    ohe             = matrix(c(1,0,0,1), nrow = 2),
    total_counts    = c(2,2),
    hash_count_list = list(c(1,1), c(1,1)),
    hash_to_file_1  = list(c(10, 20), c(30, 40)),
    flags           = list(c("flagA"), c("flagB")),
    field_marker    = c(1, 2),
    pair_to_pattern = list(c(1, 2), c(2, 3))
  )
  mock_hash_2 <- list(
    ohe             = matrix(c(1,0,0,1), nrow = 2),
    total_counts    = c(10,5),
    hash_count_list = list(c(3,7), c(2,3)),
    hash_to_file_1  = list(c(1, 2), c(3, 4)),
    flags           = list(c("A"), c("B")),
    field_marker    = c(1, 2),
    pair_to_pattern = list(c(2, 2), c(1, 1))
  )
  mock_hash_3 <- list(
    ohe             = matrix(c(1,0,0,1), nrow = 2),
    total_counts    = c(3, 3),
    hash_count_list = list(c(5,5), c(4,4)),
    hash_to_file_1  = list(c(9, 9), c(8, 8)),
    flags           = list(c("XX"), c("YY")),
    field_marker    = c(1, 2),
    pair_to_pattern = list(c(9, 9), c(8, 8))
  )

  hash_list  <- list(mock_hash_1, mock_hash_2, mock_hash_3)
  n1         <- 12
  n2         <- 10

  old_out <- combine_hash_original(hash_list, n1, n2)
  new_out <- combine_hash(hash_list, num_records_df1 = n1, num_records_df2 = n2)

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

# test_that("combine_hash works with an empty hash_list", {
#   # Edge case: if hash_list is empty, how do both versions behave?
#   # Possibly your original code might throw an error or produce a minimal list.
#   # Check what your old code does in that scenario. We'll see if they match.
#
#   empty_hash_list <- list()
#   n1 <- 0
#   n2 <- 0
#
#   old_out <- combine_hash_original(empty_hash_list, n1, n2)
#   new_out <- combine_hash(empty_hash_list, num_records_df1 = n1, num_records_df2 = n2)
#
#   # If the old code crashes, you might mark this test as skip(), or adjust your function
#   # to handle empty lists. Otherwise, compare the results:
#   expect_equal(old_out, new_out)
# })

test_that("combine_hash works when elements in hash_list differ in partial ways", {
  # Suppose the 'ohe' is the same, but the second object has different dimension
  # for hash_to_file_1. This might or might not be allowed, but let's see if both
  # versions handle it similarly.

  mock_hash_1 <- list(
    ohe             = matrix(c(1,0,0,1), nrow=2),
    total_counts    = c(2,2),
    hash_count_list = list(c(1,1), c(1,1)),
    hash_to_file_1  = list(c(10, 20), c(30, 40)),
    flags           = list(c("flagA"), c("flagB")),
    field_marker    = c(1, 2),
    pair_to_pattern = list(c(1,2))
  )

  mock_hash_2 <- list(
    ohe             = matrix(c(1,0,0,1), nrow=2),
    total_counts    = c(10,5),
    hash_count_list = list(c(3,7)),
    hash_to_file_1  = list(c(1, 2), c(3, 4), c(9,9)),  # extra element
    flags           = list(c("A"), c("B")),
    field_marker    = c(1, 2),
    pair_to_pattern = list(c(2,2), c(1,1), c(5,5))
  )

  hash_list <- list(mock_hash_1, mock_hash_2)
  n1 <- 4
  n2 <- 2

  # See if the old code handles mismatched lengths.
  # If so, does the new code do the same?
  # This test might fail if one version tries to flatten a different length
  # or if there's an assumption of identical lengths.

  old_out <- combine_hash_original(hash_list, n1, n2)
  new_out <- combine_hash(hash_list, n1, n2)

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
