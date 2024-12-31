# tests/testthat/test_simulate_comparisons.R

library(testthat)

source("simulate_comparisons_original.R")
source("simulate_comparisons.R")

test_that("simulate_comparisons matches for overlap=0 scenario", {
  # 1) Set up parameters for a small scenario with no overlap
  match_probs <- c(0.2, 0.8)
  nonmatch_probs <- c(0.7, 0.3)
  field_levels <- c(2)
  n1 <- 5
  n2 <- 4
  overlap_val <- 0

  set.seed(123)
  old_out <- simulate_comparisons_original(
    m = match_probs,
    u = nonmatch_probs,
    levels = field_levels,
    n1 = n1,
    n2 = n2,
    overlap = overlap_val
  )

  set.seed(123) # same seed => same random draws
  new_out <- simulate_comparisons(
    match_probs,
    nonmatch_probs,
    field_levels,
    size_file1 = n1,
    size_file2 = n2,
    overlap = overlap_val
  )

  # Compare
  # Because it's random, we expect identical output if random calls match
  expect_equal(old_out$Ztrue,       new_out$Ztrue)
  expect_equal(old_out$comparisons, new_out$comparisons)
  # Also check n1, n2, nDisagLevs
  expect_equal(old_out$n1,          new_out$n1)
  expect_equal(old_out$n2,          new_out$n2)
  expect_equal(old_out$nDisagLevs,  new_out$nDisagLevs)
})

test_that("simulate_comparisons matches for partial overlap scenario", {
  match_probs <- c(0.2, 0.8, 0.1, 0.9)
  nonmatch_probs <- c(0.7, 0.3, 0.6, 0.4)
  field_levels <- c(2, 2)  # 2 fields
  n1 <- 4
  n2 <- 4
  overlap_val <- 2

  set.seed(234)
  old_out <- simulate_comparisons_original(
    m = match_probs,
    u = nonmatch_probs,
    levels = field_levels,
    n1 = n1,
    n2 = n2,
    overlap = overlap_val
  )

  set.seed(234)
  new_out <- simulate_comparisons(
    match_probs,
    nonmatch_probs,
    field_levels,
    size_file1 = n1,
    size_file2 = n2,
    overlap = overlap_val
  )

  expect_equal(old_out$Ztrue,       new_out$Ztrue)
  expect_equal(old_out$comparisons, new_out$comparisons)
  expect_equal(old_out$n1,          new_out$n1)
  expect_equal(old_out$n2,          new_out$n2)
  expect_equal(old_out$nDisagLevs,  new_out$nDisagLevs)
})

test_that("simulate_comparisons matches with previous_matches offset", {
  match_probs <- c(0.3, 0.7)
  nonmatch_probs <- c(0.6, 0.4)
  field_levels <- c(2)
  n1 <- 5
  n2 <- 5
  overlap_val <- 3
  offset_val <- 2  # previous matches offset

  set.seed(345)
  old_out <- simulate_comparisons_original(
    m = match_probs,
    u = nonmatch_probs,
    levels = field_levels,
    n1 = n1,
    n2 = n2,
    overlap = overlap_val,
    previous_matches = offset_val
  )

  set.seed(345)
  new_out <- simulate_comparisons(
    match_probs,
    nonmatch_probs,
    field_levels,
    size_file1 = n1,
    size_file2 = n2,
    overlap = overlap_val,
    previous_matches = offset_val
  )

  expect_equal(old_out$Ztrue,       new_out$Ztrue)
  expect_equal(old_out$comparisons, new_out$comparisons)
  expect_equal(old_out$n1,          new_out$n1)
  expect_equal(old_out$n2,          new_out$n2)
  expect_equal(old_out$nDisagLevs,  new_out$nDisagLevs)
})

test_that("simulate_comparisons works when overlap=n2 (full overlap)", {
  # scenario => every record in the second data set matches a distinct record in the first data set
  match_probs <- c(0.1, 0.9)
  nonmatch_probs <- c(0.8, 0.2)
  field_levels <- c(2)
  n1 <- 5
  n2 <- 4
  overlap_val <- 4  # full overlap

  set.seed(456)
  old_out <- simulate_comparisons_original(
    m = match_probs,
    u = nonmatch_probs,
    levels = field_levels,
    n1 = n1,
    n2 = n2,
    overlap = overlap_val
  )

  set.seed(456)
  new_out <- simulate_comparisons(
    match_probs,
    nonmatch_probs,
    field_levels,
    size_file1 = n1,
    size_file2 = n2,
    overlap = overlap_val
  )

  expect_equal(old_out$Ztrue,       new_out$Ztrue)
  expect_equal(old_out$comparisons, new_out$comparisons)
  expect_equal(old_out$n1,          new_out$n1)
  expect_equal(old_out$n2,          new_out$n2)
  expect_equal(old_out$nDisagLevs,  new_out$nDisagLevs)
})
