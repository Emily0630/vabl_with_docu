# tests/testthat/test_BRL_hash.R

library(testthat)
library(dplyr)
library(tidyr)
source("BRL_hash_original.R")
source("BRL_hash.R")
source("BRL_hash_sample_pair.R")
source("BRL_hash_compute_unique_weights.R")
source("BRL_hash_update_m_u.R")

test_that("BRL_hash matches for a small scenario in 'base' mode", {
  set.seed(123)

  # Mock a minimal 'hash' object with small n1, n2
  # Typically:
  #   hash$ohe             => matrix of unique patterns
  #   hash$total_counts    => numeric vector for each pattern
  #   hash$hash_count_list => list of length n2
  #   hash$hash_to_file_1  => nested structure
  #   hash$pair_to_pattern => list of length n2
  #   hash$field_marker    => integer vector for columns
  #   hash$n1, hash$n2
  # We'll build a small example.

  hash_mock <- list(
    ohe = matrix(c(1,0, 0,1), nrow=2, byrow=TRUE),  # 2 patterns x 2 columns
    total_counts = c(5, 3),
    hash_count_list = list(
      c(2,3),  # for rec2=1
      c(1,2),  # for rec2=2
      c(2,1)   # for rec2=3
    ),
    hash_to_file_1 = list(
      list(`1`=1:2, `2`=3:4),  # for rec2=1
      list(`1`=1,   `2`=2:3),  # for rec2=2
      list(`1`=2:3, `2`=4:5)   # for rec2=3
    ),
    pair_to_pattern = list(
      c(1,2),
      c(2,1),
      c(1,2)
    ),
    field_marker = c(1,1),  # both columns belong to field #1
    n1 = 5,
    n2 = 3
  )

  # Set parameters
  S     <- 100   # total iterations
  burn  <- 10    # burn-in
  alpha <- 1
  beta  <- 1
  m_prior <- 1
  u_prior <- 1

  # Run original vs. refactored in 'base' mode
  set.seed(999)  # so both calls do the same random draws
  old_out <- BRL_hash_original(
    hash_mock,
    m_prior = m_prior, u_prior = u_prior,
    alpha = alpha, beta = beta,
    S = S, burn = burn,
    show_progress = FALSE, seed = 999,
    mode = "base"
  )

  set.seed(999)
  new_out <- BRL_hash(
    hash_mock,
    m_prior = m_prior, u_prior = u_prior,
    alpha = alpha, beta = beta,
    S = S, burn = burn,
    show_progress = FALSE, seed = 999,
    mode = "base"
  )

  # Compare each part of the returned list
  # If the chains are indeed identical, we can do expect_equal.
  # If there's minor numeric differences, use expect_equivalent or a tolerance.
  expect_equal(old_out$Z, new_out$Z, tolerance = 1e-8)
  expect_equal(old_out$m, new_out$m, tolerance = 1e-8)
  expect_equal(old_out$u, new_out$u, tolerance = 1e-8)
  expect_equal(old_out$overlap, new_out$overlap, tolerance = 1e-8)
})

test_that("BRL_hash matches for 'rejection' mode (small example)", {
  set.seed(1234)

  hash_mock <- list(
    ohe = matrix(c(1,0, 0,1, 1,1), nrow=3, byrow=TRUE),
    total_counts = c(4, 5, 3),
    hash_count_list = list(
      c(3,1,0),
      c(1,2,2),
      c(2,2,1),
      c(1,3,2)
    ),
    hash_to_file_1 = list(
      list(`1`=1:3, `2`=2:4, `3`=5),
      list(`1`=1, `2`=2, `3`=4:5),
      list(`1`=5, `2`=1:2, `3`=3:4),
      list(`1`=2, `2`=3, `3`=1:4)
    ),
    pair_to_pattern = list(
      c(1,2,3),
      c(2,3,1),
      c(3,1,2),
      c(1,2,3)
    ),
    field_marker = c(1,1),
    n1 = 5,
    n2 = 4
  )

  S     <- 50
  burn  <- 5
  reject_iter <- 2  # small

  set.seed(2023)
  old_out <- BRL_hash_original(
    hash_mock,
    S = S,
    burn = burn,
    show_progress = FALSE,
    seed = 2023,
    reject_iter = reject_iter,
    mode = "rejection"
  )

  set.seed(2023)
  new_out <- BRL_hash(
    hash_mock,
    S = S,
    burn = burn,
    show_progress = FALSE,
    seed = 2023,
    reject_iter = reject_iter,
    mode = "rejection"
  )

  # Compare
  expect_equal(old_out$Z,       new_out$Z,       tolerance = 1e-8)
  expect_equal(old_out$m,       new_out$m,       tolerance = 1e-8)
  expect_equal(old_out$u,       new_out$u,       tolerance = 1e-8)
  expect_equal(old_out$overlap, new_out$overlap, tolerance = 1e-8)
})

test_that("BRL_hash: S=1 edge case in 'efficient' mode", {
  # Single iteration => we can see if they do the same draw for each record.
  set.seed(987)

  hash_mock <- list(
    ohe = matrix(c(1,0,0,1), nrow=2, byrow=TRUE),
    total_counts = c(10, 2),
    hash_count_list = list(
      c(4,6),
      c(1,1)
    ),
    hash_to_file_1 = list(
      list(`1`=1:4, `2`=5:10),
      list(`1`=1,   `2`=2:3)
    ),
    pair_to_pattern = list(
      c(1,2),
      c(1,2)
    ),
    field_marker = c(1,2),
    n1 = 10,
    n2 = 2
  )

  # Single iteration -> no real burn-in
  S    <- 1
  burn <- 0

  set.seed(987)
  old_out <- BRL_hash_original(
    hash_mock,
    S = S, burn = burn,
    mode = "efficient",
    show_progress = FALSE, seed = 987
  )

  set.seed(987)
  new_out <- BRL_hash(
    hash_mock,
    S = S, burn = burn,
    mode = "efficient",
    show_progress = FALSE, seed = 987
  )

  expect_equal(old_out$Z,       new_out$Z,       tolerance = 1e-8)
  expect_equal(old_out$m,       new_out$m,       tolerance = 1e-8)
  expect_equal(old_out$u,       new_out$u,       tolerance = 1e-8)
  expect_equal(old_out$overlap, new_out$overlap, tolerance = 1e-8)
})
