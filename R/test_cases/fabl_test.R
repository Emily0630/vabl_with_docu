# tests/testthat/test_fabl.R

library(testthat)
library(dplyr)
library(tidyr)
source("fabl_sample_Z.R")
source("fabl_compute_weights.R")
source("fabl_compute_m_u.R")
source("fabl_original.R")
source("fabl.R")

test_that("fabl matches for a small scenario with default priors", {
  set.seed(100)

  # Create a minimal mock hash object
  # Typically it contains:
  #   hash$ohe             => (P x L) matrix
  #   hash$total_counts    => numeric vector length P
  #   hash$hash_count_list => list of length n2, each numeric vector of length P
  #   hash$hash_to_file_1  => list of length n2, each is sub-list for patterns
  #   hash$field_marker    => integer vector, length L
  #   hash$n1, hash$n2
  hash_mock <- list(
    ohe = matrix(c(1,0,0,1), nrow = 2, byrow = TRUE),  # 2 patterns x 2 features
    total_counts = c(10, 5),
    hash_count_list = list(
      c(3,7),
      c(2,3),
      c(1,4)
    ),
    hash_to_file_1 = list(
      list("1" = c(1,2,3), "2" = c(4,5,6,7)),
      list("1" = c(1,2),   "2" = c(3,4,5)),
      list("1" = 1:5,      "2" = 6:10)
    ),
    field_marker = c(1,1),
    n1 = 10,
    n2 = 3
  )

  # Use small S for quick testing
  S <- 20
  burn <- 5

  set.seed(999)
  old_out <- fabl_original(
    hash_mock,
    m_prior = 1,
    u_prior = 1,
    alpha   = 1,
    beta    = 1,
    S       = S,
    burn    = burn,
    show_progress = FALSE
  )

  set.seed(999)
  new_out <- fabl(
    hash_mock,
    m_prior = 1,
    u_prior = 1,
    alpha   = 1,
    beta    = 1,
    S       = S,
    burn    = burn,
    show_progress = FALSE
  )

  # Compare each element in the returned list.
  # If everything is exactly the same, expect_equal() will pass.
  # If tiny numeric differences occur, add tolerance (e.g., 1e-8).
  expect_equal(old_out$Z,       new_out$Z,       tolerance = 1e-8)
  expect_equal(old_out$m,       new_out$m,       tolerance = 1e-8)
  expect_equal(old_out$u,       new_out$u,       tolerance = 1e-8)
  expect_equal(old_out$overlap, new_out$overlap, tolerance = 1e-8)
  expect_equal(old_out$pi,      new_out$pi,      tolerance = 1e-8)
})

test_that("fabl matches for different priors and bigger S", {
  set.seed(101)

  hash_mock <- list(
    ohe = matrix(c(1,0, 0,1, 1,1), nrow = 3, byrow = TRUE),  # 3 patterns x 2 features
    total_counts = c(5, 7, 4),
    hash_count_list = list(
      c(1,4,2),
      c(2,2,3),
      c(3,2,1),
      c(4,0,1)
    ),
    hash_to_file_1 = list(
      list("1"=1:2, "2"=3:5, "3"=6:7),
      list("1"=1,   "2"=2:3, "3"=4:5),
      list("1"=c(2,3), "2"=1:2, "3"=5:6),
      list("1"=1,   "2"=c(2,3,4), "3"=c(5,6,7))
    ),
    field_marker = c(1,2),
    n1 = 7,
    n2 = 4
  )

  # Larger S, different priors
  S <- 50
  burn <- 10

  set.seed(888)
  old_out <- fabl_original(
    hash_mock,
    m_prior = 2,
    u_prior = 2,
    alpha   = 2,
    beta    = 2,
    S       = S,
    burn    = burn,
    show_progress = FALSE
  )

  set.seed(888)
  new_out <- fabl(
    hash_mock,
    m_prior = 2,
    u_prior = 2,
    alpha   = 2,
    beta    = 2,
    S       = S,
    burn    = burn,
    show_progress = FALSE
  )

  expect_equal(old_out$Z,       new_out$Z,       tolerance = 1e-8)
  expect_equal(old_out$m,       new_out$m,       tolerance = 1e-8)
  expect_equal(old_out$u,       new_out$u,       tolerance = 1e-8)
  expect_equal(old_out$overlap, new_out$overlap, tolerance = 1e-8)
  expect_equal(old_out$pi,      new_out$pi,      tolerance = 1e-8)
})


test_that("fabl handles an empty scenario (if code allows)", {
  # Some codes can't handle empty or contradictory scenarios.
  # If original code fails, you might skip or remove this test.
  set.seed(9999)

  hash_mock <- list(
    ohe = matrix(0, nrow=0, ncol=2), # 0 patterns
    total_counts = numeric(0),
    hash_count_list = list(),
    hash_to_file_1 = list(),
    field_marker = c(1,1),
    n1 = 5,
    n2 = 0
  )

  # The old code can't handle n2=0, this might fail. Adjust accordingly.
  old_out <- fabl_original(hash_mock, S=10, burn=2, show_progress=FALSE)
  new_out <- fabl(hash_mock, S=10, burn=2, show_progress=FALSE)

  expect_equal(old_out$Z,       new_out$Z)
  expect_equal(old_out$m,       new_out$m)
  expect_equal(old_out$u,       new_out$u)
  expect_equal(old_out$overlap, new_out$overlap)
  expect_equal(old_out$pi,      new_out$pi)
})
