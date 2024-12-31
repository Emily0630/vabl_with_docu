# tests/testthat/test_svabl.R

library(testthat)
library(dplyr)
library(tidyr)

source("svabl_original.R")
source("svabl.R")
source("svabl_setup.R")
source("svabl_compute_chunks.R")
source("svabl_compute_m_u_p.R")
source("svabl_compute_batch_counts.R")
source("svabl_compute_K.R")
source("svabl_update_ab.R")
source("svabl_compute_elbo.R")

test_that("svabl matches for a small scenario with default parameters", {
  # 1) Create a minimal 'hash' object
  #    Typically: hash$ohe => P x L, hash$total_counts => length P
  #              hash$hash_count_list => length n2
  #              hash$field_marker => length L
  #              hash$n1, hash$n2 => integers
  hash_mock <- list(
    ohe = matrix(c(1,0, 0,1), nrow=2, byrow=TRUE),  # 2 patterns x 2 features
    total_counts = c(5, 3),
    hash_count_list = list(
      c(2,3),
      c(1,2),
      c(4,1)
    ),
    field_marker = c(1,1),
    n1 = 10,
    n2 = 3
  )

  # 2) Set parameters for test
  threshold_val <- 1e-6
  tmax_val <- 50
  B_val <- 2   # minibatch size
  k_val <- 1
  tau_val <- 1
  seed_val <- 123

  # 3) Run old vs. new version
  set.seed(999)  # Make sure the entire environment is consistent
  old_out <- svabl_original(
    hash = hash_mock,
    threshold = threshold_val,
    tmax = tmax_val,
    b_init = TRUE,
    B = B_val,
    k = k_val,
    tau = tau_val,
    seed = seed_val,
    check_every = 5,
    store_every = 5
  )

  set.seed(999)  # same environment
  new_out <- svabl(
    hash = hash_mock,
    threshold = threshold_val,
    tmax = tmax_val,
    b_init = TRUE,
    B = B_val,
    k = k_val,
    tau = tau_val,
    seed = seed_val,
    check_every = 5,
    store_every = 5
  )

  # 4) Compare results
  #    Because there's randomness, we may see small differences.
  #    If you want exact matches, ensure random calls are identical.
  #    Otherwise, compare with a tolerance.
  expect_equal(old_out$pattern_weights, new_out$pattern_weights, tolerance=1e-6)
  expect_equal(old_out$a,               new_out$a,               tolerance=1e-6)
  expect_equal(old_out$b,               new_out$b,               tolerance=1e-6)
  expect_equal(old_out$a_pi,            new_out$a_pi,            tolerance=1e-6)
  expect_equal(old_out$b_pi,            new_out$b_pi,            tolerance=1e-6)
  expect_equal(old_out$C,               new_out$C,               tolerance=1e-6)
  # The elbo_seq might differ in the final iteration or random calls, but let's try:
  expect_equal(old_out$elbo_seq,        new_out$elbo_seq,        tolerance=1e-6)
  expect_equal(old_out$t,               new_out$t)
})

test_that("svabl can run with a fixed number of iterations", {
  hash_mock <- list(
    ohe = matrix(c(1,0,0,1,1,1), nrow=3, byrow=TRUE),
    total_counts = c(3,2,4),
    hash_count_list = list(c(1,2,0), c(2,0,1), c(0,1,2)),
    field_marker = c(1,2),
    n1 = 10,
    n2 = 3
  )

  # We'll do a small fixed_iterations
  fix_iter <- 3
  B_val    <- 2

  # old code
  set.seed(888)
  old_out <- svabl_original(
    hash = hash_mock,
    fixed_iterations = fix_iter,
    B = B_val,
    seed = 888
  )

  # new code
  set.seed(888)
  new_out <- svabl(
    hash = hash_mock,
    fixed_iterations = fix_iter,
    B = B_val,
    seed = 888
  )

  # Check iteration counts
  expect_equal(old_out$t, new_out$t)

  # Compare main parameters
  expect_equal(old_out$pattern_weights, new_out$pattern_weights, tolerance=1e-5)
  expect_equal(old_out$a,               new_out$a,               tolerance=1e-5)
  expect_equal(old_out$b,               new_out$b,               tolerance=1e-5)
  expect_equal(old_out$C,               new_out$C,               tolerance=1e-5)
  expect_equal(old_out$a_pi,            new_out$a_pi,            tolerance=1e-5)
  expect_equal(old_out$b_pi,            new_out$b_pi,            tolerance=1e-5)
  expect_equal(old_out$elbo_seq,        new_out$elbo_seq,        tolerance=1e-5)
})

test_that("svabl can handle a larger batch scenario", {
  # Suppose we do a scenario where B ~ n2 => full-batch
  hash_mock <- list(
    ohe = matrix(c(1,1,0,1), nrow=2, byrow=TRUE),
    total_counts = c(2,4),
    hash_count_list = list(
      c(2,0),
      c(0,2),
      c(1,1),
      c(2,2)
    ),
    field_marker = c(1,1),
    n1 = 5,
    n2 = 4
  )

  # B=4 => effectively using the full data each iteration
  B_val <- 4

  # old code
  set.seed(42)
  old_out <- svabl_original(
    hash = hash_mock,
    B = B_val,
    tmax = 10,
    seed = 42
  )

  # new code
  set.seed(42)
  new_out <- svabl(
    hash = hash_mock,
    B = B_val,
    tmax = 10,
    seed = 42
  )

  expect_equal(old_out$t, new_out$t)
  expect_equal(old_out$pattern_weights, new_out$pattern_weights, tolerance=1e-6)
  expect_equal(old_out$a,               new_out$a,               tolerance=1e-6)
  expect_equal(old_out$b,               new_out$b,               tolerance=1e-6)
  expect_equal(old_out$C,               new_out$C,               tolerance=1e-6)
  expect_equal(old_out$elbo_seq,        new_out$elbo_seq,        tolerance=1e-6)
})
