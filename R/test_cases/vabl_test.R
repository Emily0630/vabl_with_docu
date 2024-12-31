# tests/testthat/test_vabl.R

library(testthat)
source("vabl_original.R")
source("vabl.R")
source("vabl_compute_elbo.R")
source("vabl_compute_m_u_p.R")
source("vabl_setup.R")
source("vabl_compute_phi_C.R")
source("vabl_compute_ABZ.R")
source("vabl_compute_chunks.R")

test_that("vabl matches for a small scenario with default parameters", {
  hash_mock <- list(
    ohe = matrix(c(1,0, 0,1), nrow = 2, byrow = TRUE),
    total_counts = c(5, 3),
    # hash_count_list can be smaller or bigger, depending on n2
    hash_count_list = list(
      c(2,3),
      c(1,2),
      c(4,1)
    ),
    field_marker = c(1,1),
    n1 = 10,
    n2 = 3
  )

  tmax_val <- 50
  threshold_val <- 1e-6

  # old version
  old_out <- vabl_original(hash_mock, threshold = threshold_val, tmax = tmax_val, b_init = TRUE)
  # new version
  new_out <- vabl(hash_mock, threshold = threshold_val, tmax = tmax_val, b_init = TRUE)

  # Compare
  expect_equal(old_out$pattern_weights, new_out$pattern_weights, tolerance = 1e-6)
  expect_equal(old_out$C,              new_out$C,              tolerance = 1e-6)
  expect_equal(old_out$a,              new_out$a,              tolerance = 1e-6)
  expect_equal(old_out$b,              new_out$b,              tolerance = 1e-6)
  expect_equal(old_out$a_pi,           new_out$a_pi,           tolerance = 1e-6)
  expect_equal(old_out$b_pi,           new_out$b_pi,           tolerance = 1e-6)
  # elbo_seq might differ if the old code used a different formula or stored it at different times.
  # If you do want them identical, check with tolerance or skip if logic changed.
  expect_equal(old_out$elbo_seq,       new_out$elbo_seq,       tolerance = 1e-6)
  expect_equal(old_out$t,              new_out$t)
})

test_that("vabl matches with b_init=FALSE, minimal data", {
  hash_mock <- list(
    ohe = matrix(c(1,1, 0,1), nrow=2, byrow=TRUE),
    total_counts = c(2, 4),
    hash_count_list = list(
      c(1,1),
      c(2,2)
    ),
    field_marker = c(1,1),
    n1 = 5,
    n2 = 2
  )

  old_out <- vabl_original(
    hash_mock,
    threshold = 1e-5,
    tmax = 30,
    b_init = FALSE
  )

  new_out <- vabl(
    hash_mock,
    threshold = 1e-5,
    tmax = 30,
    b_init = FALSE
  )

  expect_equal(old_out$pattern_weights, new_out$pattern_weights, tolerance=1e-6)
  expect_equal(old_out$a, new_out$a, tolerance=1e-6)
  expect_equal(old_out$b, new_out$b, tolerance=1e-6)
  expect_equal(old_out$a_pi, new_out$a_pi, tolerance=1e-6)
  expect_equal(old_out$b_pi, new_out$b_pi, tolerance=1e-6)
  expect_equal(old_out$elbo_seq, new_out$elbo_seq, tolerance=1e-6)
  expect_equal(old_out$t, new_out$t)
})

test_that("vabl can run with fixed_iterations=5, ignoring threshold", {
  hash_mock <- list(
    ohe = matrix(c(1,0,0,1,1,1), nrow=3, byrow=TRUE),
    total_counts = c(3,2,4),
    hash_count_list = list(c(1,2,0), c(2,0,1)),
    field_marker = c(1,2),
    n1 = 10,
    n2 = 2
  )

  old_out <- vabl_original(
    hash_mock,
    threshold=1e-6,
    tmax=100,
    fixed_iterations=5
  )
  new_out <- vabl(
    hash_mock,
    threshold=1e-6,
    tmax=100,
    fixed_iterations=5
  )

  expect_equal(old_out$t, new_out$t)
  # Compare main parameters
  expect_equal(old_out$pattern_weights, new_out$pattern_weights, tolerance=1e-6)
  expect_equal(old_out$a, new_out$a, tolerance=1e-6)
  expect_equal(old_out$b, new_out$b, tolerance=1e-6)
  expect_equal(old_out$a_pi, new_out$a_pi, tolerance=1e-6)
  expect_equal(old_out$b_pi, new_out$b_pi, tolerance=1e-6)
  expect_equal(old_out$elbo_seq, new_out$elbo_seq, tolerance=1e-6)
})

test_that("vabl can run with fixed_iterations, no threshold check", {
  hash_mock <- list(
    ohe = matrix(c(1,0,0,1,1,1), nrow=3, byrow=TRUE),
    total_counts = c(3,2,4),
    hash_count_list = list(c(1,2,0), c(2,0,1)),
    field_marker = c(1,2),
    n1 = 10,
    n2 = 2
  )

  old_out <- vabl_original(
    hash_mock,
    threshold = 1e-6,
    tmax = 100,
    fixed_iterations = 5  # run exactly 5
  )

  new_out <- vabl(
    hash_mock,
    threshold = 1e-6,
    tmax = 100,
    fixed_iterations = 5
  )

  # They should do exactly 5 steps, ignoring threshold:
  expect_equal(old_out$t, new_out$t)
  expect_equal(old_out$pattern_weights, new_out$pattern_weights, tolerance=1e-8)
  expect_equal(old_out$a, new_out$a, tolerance=1e-8)
  expect_equal(old_out$b, new_out$b, tolerance=1e-8)
  expect_equal(old_out$a_pi, new_out$a_pi, tolerance=1e-8)
  expect_equal(old_out$b_pi, new_out$b_pi, tolerance=1e-8)
  expect_equal(old_out$elbo_seq, new_out$elbo_seq, tolerance=1e-8)
})

