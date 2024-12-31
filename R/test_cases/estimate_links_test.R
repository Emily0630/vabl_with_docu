# tests/testthat/test_estimate_links.R

library(testthat)
library(dplyr)
library(tidyr)

source("estimate_links_fabl.R")
source("estimate_links_vabl.R")
source("estimate_links_no_reject.R")
source("estimate_links_with_reject.R")
source("estimate_links_resolve_1to1.R")
source("estimate_links.R")
source("estimate_links_original.R")

test_that("estimate_links matches for FABL-like output with default parameters", {
  # 1) Create a mock 'out' that has $Z
  # Suppose n1=5, n2=4 => so Z is 4 x S (some sample draws)
  # We simulate a small scenario with a few posterior draws:
  fabl_out <- list(
    Z = matrix(c(
      1,1,5,6,  1,2,5,5,  1,5,5,5,  6,6,5,5  # 4 rows x 4 columns
    ), nrow=4, byrow=TRUE)
  )
  # Note: In real usage, you'd have S draws from FABL, but let's keep it small.

  hash_mock <- list(
    n1 = 5,
    n2 = 4
  )

  # 2) Call the old vs. new version
  old_out <- estimate_links_original(
    out = fabl_out,
    hash = hash_mock
    # all other defaults, e.g. l_FNM=1, l_FM1=1, l_FM2=2, etc.
  )

  new_out <- estimate_links(
    out = fabl_out,
    hash = hash_mock
  )

  # 3) Compare results
  # Usually you want to compare at least Z_hat and prob (the two returned elements).
  expect_equal(old_out$Z_hat, new_out$Z_hat, tolerance = 1e-8)
  expect_equal(old_out$prob,  new_out$prob,  tolerance = 1e-8)
})

test_that("estimate_links matches for VABL-like output with default parameters", {
  # 1) Create a mock 'out' that has $pattern_weights
  #   Suppose n2=3 => out$C is length 3, out$b_pi is a scalar, etc.
  vabl_out <- list(
    pattern_weights = c(0.3, 0.7),  # P=2, for example
    C               = c(1.0, 2.0, 1.5),
    b_pi            = 1.2
  )
  # Also, hash$flags might be used for possible records:
  hash_mock <- list(
    n1 = 5,
    n2 = 3,
    flags = list(
      list(
        eligible_records = c(1,2),
        eligible_patterns = c(1,2) # example
      ),
      list(
        eligible_records = c(3,4),
        eligible_patterns = c(1,1)
      ),
      list(
        eligible_records = c(2,5),
        eligible_patterns = c(2,2)
      )
    )
  )

  old_out <- estimate_links_original(
    out = vabl_out,
    hash = hash_mock
  )
  new_out <- estimate_links(
    out = vabl_out,
    hash = hash_mock
  )

  expect_equal(old_out$Z_hat, new_out$Z_hat, tolerance = 1e-8)
  expect_equal(old_out$prob,  new_out$prob,  tolerance = 1e-8)
})

test_that("estimate_links with reject option (l_R < Inf)", {
  # We'll do a FABL-like scenario but with l_R=2 for reject
  fabl_out <- list(
    Z = matrix(c(
      1,1,1,6,  2,2,5,5,
      6,5,5,5,  5,5,5,6
    ), nrow=4, byrow=TRUE)
  )
  hash_mock <- list(n1=5, n2=4)

  # old vs new
  old_out <- estimate_links_original(
    out = fabl_out, hash = hash_mock,
    l_R = 2
  )
  new_out <- estimate_links(
    out = fabl_out, hash = hash_mock,
    l_R = 2
  )

  # compare
  expect_equal(old_out$Z_hat, new_out$Z_hat, tolerance=1e-8)
  expect_equal(old_out$prob,  new_out$prob,  tolerance=1e-8)
})

test_that("estimate_links with nonmatch_label='n_1 + j' and resolve=FALSE", {
  # Another scenario. Suppose VABL-like output again:
  vabl_out <- list(
    pattern_weights = c(0.5, 0.5),
    C = c(1.0, 2.0),
    b_pi = 1.8
  )
  hash_mock <- list(
    n1 = 3,
    n2 = 2,
    flags = list(
      list(
        eligible_records = c(1),
        eligible_patterns = c(1)
      ),
      list(
        eligible_records = c(2,3),
        eligible_patterns = c(1,2)
      )
    )
  )

  old_out <- estimate_links_original(
    out = vabl_out,
    hash = hash_mock,
    nonmatch_label = "n_1 + j",
    resolve = FALSE
  )
  new_out <- estimate_links(
    out = vabl_out,
    hash = hash_mock,
    nonmatch_label = "n_1 + j",
    resolve = FALSE
  )

  expect_equal(old_out$Z_hat, new_out$Z_hat, tolerance=1e-8)
  expect_equal(old_out$prob,  new_out$prob,  tolerance=1e-8)
})
