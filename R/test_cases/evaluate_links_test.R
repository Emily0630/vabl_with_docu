# tests/testthat/test_evaluate_links.R

library(testthat)

source("evaluate_links_original.R")
source("evaluate_links.R")

test_that("evaluate_links matches on simple scenario with no predicted matches", {
  # 1) Suppose n1=5
  #    We predict no matches => estimated_links[] = 0
  #    Suppose the true_links has 2 matches => positions j=1..2 => link1..2
  #    Then recall=? precision=? fmeasure=?

  estimated_links <- c(0, 0, 0, 0, 0)
  true_links      <- c(1, 2, 0, 0, 0)
  n1             <- 5

  old_vals <- evaluate_links_original(estimated_links, true_links, n1)
  new_vals <- evaluate_links(estimated_links, true_links, n1)

  # Compare
  expect_equal(old_vals, new_vals, tolerance=1e-8)
})

test_that("evaluate_links matches on scenario with perfect match", {
  # 2) Suppose n1=5
  #    We have estimated_links = c(1,2,3,0) => 3 predicted matches, 1 non-match
  #    Suppose true_links = same => c(1,2,3,0)
  #    Then recall=1, precision=1 => fmeasure=1
  estimated_links <- c(1, 2, 3, 0)
  true_links      <- c(1, 2, 3, 0)
  n1 <- 5

  old_vals <- evaluate_links_original(estimated_links, true_links, n1)
  new_vals <- evaluate_links(estimated_links, true_links, n1)

  expect_equal(old_vals, new_vals, tolerance=1e-8)
})

test_that("evaluate_links matches on partially correct scenario", {
  # 3) Suppose n1=3
  #    estimated_links = c(1,1,2, 0) => 3 predictions of link(1..2) and 1 non-match
  #    true_links      = c(1,2,0, 3) => actual links are (1,2,...)
  #
  # We'll see partial recall / precision
  estimated_links <- c(1,1,2,0)
  true_links      <- c(1,2,0,3)
  n1 <- 3

  old_vals <- evaluate_links_original(estimated_links, true_links, n1)
  new_vals <- evaluate_links(estimated_links, true_links, n1)

  expect_equal(old_vals, new_vals, tolerance=1e-8)
})

test_that("evaluate_links matches scenario with some ties or out-of-range predictions", {
  # 4) Suppose n1=4
  #    Some predicted links are out-of-range => e.g., 5 => means we interpret as no link
  #    Then the function only counts links <= n1 & > 0
  estimated_links <- c(5, 4, 0, 4)
  true_links      <- c(4, 4, 4, 0)
  n1 <- 4

  # In the code, only positions with predicted in (1..4) are counted as matched
  # => so we have predicted links at j=2 => 4, j=4 => 4
  # etc.
  old_vals <- evaluate_links_original(estimated_links, true_links, n1)
  new_vals <- evaluate_links(estimated_links, true_links, n1)

  expect_equal(old_vals, new_vals, tolerance=1e-8)
})
