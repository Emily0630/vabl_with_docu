# test_compare_records.R

library(testthat)
source("compare_records_original.R")
source("compare_records.R")
source("create_breaklist.R")
source("compute_string_distance.R")
source("one_hot_encode_factor.R")
# -------------------------------------------------------------------------
# 1. Minimal Example: Single Binary Field
# -------------------------------------------------------------------------
test_that("compare_records matches for a single binary field", {
  # Create sample data frames
  df1 <- data.frame(id = 1:3, fruit = c("apple", "banana", "apple"))
  df2 <- data.frame(id = 1:3, fruit = c("apple", "apple",  "banana"))

  # Original version of compare_records (assuming it's named compare_records_original)
  result_old <- compare_records_original(
    df1, df2, fields = "fruit",
    types  = "bi"
  )

  # Refactored version (assuming it's named compare_records_refactored)
  result_new <- compare_records(
    df1, df2, fields = "fruit",
    types  = "bi"
  )

  # Compare results
  expect_equal(result_old$comparisons, result_new$comparisons)
  expect_equal(result_old$n1,          result_new$n1)
  expect_equal(result_old$n2,          result_new$n2)
  expect_equal(result_old$nDisagLevs,  result_new$nDisagLevs)
})

# -------------------------------------------------------------------------
# 2. Single Field: String Distance (Levenshtein)
# -------------------------------------------------------------------------
test_that("compare_records matches for a single string field (Levenshtein)", {
  df1 <- data.frame(name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(name = c("Alice", "Bobo", "Charlene"))

  result_old <- compare_records_original(
    df1, df2,
    fields = "name",
    types  = "lv",           # string comparison
    breaks = c(0, 0.25, 0.5) # breakpoints for string distance
  )

  result_new <- compare_records(
    df1, df2,
    fields = "name",
    types  = "lv",
    breaks = c(0, 0.25, 0.5)
  )

  expect_equal(result_old$comparisons, result_new$comparisons)
  expect_equal(result_old$n1,          result_new$n1)
  expect_equal(result_old$n2,          result_new$n2)
  expect_equal(result_old$nDisagLevs,  result_new$nDisagLevs)
})

# -------------------------------------------------------------------------
# 3. Single Field: String Distance (Damerau-Levenshtein)
# -------------------------------------------------------------------------
test_that("compare_records matches for a single string field (Damerau-Levenshtein)", {
  df1 <- data.frame(name = c("Marry", "Jon", "Lisa"))
  df2 <- data.frame(name = c("Mary",  "John", "Lysa"))

  result_old <- compare_records_original(
    df1, df2,
    fields          = "name",
    types           = "lv",
    breaks          = c(0, 0.25, 0.5),
    distance_metric = "Damerau-Levenshtein"
  )

  result_new <- compare_records(
    df1, df2,
    fields          = "name",
    types           = "lv",
    breaks          = c(0, 0.25, 0.5),
    distance_metric = "Damerau-Levenshtein"
  )

  expect_equal(result_old$comparisons, result_new$comparisons)
  expect_equal(result_old$n1,          result_new$n1)
  expect_equal(result_old$n2,          result_new$n2)
  expect_equal(result_old$nDisagLevs,  result_new$nDisagLevs)
})

# -------------------------------------------------------------------------
# 4. Single Field: Numeric Distance
# -------------------------------------------------------------------------
test_that("compare_records matches for a single numeric field", {
  df1 <- data.frame(height = c(65.1, 70.2, 68.9))  # in inches
  df2 <- data.frame(height = c(65.1, 69.8, 70.5))

  # Custom breakpoints for numeric distances
  numeric_breaks <- c(0, 0.5, 1, 2)

  result_old <- compare_records_original(
    df1, df2,
    fields = "height",
    types  = "nu",
    breaks = numeric_breaks
  )

  result_new <- compare_records(
    df1, df2,
    fields = "height",
    types  = "nu",
    breaks = numeric_breaks
  )

  expect_equal(result_old$comparisons, result_new$comparisons)
  expect_equal(result_old$n1,          result_new$n1)
  expect_equal(result_old$n2,          result_new$n2)
  expect_equal(result_old$nDisagLevs,  result_new$nDisagLevs)
})

# -------------------------------------------------------------------------
# 5. Multiple Fields, Mixed Types
# -------------------------------------------------------------------------
test_that("compare_records matches for multiple fields with mixed types", {
  df1 <- data.frame(
    color  = c("red", "blue", "red"),
    size   = c(1.1, 2.0, 3.5),
    fruit  = c("apple", "banana", "cherry")
  )
  df2 <- data.frame(
    color  = c("red", "red", "blue"),
    size   = c(1.3, 2.1, 3.0),
    fruit  = c("apple", "berry", "cherry")
  )

  # Suppose we have:
  #   - color: binary check (bi)
  #   - size: numeric (nu)
  #   - fruit: string distance (lv)
  field_types  <- c("bi", "nu", "lv")
  field_breaks <- list(NA, c(0, 0.2, 0.5), c(0, 0.25, 0.75))
  #  ^ for color (bi) we use NA,
  #  ^ for size (nu) we specify numeric cutpoints,
  #  ^ for fruit (lv) we specify string cutpoints.

  result_old <- compare_records_original(
    df1, df2,
    fields = c("color", "size", "fruit"),
    types  = field_types,
    breaks = field_breaks
  )

  result_new <- compare_records(
    df1, df2,
    fields = c("color", "size", "fruit"),
    types  = field_types,
    breaks = field_breaks
  )

  expect_equal(result_old$comparisons, result_new$comparisons)
  expect_equal(result_old$n1,          result_new$n1)
  expect_equal(result_old$n2,          result_new$n2)
  expect_equal(result_old$nDisagLevs,  result_new$nDisagLevs)
})


# -------------------------------------------------------------------------
# 6. Checking Factor Levels Consistency
# -------------------------------------------------------------------------
test_that("compare_records yields same factor-level encoding for all fields", {
  df1 <- data.frame(v1 = c("A", "A", "B"), v2 = c(1, 2, 3))
  df2 <- data.frame(v1 = c("A", "B", "B"), v2 = c(1, 1, 3))

  # We'll do string for 'v1' and numeric for 'v2'
  field_types  <- c("lv", "nu")
  # Provide breakpoints for 'v1' and 'v2'
  # (though we only use them effectively for 'nu' and 'lv')
  field_breaks <- list(c(0, 0.5), c(0, 1))

  result_old <- compare_records_original(
    df1, df2,
    fields = c("v1", "v2"),
    types  = field_types,
    breaks = field_breaks
  )

  result_new <- compare_records(
    df1, df2,
    fields = c("v1", "v2"),
    types  = field_types,
    breaks = field_breaks
  )

  expect_equal(result_old$comparisons, result_new$comparisons)
  expect_equal(result_old$n1,          result_new$n1)
  expect_equal(result_old$n2,          result_new$n2)
  expect_equal(result_old$nDisagLevs,  result_new$nDisagLevs)
})
