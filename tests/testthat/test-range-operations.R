# range_flatten ----------------------------------------------------------------

test_that("flattens adjacent ranges", {
  starts <- c(1, 2)
  ends <- c(2, 3)
  expected_out <- list(starts = c(1), ends = c(3))
  expect_identical(
    range_flatten(starts, ends),
    expected_out
  )
})

test_that("flattens overlapping ranges", {
  starts <- c(0, 2, 7, 10)
  ends <- c(2, 3, 11, 20)
  expected_out <- list(starts = sort(c(0, 7)), ends = sort(c(3, 20)))
  expect_identical(
    range_flatten(starts, ends),
    expected_out
  )
})

test_that("flattens nested ranges", {
  starts <- c(1, 2, 3, 4, 5)
  ends <- c(100, 10, 9, 8, 5)
  expected_out <- list(starts = c(1), ends = c(100))
  expect_identical(
    range_flatten(starts, ends),
    expected_out
  )
})

test_that("does not flatten non-overlapping, non-adjacent ranges", {
  starts <- c(0, 1, 10, 50, 1000)
  ends <- c(0, 1, 20, 70, 2000)
  expected_out <- list(starts = sort(starts), ends = sort(ends))
  expect_identical(
    range_flatten(starts, ends),
    expected_out
  )
})

test_that("combines identical instants", {
  starts <- c(0, 0)
  ends <- c(0, 0)
  expected_out <- list(starts = c(0), ends = c(0))
  expect_identical(
    range_flatten(starts, ends),
    expected_out
  )
})

test_that("works as expected with negative valued ranges", {
  # Adjacent
  starts <- c(-2, -3)
  ends <- c(-1, -2)
  expected_out <- list(starts = c(-3), ends = c(-1))
  expect_identical(
    range_flatten(starts, ends),
    expected_out
  )
  # Overlapping
  starts <- c(-2, -3, -11, -20, 60, 80)
  ends <- c(-1, -2, -7, -10, 81, 90)
  expected_out <- list(starts = sort(c(-20, -3, 60)), ends = sort(c(-7, -1, 90)))
  expect_identical(
    range_flatten(starts, ends),
    expected_out
  )
  # Nested
  starts <- c(-100, -10, -9, -8, 10)
  ends <- c(100, -2, -3, -4, 20)
  expected_out <- list(starts = c(-100), ends = c(100))
  expect_identical(
    range_flatten(starts, ends),
    expected_out
  )
  # Non-Overlapping, Non-Adjacent
  starts <- c(-0, -1, -20, -70, -2000, 1)
  ends <- c(-0, -1, -10, -50, -1000, 10)
  expected_out <- list(starts = sort(starts), ends = sort(ends))
  expect_identical(
    range_flatten(starts, ends),
    expected_out
  )
  # Instants
  starts <- c(-10, -10)
  ends <- c(-10, -10)
  expected_out <- list(starts = c(-10), ends = c(-10))
  expect_identical(
    range_flatten(starts, ends),
    expected_out
  )
})

test_that("returns empty range on empty input.", {
  starts <- numeric()
  ends <- numeric()
  expected_out <- list(starts = starts, ends = ends)
  expect_identical(
    range_flatten(starts, ends),
    expected_out
  )
})

test_that("handles length-1 inputs as expected.", {
  starts <- 1
  ends <- 2
  expected_out <- list(starts = starts, ends = ends)
  expect_identical(
    range_flatten(starts, ends),
    expected_out
  )
})

# range_is_flat ----------------------------------------------------------------
