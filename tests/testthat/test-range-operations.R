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



# range_within -----------------------------------------------------------------

test_that("range_within works as expected", {

  expect_true(range_within(1, 2, 0, 3))
  expect_true(range_within(1, 1, 1, 1))
  expect_true(range_within(0, 1, 0, 1))
  expect_true(range_within(1, 1, 0, 2))
  expect_true(range_within(1, 2, 1, 3))
  expect_true(range_within(2, 3, 1, 3))
  expect_true(range_within(c(1, 5), c(2, 10), c(0, 4), c(3, 11)))
  expect_true(range_within(c(1, 5, 12), c(2, 9, 12), c(0, 4, 12), c(3, 11, 12)))
  expect_true(range_within(c(1, 5), c(1, 10), c(0, 4, 12), c(3, 11, 12)))
  expect_true(range_within(c(0, 4, 8), c(3, 7, 9), -10, 10))

  expect_false(range_within(5, 8, 6, 7))
  expect_false(range_within(5, 8, 5, 7))
  expect_false(range_within(5, 8, 6, 8))
  expect_false(range_within(0, 0, 1, 1))
  expect_false(range_within(0, 2, 1, 1))
  expect_false(range_within(10, 20, c(0, 15), c(12, 22)))
  expect_false(range_within(c(1, 5), c(2, 20), -10, 10))
  expect_false(range_within(c(0, 4), c(3, 11), c(1, 5), c(2, 10)))
  expect_false(range_within(c(0, 4, 12), c(3, 11, 12), c(1, 5), c(1, 10)))
  expect_false(range_within(-10, 10, c(0, 4, 8), c(3, 7, 9)))

})

# range_intersects -------------------------------------------------------------

test_that("range_intersects correctly identifies non-instant intersections", {

  # Within
  expect_true(range_intersects(-5, 5, -1, 1))
  expect_true(range_intersects(-1, 1, -5, 5))
  expect_true(range_intersects(3, 5, 3, 4))
  expect_true(range_intersects(3, 4, 3, 5))
  expect_true(range_intersects(3, 5, 4, 5))
  expect_true(range_intersects(4, 5, 3, 5))
  expect_true(range_intersects(c(0, 20), c(10, 30), c(1, 4, 21), c(2, 5, 29)))
  expect_true(range_intersects(c(1, 4, 21), c(2, 5, 29), c(0, 20), c(10, 30)))
  expect_true(range_intersects(0, 100, c(0, 20), c(10, 30)))
  expect_true(range_intersects(c(0, 20), c(10, 30), 0, 100))

  # Non-Aligned
  expect_true(range_intersects(1, 10, 5, 20))
  expect_true(range_intersects(5, 20, 1, 10))
  expect_true(range_intersects(c(0, 10), c(5, 20), c(1, 15, 50), c(2, 23, 60)))
  expect_true(range_intersects(c(1, 15, 50), c(2, 23, 60), c(0, 10), c(5, 20)))

})

test_that("range_intersects correctly identifies non-intersections", {

  expect_false(range_intersects(0, 0, 1, 1))
  expect_false(range_intersects(0, 10, 11, 20))
  expect_false(range_intersects(c(0, 5), c(1, 6), c(2, 12), c(3, 20)))

})

test_that("range_intersects `inclusive` argument works as expected", {

  x_starts <- 1
  x_ends <- 2
  y_starts <- c(0, 2)
  y_ends <- c(1, 3)

  expect_false(
    range_intersects(x_starts, x_ends, y_starts, y_ends, inclusive = FALSE)
  )
  expect_true(
    range_intersects(x_starts, x_ends, y_starts, y_ends, inclusive = TRUE)
  )
  expect_false(
    range_intersects(y_starts, y_ends, x_starts, x_ends, inclusive = FALSE)
  )
  expect_true(
    range_intersects(y_starts, y_ends, x_starts, x_ends, inclusive = TRUE)
  )

})

test_that("range_intersects instant x instant intersections are as expected", {

  expect_false(range_intersects(1, 1, 1, 1, inclusive = FALSE))
  expect_true(range_intersects(1, 1, 1, 1, inclusive = TRUE))

})

test_that("range_intersect instant x range edge intersections are as expected", {

  # instant x range start
  expect_false(range_intersects(0, 0, 0, 1, inclusive = FALSE))
  expect_true(range_intersects(0, 0, 0, 1, inclusive = TRUE))
  expect_false(range_intersects(0, 1, 1, 1, inclusive = FALSE))
  expect_true(range_intersects(0, 1, 0, 0, inclusive = TRUE))

  # instant x range end
  expect_false(range_intersects(1, 1, 0, 1, inclusive = FALSE))
  expect_true(range_intersects(1, 1, 0, 1, inclusive = TRUE))
  expect_false(range_intersects(0, 1, 1, 1, inclusive = FALSE))
  expect_true(range_intersects(0, 1, 1, 1, inclusive = TRUE))

})

test_that("range_intersects instant x range intersections are as expected", {

  expect_false(range_intersects(0, 0, -1, 1, inclusive = FALSE))
  expect_true(range_intersects(0, 0, -1, 1, inclusive = TRUE))
  expect_false(range_intersects(-1, 1, 0, 0, inclusive = FALSE))
  expect_true(range_intersects(-1, 1, 0, 0, inclusive = TRUE))

})

# range_intersect --------------------------------------------------------------

test_that("range_intersect correctly identifies non-instant intersections", {

  # Within
  expect_identical(range_intersect(-5, 5, -1, 1), list(starts = -1, ends = 1))
  expect_identical(range_intersect(-1, 1, -5, 5), list(starts = -1, ends = 1))
  expect_identical(range_intersect(3, 5, 3, 4), list(starts = 3, ends = 4))
  expect_identical(range_intersect(3, 4, 3, 5), list(starts = 3, ends = 4))
  expect_identical(range_intersect(3, 5, 4, 5), list(starts = 4, ends = 5))
  expect_identical(range_intersect(4, 5, 3, 5), list(starts = 4, ends = 5))
  expect_identical(
    range_intersect(c(0, 20), c(10, 30), c(1, 4, 21), c(2, 5, 29)),
    list(starts = c(1, 4, 21), ends = c(2, 5, 29))
  )
  expect_identical(
    range_intersect(c(1, 4, 21), c(2, 5, 29), c(0, 20), c(10, 30)),
    list(starts = c(1, 4, 21), ends = c(2, 5, 29))
  )
  expect_identical(
    range_intersect(0, 100, c(0, 20), c(10, 30)),
    list(starts = c(0, 20), ends = c(10, 30))
  )
  expect_identical(
    range_intersect(c(0, 20), c(10, 30), 0, 100),
    list(starts = c(0, 20), ends = c(10, 30))
  )

  # Non-Aligned
  expect_identical(range_intersect(1, 10, 5, 20), list(starts = 5, ends = 10))
  expect_identical(range_intersect(5, 20, 1, 10), list(starts = 5, ends = 10))
  expect_identical(
    range_intersect(c(0, 10), c(5, 20), c(1, 15, 100), c(2, 23, 110)),
    list(starts = c(1, 15), ends = c(2, 20))
  )
  expect_identical(
    range_intersect(c(1, 15, 100), c(2, 23, 110), c(0, 10), c(5, 20)),
    list(starts = c(1, 15), ends = c(2, 20))
  )

})

test_that("range_intersect returns empty numeric when no intersection", {

  x_starts <- c(1, 10)
  x_ends <- c(2, 11)
  y_starts <- c(3, 20)
  y_ends <- c(4, 22)

  out <- list(starts = numeric(), ends = numeric())
  expect_identical(
    range_intersect(x_starts, x_ends, y_starts, y_ends, inclusive = FALSE),
    out
  )
  expect_identical(
    range_intersect(x_starts, x_ends, y_starts, y_ends, inclusive = TRUE),
    out
  )
  expect_identical(
    range_intersect(y_starts, y_ends, x_starts, x_ends, inclusive = FALSE),
    out
  )
  expect_identical(
    range_intersect(y_starts, y_ends, x_starts, x_ends,  inclusive = TRUE),
    out
  )

})

test_that("range_intersect `inclusive` argument works as expected", {

  x_starts <- 1
  x_ends <- 2
  y_starts <- c(0, 2)
  y_ends <- c(1, 3)

  expect_identical(
    range_intersect(x_starts, x_ends, y_starts, y_ends, inclusive = FALSE),
    list(starts = numeric(), ends = numeric())
  )
  expect_identical(
    range_intersect(x_starts, x_ends, y_starts, y_ends, inclusive = TRUE),
    list(starts = c(1, 2), ends = c(1, 2))
  )
  expect_identical(
    range_intersect(y_starts, y_ends, x_starts, x_ends, inclusive = FALSE),
    list(starts = numeric(), ends = numeric())
  )
  expect_identical(
    range_intersect(y_starts, y_ends, x_starts, x_ends, inclusive = TRUE),
    list(starts = c(1, 2), ends = c(1, 2))
  )

})

test_that("range_intersect instant x instant intersections are as expected", {

  expect_identical(
    range_intersect(1, 1, 1, 1, inclusive = FALSE),
    list(starts = numeric(), ends = numeric())
  )
  expect_identical(
    range_intersect(1, 1, 1, 1, inclusive = TRUE),
    list(starts = 1, ends = 1)
  )

})

test_that("range_intersect instant x range edge intersections are as expected", {

  # instant x range start
  expect_identical(
    range_intersect(0, 0, 0, 1, inclusive = FALSE),
    list(starts = numeric(), ends = numeric())
  )
  expect_identical(
    range_intersect(0, 0, 0, 1, inclusive = TRUE),
    list(starts = 0, ends = 0)
  )
  expect_identical(
    range_intersect(0, 1, 1, 1, inclusive = FALSE),
    list(starts = numeric(), ends = numeric())
  )
  expect_identical(
    range_intersect(0, 1, 0, 0, inclusive = TRUE),
    list(starts = 0, ends = 0)
  )

  # instant x range end
  expect_identical(
    range_intersect(1, 1, 0, 1, inclusive = FALSE),
    list(starts = numeric(), ends = numeric())
  )
  expect_identical(
    range_intersect(1, 1, 0, 1, inclusive = TRUE),
    list(starts = 1, ends = 1)
  )
  expect_identical(
    range_intersect(0, 1, 1, 1, inclusive = FALSE),
    list(starts = numeric(), ends = numeric())
  )
  expect_identical(
    range_intersect(0, 1, 1, 1, inclusive = TRUE),
    list(starts = 1, ends = 1)
  )

})

test_that("range_intersect instant x range intersections are as expected", {

  expect_identical(
    range_intersect(0, 0, -1, 1, inclusive = FALSE),
    list(starts = numeric(), ends = numeric())
  )
  expect_identical(
    range_intersect(0, 0, -1, 1, inclusive = TRUE),
    list(starts = 0, ends = 0)
  )
  expect_identical(
    range_intersect(-1, 1, 0, 0, inclusive = FALSE),
    list(starts = numeric(), ends = numeric())
  )
  expect_identical(
    range_intersect(-1, 1, 0, 0, inclusive = TRUE),
    list(starts = 0, ends = 0)
  )

})
