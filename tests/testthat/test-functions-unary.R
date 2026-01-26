# unary dispatch ---------------------------------------------------------------

# TODO
test_that("Unary functions work on <Interval> and <phinterval> inputs", {

})

# unary functions --------------------------------------------------------------

# TODO
test_that("Unary functions empty input results in empty output", {

})

# TODO
test_that("Unary functions NA input results in NA output", {

})

# TODO
test_that("Unary functions error on invalid inputs", {

})

# phint_sift -------------------------------------------------------------------

test_that("phint_sift() empty input results in empty output", {
  expect_equal(phint_sift(interval()), phinterval())
  expect_equal(phint_sift(phinterval()), phinterval())
})

test_that("phint_sift() NA input results in NA output", {
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_)
  expect_equal(phint_sift(interval(NA, NA)), na_phint)
  expect_equal(phint_sift(na_phint), na_phint)
})

test_that("phint_sift() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:30", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:10:50", tz = "UTC")

  int12 <- interval(t1, t2)
  int34 <- interval(t3, t4)
  hole <- hole(tzone = "UTC")

  # No instants
  expect_equal(phint_sift(int12), as_phinterval(int12))
  expect_equal(
    phint_sift(c(int12, int34)),
    as_phinterval(c(int12, int34))
  )

  # With instants
  expect_equal(phint_sift(interval(t1, t1)), hole)
  expect_equal(
    phint_sift(phinterval(c(t1, t4), c(t1, t4), by = 1)),
    hole
  )
  expect_equal(
    phint_sift(phint_squash(interval(c(t1, t2, t3), c(t1, t2, t4)))),
    as_phinterval(int34)
  )
})

# phint_complement -------------------------------------------------------------

test_that("phint_complement() empty input results in empty output", {
  expect_identical(phint_complement(interval()), phinterval())
  expect_identical(phint_complement(phinterval()), phinterval())
})

test_that("phint_complement() NA input results in NA output", {
  na_int <- interval(NA, NA)
  int <- interval(as.Date("2021-01-01"), as.Date("2022-01-01"))
  hole <- phinterval(interval())

  expect_equal(
    is.na(phint_complement(c(hole, int, na_int))),
    c(FALSE, FALSE, TRUE)
  )
})

test_that("phint_complement() works as expected", {
  origin <- lubridate::origin
  t1 <- origin
  t2 <- origin + 100
  t3 <- origin + 250
  t4 <- origin + 700

  int12 <- interval(t1, t2)
  int23 <- interval(t2, t3)
  int34 <- interval(t3, t4)
  hole <- phinterval(interval())

  # FLAG: <Interval> vectors cannot have -Inf as their start. As a workaround,
  # manually constructing the correct <phinterval>.
  manual_phinterval <- function(starts, ends, tzone = "UTC") {
    new_phinterval(matrix(c(starts, ends), ncol = 2L), tzone = tzone)
  }

  # Input with finite endpoints
  expect_equal(
    phint_complement(int23),
    manual_phinterval(
      starts = c(-Inf, as.numeric(t3)),
      ends = c(as.numeric(t2), Inf)
    )
  )
  expect_equal(
    phint_complement(phint_squash(c(int12, int34))),
    manual_phinterval(
      starts = c(-Inf, as.numeric(t2), as.numeric(t4)),
      ends = c(as.numeric(t1), as.numeric(t3), Inf)
    )
  )

  # Input with one infinite endpoint
  expect_equal(
    phint_complement(manual_phinterval(as.numeric(t1), Inf)),
    manual_phinterval(-Inf, as.numeric(t1))
  )
  expect_equal(
    phint_complement(manual_phinterval(-Inf, as.numeric(t1))),
    manual_phinterval(as.numeric(t1), Inf)
  )

  # Holes and infinite intervals
  expect_equal(phint_complement(hole), manual_phinterval(-Inf, Inf))
  expect_equal(phint_complement(manual_phinterval(-Inf, Inf)), hole)
})

# phint_invert -----------------------------------------------------------------

test_that("phint_invert() empty input results in empty output", {
  expect_equal(phint_invert(interval()), phinterval())
  expect_equal(phint_invert(phinterval()), phinterval())
})

test_that("phint_invert() NA input results in NA output", {
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_)
  expect_equal(phint_invert(interval(NA, NA)), na_phint)
  expect_equal(phint_invert(na_phint), na_phint)
})

test_that("phint_invert() errors on invalid inputs", {
  expect_error(phint_invert(10))
  expect_error(phint_invert(interval(NA, NA), hole_to = "span"))
})

test_that("phint_invert() works as expected", {
  origin <- lubridate::origin
  t1 <- origin
  t2 <- origin + 100
  t3 <- origin + 250
  t4 <- origin + 700
  t5 <- origin + 900
  t6 <- origin + 1200

  int11 <- interval(t1, t1)
  int12 <- interval(t1, t2)
  int23 <- interval(t2, t3)
  int34 <- interval(t3, t4)
  int44 <- interval(t4, t4)
  hole <- hole(tzone = "UTC")
  inf_phint <- phinterval(as.POSIXct(-Inf), as.POSIXct(Inf), tzone = "UTC")

  # Single spans
  expect_equal(phint_invert(c(int23, int34, int11)), rep(hole, 3))
  expect_equal(phint_invert(as_phinterval(c(int23, int34, int11))), rep(hole, 3))

  # Holey intervals
  expect_equal(phint_invert(phint_squash(c(int12, int34))), phinterval(t2, t3))
  expect_equal(phint_invert(phint_squash(c(int11, int34))), phinterval(t1, t3))
  expect_equal(phint_invert(phint_squash(c(int23, int44))), phinterval(t3, t4))
  expect_equal(
    phint_invert(phint_squash(c(int12, int34, interval(t5, t6)))),
    phint_squash(c(int23, interval(t4, t5)))
  )

  # Holes
  expect_equal(phint_invert(hole), hole)
  expect_equal(phint_invert(hole, hole_to = "hole"), hole)
  expect_equal(phint_invert(hole, hole_to = "na"), phinterval(NA_POSIXct_, NA_POSIXct_))
  expect_equal(phint_invert(hole, hole_to = "inf"), inf_phint)
})
