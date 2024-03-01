# TODO Ethan:
# - take a look at the lubridate::interval tests, they're very clean and descriptive
#   https://github.com/tidyverse/lubridate/blob/main/tests/testthat/test-intervals.R
#   https://github.com/tidyverse/lubridate/blob/main/tests/testthat/test-vctrs.R

# TODO:
# Create a test helper that quickly creates phintervals or intervals which
# start at the origin (1970-01-01) and have UTC timezone.
# - during tests, you can shift these using int/phint shift to vary things up

# TODO Ethan:
# Test that `phinterval` and `Interval` behave the same (when exposed to the user)
# by checking the output of `phint_*` and matching `int_*` functions on an
# `int` Interval and the phinterval `phint = as_phinterval(int)`.
#
# Ex. int_end(int) and phint_end(phint) should be identical. Calls to `%within%`
# should also behave the same.

# phinterval -------------------------------------------------------------------

test_that("NA inputs result in NA output", {

  NA_POSIXct_ <- lubridate::NA_POSIXct_
  starts1 <- lubridate::ymd(20200101) + lubridate::ddays(c(0, 2, 4))
  ends1   <- lubridate::ymd(20200101) + lubridate::ddays(c(1, 3, NA_real_))
  starts2 <- NA_POSIXct_
  ends2   <- NA_POSIXct_
  starts3 <- lubridate::ymd(20111111)
  ends3   <- lubridate::ymd(20111111) + lubridate::dseconds(60)

  int1 <- lubridate::interval(starts1, ends1, tzone = "UTC")
  int2 <- lubridate::interval(starts2, ends2, tzone = "UTC")
  int3 <- lubridate::interval(starts3, ends3, tzone = "UTC")

  phint <- phinterval(intervals = list(int1, int2, int3), tzone = "UTC")
  expect_identical(
    phinterval(intervals = list(int1, int2, int3), tzone = "UTC"),
    new_phinterval(
      reference_time = c(NA_POSIXct_, NA_POSIXct_, starts3),
      range_starts = list(NA_real_, NA_real_, 0),
      range_ends = list(NA_real_, NA_real_, 60),
      tzone = "UTC"
    )
  )

})

# as_phinterval ----------------------------------------------------------------

test_that("NA inputs result in NA output", {

  NA_POSIXct_ <- lubridate::NA_POSIXct_

  starts <- lubridate::ymd(20200101) + lubridate::ddays(c(0, NA_real_, 4, NA_real_))
  ends   <- lubridate::ymd(20200101) + lubridate::ddays(c(1, 3, NA_real_, NA_real_))
  int <- lubridate::interval(starts, ends)

  expect_identical(
    as_phinterval(int, tzone = "UTC"),
    new_phinterval(
      reference_time = c(lubridate::ymd(20200101, tz = "UTC"), NA_POSIXct_, NA_POSIXct_, NA_POSIXct_),
      range_starts = list(0, NA_real_, NA_real_, NA_real_),
      range_ends = list(24*60*60, NA_real_, NA_real_, NA_real_),
      tzone = "UTC"
    )
  )

})

# phint_start ------------------------------------------------------------------

test_that("starts are correct", {

  ymd      <- function(x, ...) lubridate::ymd(x, ...)
  ddays    <- function(x, ...) lubridate::ddays(x, ...)
  dseconds <- function(x, ...) lubridate::dseconds(x, ...)

  starts1 <- lubridate::ymd(20200101) + ddays(c(0, 2, 4)) + dseconds(c(0, 1201, 2403))
  ends1   <- lubridate::ymd(20200101) + ddays(c(1, 3, 5)) + dseconds(c(82, 709, 8999))
  int1 <- lubridate::interval(starts1, ends1)

  starts2 <- lubridate::ymd(20240809) + ddays(c(10, 0))
  ends2   <- lubridate::ymd(20240809) + ddays(c(20, 5))
  int2 <- lubridate::interval(starts2, ends2)

  phint <- phinterval(intervals = list(int1, int2), tzone = "UTC")
  expect_identical(
    phint_start(phint),
    c(lubridate::ymd(20200101, tz = "UTC"), lubridate::ymd(20240809, tz = "UTC"))
  )
})

test_that("NA input results in NA output", {

  int1 <- lubridate::interval(NA_POSIXct_, NA_POSIXct_)

  starts2 <- lubridate::ymd(20240809) + lubridate::ddays(c(10, 0))
  ends2   <- lubridate::ymd(20240809) + lubridate::ddays(c(20, 5))
  int2 <- lubridate::interval(starts2, ends2)

  phint <- phinterval(intervals = list(int1, int2), tzone = "UTC")
  expect_identical(
    phint_start(phint),
    c(NA_POSIXct_, lubridate::ymd(20240809, tz = "UTC"))
  )

  phint <- as_phinterval(int1, tzone = "UTC")
  expect_identical(
    phint_start(phint),
    NA_POSIXct_
  )
})

# phint_invert -----------------------------------------------------------------

test_that("holes are correct", {

  starts1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 120, 240))
  ends1   <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 4000))
  int1 <- lubridate::interval(starts1, ends1)

  starts2 <- lubridate::ymd(20211011) + lubridate::dminutes(c(0, 10))
  ends2   <- lubridate::ymd(20211011) + lubridate::dminutes(c(5, 20))
  int2 <- lubridate::interval(starts2, ends2)

  phint <- phinterval(intervals = list(int1, int2), tzone = "UTC")
  expect_identical(
    phint_invert(phint),
    new_phinterval(
      reference_time = as.POSIXct(lubridate::ymd(c(20000101, 20211011), tz = "UTC")),
      range_starts = list(c(60, 180), c(60*5)),
      range_ends = list(c(120, 240), c(60*10)),
      tzone = "UTC"
    )
  )

})

test_that("NA inputs result in NA output", {

  NA_POSIXct_ <- lubridate::NA_POSIXct_
  starts1 <- NA_POSIXct_
  ends1   <- NA_POSIXct_
  starts2 <- lubridate::ymd(20111111) + lubridate::dseconds(c(0, 60))
  ends2   <- lubridate::ymd(20111111) + lubridate::dseconds(c(30, 120))

  int1 <- lubridate::interval(starts1, ends1, tzone = "UTC")
  int2 <- lubridate::interval(starts2, ends2, tzone = "UTC")

  phint <- phinterval(intervals = list(int1, int2), tzone = "UTC")
  expect_identical(
    phint_invert(phint),
    new_phinterval(
      reference_time = c(NA_POSIXct_, lubridate::ymd(20111111, tz = "UTC")),
      range_starts = list(NA_real_, 30),
      range_ends = list(NA_real_, 60),
      tzone = "UTC"
    )
  )

})

test_that("empty input results in empty phinterval output", {

  expect_identical(
    phint_invert(phinterval()),
    phinterval()
  )

})

test_that("non-holey phinterval inputs result in NA outputs", {

  starts1 <- lubridate::ymd(20000101)
  ends1   <- lubridate::ymd(20000102)
  starts2 <- lubridate::ymd(20111111) + lubridate::dseconds(c(0, 60))
  ends2   <- lubridate::ymd(20111111) + lubridate::dseconds(c(30, 120))

  int1 <- lubridate::interval(starts1, ends1, tzone = "UTC")
  int2 <- lubridate::interval(starts2, ends2, tzone = "UTC")

  phint <- phinterval(intervals = list(int1, int2), tzone = "UTC")
  expect_identical(
    phint_invert(phint),
    new_phinterval(
      reference_time = c(NA_POSIXct_, lubridate::ymd(20111111, tz = "UTC")),
      range_starts = list(NA_real_, 30),
      range_ends = list(NA_real_, 60),
      tzone = "UTC"
    )
  )

})

# is_holey ---------------------------------------------------------------------

test_that("holey phintervals correctly identified", {

  starts1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 120, 240))
  ends1   <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 4000))
  int1 <- lubridate::interval(starts1, ends1)

  starts2 <- lubridate::ymd(20211011) + lubridate::dminutes(c(0, 10))
  ends2   <- lubridate::ymd(20211011) + lubridate::dminutes(c(5, 20))
  int2 <- lubridate::interval(starts2, ends2)

  int3 <- lubridate::interval(lubridate::ymd(20200101), lubridate::ymd(20200201))

  phint <- phinterval(intervals = list(int1, int2, int3), tzone = "UTC")
  expect_identical(
    is_holey(phint),
    c(TRUE, TRUE, FALSE)
  )

})

test_that("intervals have no holes", {

  starts <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 240))
  ends   <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 4000))
  int <- lubridate::interval(starts, ends)

  expect_identical(
    is_holey(int),
    c(FALSE, FALSE)
  )

})

test_that("NA inputs result in NA outputs", {

  ## `interval` input
  starts <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, NA_real_))
  ends   <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 4000))
  int <- lubridate::interval(starts, ends)

  expect_identical(
    is_holey(int),
    c(FALSE, NA)
  )

  ## `phinterval` input
  starts1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 120, 240))
  ends1   <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 4000))
  int1 <- lubridate::interval(starts1, ends1)

  starts2 <- lubridate::ymd(20211011) + lubridate::dminutes(c(NA_real_, 10))
  ends2   <- lubridate::ymd(20211011) + lubridate::dminutes(c(5, 20))
  int2 <- lubridate::interval(starts2, ends2)

  int3 <- lubridate::interval(lubridate::ymd(20200101), lubridate::ymd(20200201))

  phint <- phinterval(intervals = list(int1, int2, int3), tzone = "UTC")
  expect_identical(
    is_holey(phint),
    c(TRUE, NA, FALSE)
  )

})

test_that("empty inputs result in empty logical outputs", {

  expect_identical(
    is_holey(phinterval()),
    logical()
  )

})

# n_spans ----------------------------------------------------------------------

test_that("phintervals have correct span count", {

  starts1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 120, 240))
  ends1   <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 4000))
  int1 <- lubridate::interval(starts1, ends1)

  starts2 <- lubridate::ymd(20211011) + lubridate::dminutes(c(0, 10))
  ends2   <- lubridate::ymd(20211011) + lubridate::dminutes(c(5, 20))
  int2 <- lubridate::interval(starts2, ends2)

  int3 <- lubridate::interval(lubridate::ymd(20200101), lubridate::ymd(20200201))

  phint <- phinterval(intervals = list(int1, int2, int3), tzone = "UTC")
  expect_identical(
    n_spans(phint),
    c(3L, 2L, 1L)
  )

})

test_that("intervals have 1L span", {

  starts <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 120, 240))
  ends   <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 4000))
  int <- lubridate::interval(starts, ends)

  phint <- as_phinterval(int, tzone = "UTC")
  expect_identical(
    n_spans(int),
    rep(1L, length(int))
  )

})

test_that("NA inputs result in NA outputs", {

  starts1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 120, 240, 400))
  ends1   <- lubridate::ymd(20000101) + lubridate::dseconds(c(6, 180, 360, 500))
  int1 <- lubridate::interval(starts1, ends1)
  int2 <- lubridate::interval(NA_POSIXct_, NA_POSIXct_)

  phint <- phinterval(intervals = list(int1, int2), tzone = "UTC")
  expect_identical(
    n_spans(phint),
    c(4L, NA_integer_)
  )

})

test_that("empty inputs result in empty integer outputs", {

  expect_identical(
    n_spans(phinterval()),
    integer()
  )

})

# phint_to_spans ---------------------------------------------------------------

test_that("spans are correct", {

  starts1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 120, 240))
  ends1   <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 4000))
  int1 <- lubridate::interval(starts1, ends1)

  starts2 <- lubridate::ymd(20211011) + lubridate::dminutes(c(0, 10))
  ends2   <- lubridate::ymd(20211011) + lubridate::dminutes(c(5, 20))
  int2 <- lubridate::interval(starts2, ends2)

  phint <- phinterval(intervals = list(int1, int2), tzone = "UTC")
  expect_identical(
    phint_to_spans(phint),
    list(int1, int2)
  )

})

test_that("spans are ordered by start date", {

  starts1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 120, 240))
  ends1   <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 4000))
  int1 <- lubridate::interval(starts1, ends1)

  starts2 <- lubridate::ymd(20211011) + lubridate::dminutes(c(0, 10))
  ends2   <- lubridate::ymd(20211011) + lubridate::dminutes(c(5, 20))
  int2 <- lubridate::interval(starts2, ends2)

  phint <- phinterval(intervals = list(int1, int2), tzone = "UTC")
  expect_identical(
    phint_to_spans(phint),
    list(int1, int2)
  )

})

test_that("equivilant phintervals with different fields have the same output", {

  origin <- .POSIXct(0, tz = "UTC")
  phint1 <- new_phinterval(
    reference_time = c(rep(origin, 2), NA_POSIXct_, origin),
    range_starts = list(
      c(60, 120),
      c(240, 180, 300),
      NA_real_,
      0
    ),
    range_ends = list(
      c(90, 150),
      c(270, 210, 330),
      NA_real_,
      30
    )
  )

  origin_plus_10 <- .POSIXct(10, tz = "UTC")
  phint2 <- new_phinterval(
    reference_time = c(rep(origin_plus_10, 2), NA_POSIXct_, origin_plus_10),
    range_starts = list(
      c(50, 110),
      c(230, 170, 290),
      NA_real_,
      -10
    ),
    range_ends = list(
      c(80, 140),
      c(260, 200, 320),
      NA_real_,
      20
    )
  )

  expect_identical(
    phint_to_spans(phint1),
    phint_to_spans(phint2)
  )

})

test_that("NA inputs result in NA output", {

  starts1 <- NA_POSIXct_
  ends1   <- NA_POSIXct_
  starts2 <- lubridate::ymd(20111111) + lubridate::dseconds(c(0, 60))
  ends2   <- lubridate::ymd(20111111) + lubridate::dseconds(c(30, 120))

  int1 <- lubridate::interval(starts1, ends1, tzone = "UTC")
  int2 <- lubridate::interval(starts2, ends2, tzone = "UTC")

  phint <- phinterval(intervals = list(int1, int2), tzone = "UTC")
  expect_identical(
    phint_to_spans(phint),
    list(int1, int2)
  )

})

test_that("empty input results an empty list output", {

  expect_identical(
    phint_to_spans(phinterval()),
    list()
  )

})

# phint_lengths ----------------------------------------------------------------

test_that("equivilant phintervals with different fields have the same output", {

  origin <- .POSIXct(0, tz = "UTC")
  phint1 <- new_phinterval(
    reference_time = c(rep(origin, 2), NA_POSIXct_, origin),
    range_starts = list(
      c(60, 120),
      c(240, 180, 300),
      NA_real_,
      0
    ),
    range_ends = list(
      c(90, 150),
      c(270, 210, 330),
      NA_real_,
      30
    )
  )

  origin_plus_10 <- .POSIXct(10, tz = "UTC")
  phint2 <- new_phinterval(
    reference_time = c(rep(origin_plus_10, 2), NA_POSIXct_, origin_plus_10),
    range_starts = list(
      c(50, 110),
      c(230, 170, 290),
      NA_real_,
      -10
    ),
    range_ends = list(
      c(80, 140),
      c(260, 200, 320),
      NA_real_,
      20
    )
  )

  expect_identical(
    phint_lengths(phint1),
    phint_lengths(phint2)
  )

})

# phint_squash -----------------------------------------------------------------

test_that("overlaps in the phinterval are flattened", {

  starts1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 120, 240))
  ends1   <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 4000))
  int1 <- lubridate::interval(starts1, ends1)

  starts2 <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 10))
  ends2   <- lubridate::ymd(20000101) + lubridate::dseconds(c(5, 20))
  int2 <- lubridate::interval(starts2, ends2)

  starts3 <- lubridate::ymd(20000101) + lubridate::dseconds(1000)
  ends3   <- lubridate::ymd(20000101) + lubridate::dseconds(5000)
  int3 <- lubridate::interval(starts3, ends3)

  phint <- phinterval(intervals = list(int1, int2, int3), tzone = "UTC")

  flat_starts <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 120, 240))
  flat_ends <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 5000))
  flat_int <- lubridate::interval(flat_starts, flat_ends)

  flat_phint <- phinterval(intervals = list(flat_int), tzone = "UTC")

  expect_identical(
    standardize_phinterval(phint_squash(phint)),
    standardize_phinterval(flat_phint)
  )

})

test_that("NA inputs are removed when `na.rm = TRUE`, result in NA when `FALSE`", {

  date <- lubridate::ymd(20000101)

  starts1 <- date + lubridate::dseconds(c(0, 120, 240))
  ends1   <- date + lubridate::dseconds(c(60, 180, 4000))
  int1 <- lubridate::interval(starts1, ends1)

  starts2 <- date + lubridate::dseconds(c(0, 10, 5000))
  ends2   <- date + lubridate::dseconds(c(5, 20, 6000))
  int2 <- lubridate::interval(starts2, ends2)

  int3 <- lubridate::interval(NA_POSIXct_, NA_POSIXct_)

  phint <- phinterval(intervals = list(int1, int2, int3), tzone = "UTC")

  flat_starts <- date + lubridate::dseconds(c(0, 120, 240, 5000))
  flat_ends <- date + lubridate::dseconds(c(60, 180, 4000, 6000))
  flat_int <- lubridate::interval(flat_starts, flat_ends)

  flat_phint <- phinterval(intervals = list(flat_int), tzone = "UTC")

  ## na.rm = TRUE
  expect_identical(
    standardize_phinterval(phint_squash(phint, na.rm = TRUE)),
    standardize_phinterval(flat_phint)
  )

  ## na.rm = FALSE
  expect_identical(
    standardize_phinterval(phint_squash(phint, na.rm = FALSE)),
    NA_phinterval(tzone = "UTC")
  )

})

test_that("all NA input always results in NA output", {

  na_int <- lubridate::interval(NA_POSIXct_, NA_POSIXct_)
  phint <- phinterval(intervals = list(na_int, na_int), tzone = "UTC")

  ## na.rm = TRUE
  expect_identical(
    phint_squash(phint, na.rm = TRUE),
    NA_phinterval(tzone = "UTC")
  )

  ## na.rm = FALSE
  expect_identical(
    phint_squash(phint, na.rm = FALSE),
    NA_phinterval(tzone = "UTC")
  )

})

test_that("empty input results in empty phinterval output", {

  ## na.rm = TRUE
  expect_identical(
    phint_squash(phinterval(), na.rm = TRUE),
    phinterval()
  )

  ## na.rm = FALSE
  expect_identical(
    phint_squash(phinterval(), na.rm = FALSE),
    phinterval()
  )

})

# int_squash -------------------------------------------------------------------

test_that("overlaps in the Interval are flattened", {

  starts1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 120, 240))
  ends1   <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 4000))
  int1 <- lubridate::interval(starts1, ends1, tzone = "UTC")

  starts2 <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 10))
  ends2   <- lubridate::ymd(20000101) + lubridate::dseconds(c(5, 20))
  int2 <- lubridate::interval(starts2, ends2, tzone = "UTC")

  starts3 <- lubridate::ymd(20000101) + lubridate::dseconds(1000)
  ends3   <- lubridate::ymd(20000101) + lubridate::dseconds(5000)
  int3 <- lubridate::interval(starts3, ends3, tzone = "UTC")

  int <- c(int1, int2, int3)

  flat_starts <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 120, 240))
  flat_ends <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 5000))
  flat_int <- lubridate::interval(flat_starts, flat_ends)

  flat_phint <- phinterval(intervals = list(flat_int), tzone = "UTC")

  expect_identical(
    standardize_phinterval(int_squash(int)),
    standardize_phinterval(flat_phint)
  )

})

test_that("NA inputs are removed when `na.rm = TRUE`, result in NA when `FALSE`", {

  date <- lubridate::ymd(20000101)

  starts1 <- date + lubridate::dseconds(c(0, 120, 240))
  ends1   <- date + lubridate::dseconds(c(60, 180, 4000))
  int1 <- lubridate::interval(starts1, ends1, tzone = "UTC")

  starts2 <- date + lubridate::dseconds(c(0, 10, 5000))
  ends2   <- date + lubridate::dseconds(c(5, 20, 6000))
  int2 <- lubridate::interval(starts2, ends2, tzone = "UTC")

  int3 <- lubridate::interval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")

  int <- c(int1, int2, int3)

  flat_starts <- date + lubridate::dseconds(c(0, 120, 240, 5000))
  flat_ends <- date + lubridate::dseconds(c(60, 180, 4000, 6000))
  flat_int <- lubridate::interval(flat_starts, flat_ends)

  flat_phint <- phinterval(intervals = list(flat_int), tzone = "UTC")

  ## na.rm = TRUE
  expect_identical(
    standardize_phinterval(int_squash(int, na.rm = TRUE)),
    standardize_phinterval(flat_phint)
  )

  ## na.rm = FALSE
  expect_identical(
    standardize_phinterval(int_squash(int, na.rm = FALSE)),
    NA_phinterval(tzone = "UTC")
  )

})

test_that("all NA input always results in NA output", {

  na_int <- lubridate::interval(NA_POSIXct_, NA_POSIXct_)

  ## na.rm = TRUE
  expect_identical(
    int_squash(na_int, na.rm = TRUE),
    NA_phinterval(tzone = "UTC")
  )

  ## na.rm = FALSE
  expect_identical(
    int_squash(na_int, na.rm = FALSE),
    NA_phinterval(tzone = "UTC")
  )

})

test_that("empty input results in empty phinterval output", {

  ## na.rm = TRUE
  expect_identical(
    int_squash(lubridate::interval(), na.rm = TRUE),
    phinterval()
  )

  ## na.rm = FALSE
  expect_identical(
    int_squash(lubridate::interval(), na.rm = FALSE),
    phinterval()
  )

})

# phint_overlaps ---------------------------------------------------------------

test_that("overlaps are correctly identified", {

  origin <- .POSIXct(0, tz = "UTC")
  phint1 <- new_phinterval(
    reference_time = rep(origin, 2),
    range_starts = list(c(0, 50), 10),
    range_ends = list(c(30, 80), 70)
  )
  phint2 <- new_phinterval(
    reference_time = rep(origin, 2),
    range_starts = list(c(70, 100), 0),
    range_ends = list(c(90, 120), 5)
  )

  expect_identical(
    phint_overlaps(phint1, phint2),
    c(TRUE, FALSE)
  )

})

test_that("instants within ranges are overlaps iff `instants = TRUE`", {

  origin <- .POSIXct(0, tz = "UTC")
  phint1 <- new_phinterval(
    reference_time = origin,
    range_starts = list(0),
    range_ends = list(0)
  )
  phint2 <- new_phinterval(
    reference_time = origin,
    range_starts = list(-1),
    range_ends = list(1)
  )

  expect_true(phint_overlaps(phint1, phint2, instants = TRUE))
  expect_false(phint_overlaps(phint1, phint2, instants = FALSE))

})

test_that("aligned spans are not overlaps if `instants = FALSE`", {

  origin <- .POSIXct(0, tz = "UTC")
  phint1 <- new_phinterval(
    reference_time = origin,
    range_starts = list(c(0, 10)),
    range_ends = list(c(5, 20))
  )
  phint2 <- new_phinterval(
    reference_time = origin,
    range_starts = list(5),
    range_ends = list(10)
  )

  expect_false(phint_overlaps(phint1, phint2, instants = FALSE))

})

test_that("aligned spans are overlaps if `instants = TRUE`", {

  origin <- .POSIXct(0, tz = "UTC")
  phint1 <- new_phinterval(
    reference_time = rep(origin, 2),
    range_starts = list(0, 10),
    range_ends = list(5, 20)
  )
  phint2 <- new_phinterval(
    reference_time = rep(origin, 2),
    range_starts = list(-10, 20),
    range_ends = list(0, 30)
  )

  expect_identical(
    phint_overlaps(phint1, phint2, instants = TRUE),
    c(TRUE, TRUE)
  )

})

# phint_union ------------------------------------------------------------------

test_that("overlapping phintervals are merged correctly", {

  origin <- .POSIXct(0, tz = "UTC")
  phint1 <- new_phinterval(
    reference_time = rep(origin, 2),
    range_starts = list(c(0, 50), 10),
    range_ends = list(c(30, 80), 70)
  )
  phint2 <- new_phinterval(
    reference_time = rep(origin, 2),
    range_starts = list(c(70, 100), 0),
    range_ends = list(c(90, 120), 5)
  )

  expect_identical(
    phint_union(phint1, phint2),
    new_phinterval(
      reference_time = rep(origin, 2),
      range_starts = list(c(0, 50, 100), c(0, 10)),
      range_ends = list(c(30, 90, 120), c(5, 70))
    )
  )

})

# standardize_phinterval -------------------------------------------------------

test_that("`ranges_starts` start at 0 and are ordered", {

  origin <- .POSIXct(0, tz = "UTC")

  starts1 <- NA_POSIXct_
  ends1   <- NA_POSIXct_
  int1 <- lubridate::interval(starts1, ends1, tzone = "UTC")

  starts2 <- origin + lubridate::dseconds(c(10, 60, 240, 50))
  ends2   <- origin + lubridate::dseconds(c(30, 120, 300, 55))
  int2 <- lubridate::interval(starts2, ends2, tzone = "UTC")

  starts3 <- origin + lubridate::dseconds(c(15.60))
  ends3   <- origin + lubridate::dseconds(c(27.99))
  int3 <- lubridate::interval(starts3, ends3, tzone = "UTC")

  phint <- phinterval(intervals = list(int1, int2, int3), tzone = "UTC")

  std_phint <- standardize_phinterval(phint)
  std_range_starts <- field(std_phint, "range_starts")

  expect_identical(
    std_range_starts,
    list(
      NA_real_,
      c(0, 40, 50, 230),
      0
    )
  )

})

test_that("input and output phintervals are equal", {

  origin <- .POSIXct(0, tz = "UTC")
  phint <- new_phinterval(
    reference_time = c(rep(origin, 2), NA_POSIXct_, origin),
    range_starts = list(
      c(60, 120),
      c(240, 180, 300),
      NA_real_,
      0
    ),
    range_ends = list(
      c(90, 150),
      c(270, 210, 330),
      NA_real_,
      30
    )
  )

  # TODO: This is a bad test, make a custom expectation for comparing two
  #       phintervals. I think that `all.equal` doesn't play nice with the vctrs record
  #       type proxies (it accesses the underlying lists).
  expect_identical(
    all(vec_equal(phint, standardize_phinterval(phint), na_equal = TRUE)),
    TRUE
  )

})

test_that("NA input results in NA output", {

  expect_identical(
    standardize_phinterval(NA_phinterval()),
    NA_phinterval()
  )

})

test_that("empty input results in empty output", {

  expect_identical(
    standardize_phinterval(phinterval()),
    phinterval()
  )

})

# %within% ---------------------------------------------------------------------

test_that("`phinterval` %within% `phinterval` works as expected", {

  origin <- .POSIXct(0, tz = "UTC")
  phint1 <- new_phinterval(
    reference_time = rep(origin, 2),
    range_starts = list(c(60, 120), 10),
    range_ends = list(c(90, 150), 100)
  )
  phint2 <- new_phinterval(
    reference_time = rep(origin, 2),
    range_starts = list(c(50, 110), 0),
    range_ends = list(c(100, 200), 5)
  )

  expect_identical(
    phint1 %within% phint2,
    c(TRUE, FALSE)
  )

})

test_that("`Interval` %within% `phinterval` works as expected", {

  origin <- .POSIXct(0, tz = "UTC")
  int <- lubridate::interval(c(origin + 60, origin - 5), c(origin + 90, origin))
  phint <- new_phinterval(
    reference_time = rep(origin, 2),
    range_starts = list(c(50, 110), 0),
    range_ends = list(c(100, 200), 5)
  )

  expect_identical(int %within% phint, c(TRUE, FALSE))

})

test_that("`phinterval` %within% `Interval` works as expected", {

  origin <- .POSIXct(0, tz = "UTC")
  phint <- new_phinterval(
    reference_time = rep(origin, 2),
    range_starts = list(c(50, 110), 0),
    range_ends = list(c(100, 200), 5)
  )
  int <- lubridate::interval(c(origin, origin - 5), c(origin + 300, origin))

  expect_identical(phint %within% int,c(TRUE, FALSE))

})

test_that("datetime %within% `phinterval` works as expected", {

  origin <- .POSIXct(0, tz = "UTC")
  phint <- new_phinterval(
    reference_time = rep(origin, 2),
    range_starts = list(c(50, 110), 0),
    range_ends = list(c(100, 200), 5)
  )

  date <- as.Date(c("2010-01-01", "1970-01-01"))
  posixct <- c(origin + 75, origin + 1000)
  posixlt <- as.POSIXlt(posixct)

  expect_identical(date %within% phint, c(FALSE, TRUE))
  expect_identical(posixct %within% phint, c(TRUE, FALSE))
  expect_identical(posixlt %within% phint, c(TRUE, FALSE))

})
