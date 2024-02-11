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

# TODO Ethan: This passes, by make sure you are SUPER careful of timezones moving forward
test_that("starts are correct", {

  ymd      <- function(x, ...) lubridate::ymd(x, ...)
  ddays    <- function(x, ...) lubridate::ddays(x, ...)
  dseconds <- function(x, ...) lubridate::dseconds(x, ...)

  starts1 <- ymd(20200101) + ddays(c(0, 2, 4)) + dseconds(c(0, 1201, 2403))
  ends1   <- ymd(20200101) + ddays(c(1, 3, 5)) + dseconds(c(82, 709, 8999))
  int1 <- lubridate::interval(starts1, ends1)

  starts2 <- ymd(20240809) + ddays(c(10, 0))
  ends2   <- ymd(20240809) + ddays(c(20, 5))
  int2 <- lubridate::interval(starts2, ends2)

  phint <- phinterval(intervals = list(int1, int2), tzone = "UTC")
  expect_identical(
    phint_start(phint),
    c(ymd(20200101, tz = "UTC"), ymd(20240809, tz = "UTC"))
  )
})

# TODO Ethan:
# - Check NAs!
# - make sure the output class is a list of POSIXct (EVEN THE NA VALUES)

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
