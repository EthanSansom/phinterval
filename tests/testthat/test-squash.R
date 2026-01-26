# phint_squash -----------------------------------------------------------------

test_that("phint_squash() respects empty_to argument", {
  phint <- phinterval(tzone = "UTC")
  int <- interval(tzone = "UTC")

  empty <- phinterval(tzone = "UTC")
  na <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")
  hole <- hole(tzone = "UTC")

  expect_equal(phint_squash(phint, empty_to = "empty"), empty)
  expect_equal(phint_squash(phint, empty_to = "na"), na)
  expect_equal(phint_squash(phint, empty_to = "hole"), hole)

  expect_equal(phint_squash(int, empty_to = "empty"), empty)
  expect_equal(phint_squash(int, empty_to = "na"), na)
  expect_equal(phint_squash(int, empty_to = "hole"), hole)
})

test_that("phint_squash() respects na.rm argument", {
  na_int <- interval(NA, NA)
  int1 <- interval(as.Date("2021-01-01"), as.Date("2021-01-02"))
  int2 <- interval(as.Date("2021-01-04"), as.Date("2021-01-05"))
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_)
  phint <- phinterval(
    start = c(as.Date("2021-01-01"), as.Date("2021-01-04")),
    end = c(as.Date("2021-01-02"), as.Date("2021-01-05")),
    by = 1
  )

  expect_equal(phint_squash(na_int, na.rm = TRUE), na_phint)
  expect_equal(phint_squash(na_int, na.rm = FALSE), na_phint)
  expect_equal(phint_squash(na_phint, na.rm = TRUE), na_phint)
  expect_equal(phint_squash(na_phint, na.rm = FALSE), na_phint)

  expect_equal(phint_squash(c(int1, int2, na_int), na.rm = TRUE), phint)
  expect_equal(phint_squash(c(int1, int2, na_int), na.rm = FALSE), na_phint)
  expect_equal(phint_squash(c(na_int, int1), na.rm = TRUE), as_phinterval(int1))
  expect_equal(phint_squash(c(na_int, int1), na.rm = FALSE), na_phint)
})

# TODO: Add `by` and `order_by`
test_that("phint_squash() errors on invalid inputs", {
  expect_error(phint_squash(phinterval(), na.rm = NA))
  expect_error(phint_squash(phinterval(), empty_to = ""))
  expect_error(phint_squash(phinterval(), empty_to = 10))
  expect_error(phint_squash(as.Date("2020-01-01")))
})

test_that("phint_squash() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:05:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:15:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:25:00", tz = "UTC")

  int12 <- interval(t1, t2)
  int23 <- interval(t2, t3)
  int34 <- interval(t3, t4)
  int45 <- interval(t4, t5)
  int14 <- interval(t1, t4)
  int35 <- interval(t3, t5)
  int46 <- interval(t4, t6)

  int22 <- interval(t2, t2)

  hole <- hole(tzone = "UTC")

  # Squash scalar
  expect_equal(phint_squash(int22), as_phinterval(int22))
  expect_equal(phint_squash(int12), as_phinterval(int12))
  expect_equal(phint_squash(as_phinterval(int12)), as_phinterval(int12))
  expect_identical(phint_squash(hole), hole)

  # Squash abutting
  expect_equal(phint_squash(c(int12, int23)), phinterval(t1, t3))
  expect_equal(phint_squash(as_phinterval(c(int46, int14))), phinterval(t1, t6))

  # Squash overlapping
  expect_equal(phint_squash(c(int12, int14)), as_phinterval(int14))
  expect_equal(phint_squash(c(int12, int14)), as_phinterval(int14))
  expect_equal(phint_squash(as_phinterval(c(int12, int22))), as_phinterval(int12))
  expect_equal(phint_squash(c(int14, int35)), phinterval(t1, t5))
  expect_equal(phint_squash(c(int46, int35)), phinterval(t3, t6))

  # Squash non-overlapping
  expect_equal(
    phint_squash(c(int12, int34, int45)),
    phinterval(c(t1, t3, t4), c(t2, t4, t5), by = 1)
  )
  expect_equal(
    phint_squash(c(hole, int12, int34)),
    phinterval(c(t1, t3), c(t2, t4), by = 1)
  )
})

# TODO
test_that("phint_squash(by = by, order_by = order_by) works as expected", {

})

# datetime_squash --------------------------------------------------------------

# TODO
