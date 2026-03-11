# phint_flatten ----------------------------------------------------------------

test_that("phint_flatten() errors on invalid inputs", {
  phint <- phinterval(as.Date("2021-01-01"), as.Date("2021-02-01"))

  expect_error(phint_flatten(10))
  expect_error(phint_flatten(phint, what = NA))
  expect_error(phint_flatten(phint, what = "gap"))
})

test_that("phint_flatten() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:04:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:08:00", tz = "UTC")

  int12 <- phinterval(t1, t2)
  int23 <- phinterval(t2, t3)
  int34 <- phinterval(t3, t4)
  int45 <- phinterval(t4, t5)
  int56 <- phinterval(t5, t6)
  int13 <- phinterval(t1, t3)
  int25 <- phinterval(t2, t5)

  hole <- hole(tzone = "UTC")
  empty <- phinterval(tzone = "UTC")
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")

  # Empty
  expect_equal(phint_flatten(empty), empty)
  expect_equal(phint_flatten(empty, what = "holes"), empty)

  # All-NA or hole input returns empty output
  expect_equal(phint_flatten(na_phint), empty)
  expect_equal(phint_flatten(rep(na_phint, 3)), empty)
  expect_equal(phint_flatten(hole), empty)
  expect_equal(phint_flatten(hole, what = "holes"), empty)

  # Single span
  expect_equal(phint_flatten(int12), int12)
  expect_equal(phint_flatten(rep(int12, 3)), int12)

  # Non-overlapping spans are returned as individual elements
  expect_equal(phint_flatten(c(int12, int34)), c(int12, int34))
  expect_equal(phint_flatten(c(int12, int34, int56)), c(int12, int34, int56))

  # Overlapping or adjacent spans are merged
  expect_equal(phint_flatten(c(int12, int23)), int13)
  expect_equal(phint_flatten(c(int12, int25, int34)), phinterval(t1, t5))

  # NA elements are ignored
  expect_equal(phint_flatten(c(int12, na_phint, int34)), c(int12, int34))
  expect_equal(phint_flatten(c(na_phint, int12, na_phint, int34, na_phint)), c(int12, int34))

  # what = "holes": returns gaps between spans
  expect_equal(phint_flatten(c(int12, int34), what = "holes"), int23)
  expect_equal(phint_flatten(c(int12, int34, int56), what = "holes"), c(int23, int45))

  # what = "holes": adjacent or overlapping spans have no gap
  expect_equal(phint_flatten(c(int12, int23), what = "holes"), empty)
  expect_equal(phint_flatten(c(int12, int25), what = "holes"), empty)

  # what = "holes": single span has no gaps
  expect_equal(phint_flatten(int12, what = "holes"), empty)

  # Output invariant: all elements are scalar spans, in varied settings
  expect_true(all(n_spans(phint_flatten(c(int12, int34, int56))) == 1L))
  expect_true(all(n_spans(phint_flatten(c(int12, int34, int56), what = "holes")) == 1L))
  expect_true(all(n_spans(phint_flatten(c(int12, na_phint, int34, int56))) == 1L))
  expect_true(all(n_spans(phint_flatten(c(phint_squash(c(int12, int34)), int25, int56))) == 1L))
})

test_that("phint_flatten() handles instantaneous inputs correctly", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:01:05", tz = "UTC")
  int12 <- phinterval(t1, t2)
  int23 <- phinterval(t2, t3)
  int34 <- phinterval(t3, t4)
  int45 <- phinterval(t4, t5)
  int11 <- phinterval(t1, t1)
  int22 <- phinterval(t2, t2)
  int33 <- phinterval(t3, t3)
  int44 <- phinterval(t4, t4)
  empty <- phinterval(tzone = "UTC")

  # Single instant
  expect_equal(phint_flatten(int22), int22)
  expect_equal(phint_flatten(int22, what = "holes"), empty)

  # Multiple instants: what = "spans" returns each distinct instant
  expect_equal(phint_flatten(c(int22, int33)), c(int22, int33))
  expect_equal(phint_flatten(rep(int22, 3)), int22)

  # Instant within a span
  expect_equal(phint_flatten(c(int12, int22), what = "spans"), int12)
  expect_equal(phint_flatten(c(int12, int22), what = "holes"), empty)

  # Instant between two spans: what = "holes" ignores the instant
  expect_equal(
    phint_flatten(c(int12, int33, int45), what = "holes"),
    phinterval(t2, t4)
  )

  # Instant at the boundary of a span: what = "holes" returns the gap on the
  # other side only
  expect_equal(phint_flatten(c(int11, int34), what = "holes"), phinterval(t1, t3))
  expect_equal(phint_flatten(c(int12, int33), what = "holes"), phinterval(t2, t3))

  # Only instants
  expect_equal(
    phint_flatten(c(int22, int33), what = "holes"),
    phinterval(t2, t3)
  )
  expect_equal(
    phint_flatten(c(int11, int22, int33), what = "holes"),
    phinterval(t1, t3)
  )
  expect_equal(
    phint_flatten(c(int11, int22, int33, int44), what = "holes"),
    phinterval(t1, t4)
  )
})

# datetime_flatten -------------------------------------------------------------

test_that("phint_flatten() errors on invalid inputs", {
  t1 <- as.Date("2021-01-01")
  t2 <- as.Date("2021-02-01")

  expect_error(datetime_flatten(10, t2))
  expect_error(datetime_flatten(t1, NA))
  expect_error(datetime_flatten(t1, t2, what = NA))
  expect_error(datetime_flatten(t1, t2, what = "gap"))
})

test_that("datetime_flatten() recycles inputs", {
  t0 <- lubridate::POSIXct(tz = "UTC")
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:30:00", tz = "UTC")

  time0 <- as.POSIXct(numeric(), tz = "UTC")
  starts1 <- t1
  ends1 <- t2
  starts3 <- c(t1, t3, t4)
  ends3 <- c(t2, t4, t4)
  phint0 <- phinterval(time0, time0)

  expect_equal(datetime_flatten(starts1, ends3), datetime_flatten(rep(starts1, 3), ends3))
  expect_equal(datetime_flatten(starts3, ends1), datetime_flatten(starts3, rep(starts1, 3)))
  expect_equal(datetime_flatten(time0, ends1), phint0)
  expect_equal(datetime_flatten(starts1, time0), phint0)

  expect_error(datetime_flatten(rep(starts1, 2), ends3))
})

test_that("datetime_flatten() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:04:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:08:00", tz = "UTC")
  t_na <- as.POSIXct(NA, tz = "UTC")

  int12 <- phinterval(t1, t2)
  int23 <- phinterval(t2, t3)
  int34 <- phinterval(t3, t4)
  int45 <- phinterval(t4, t5)
  int56 <- phinterval(t5, t6)
  int13 <- phinterval(t1, t3)
  int25 <- phinterval(t2, t5)

  hole <- hole(tzone = "UTC")
  empty <- phinterval(tzone = "UTC")
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")

  # All-NA or hole input returns empty output
  expect_equal(datetime_flatten(t_na, t_na), empty)
  expect_equal(datetime_flatten(rep(t_na, 3), rep(t_na, 3)), empty)

  # Single span
  expect_equal(datetime_flatten(t1, t2), int12)
  expect_equal(datetime_flatten(rep(t1, 3), rep(t2, 3)), int12)

  # Non-overlapping spans are returned as individual elements
  expect_equal(datetime_flatten(c(t1, t3), c(t2, t4)), c(int12, int34))
  expect_equal(datetime_flatten(c(t1, t3, t5), c(t2, t4, t6)), c(int12, int34, int56))

  # Overlapping or adjacent spans are merged
  expect_equal(datetime_flatten(c(t1, t2), c(t2, t3)), int13)
  expect_equal(datetime_flatten(c(t1, t2, t3), c(t2, t5, t4)), phinterval(t1, t5))

  # NA elements are ignored
  expect_equal(datetime_flatten(c(t1, t_na, t3), c(t2, t_na, t4)), c(int12, int34))
  expect_equal(datetime_flatten(c(t_na, t1, t_na, t3, t_na), c(t_na, t2, t_na, t4, t_na)), c(int12, int34))

  # what = "holes": returns gaps between spans
  expect_equal(datetime_flatten(c(t1, t3), c(t2, t4), what = "holes"), int23)
  expect_equal(datetime_flatten(c(t1, t3, t5), c(t2, t4, t6), what = "holes"), c(int23, int45))

  # what = "holes": adjacent or overlapping spans have no gap
  expect_equal(datetime_flatten(c(t1, t2), c(t2, t3), what = "holes"), empty)
  expect_equal(datetime_flatten(c(t1, t2), c(t2, t5), what = "holes"), empty)

  # what = "holes": single span has no gaps
  expect_equal(datetime_flatten(t1, t2, what = "holes"), empty)

  # Output invariant: all elements are scalar spans, in varied settings
  expect_true(all(n_spans(datetime_flatten(c(t1, t3, t5), c(t2, t4, t6))) == 1L))
  expect_true(all(n_spans(datetime_flatten(c(t1, t3, t5), c(t2, t4, t6), what = "holes")) == 1L))
  expect_true(all(n_spans(datetime_flatten(c(t1, t_na, t3, t5), c(t2, t_na, t4, t6))) == 1L))
})

test_that("datetime_flatten() handles instantaneous inputs correctly", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:01:05", tz = "UTC")
  int12 <- phinterval(t1, t2)
  int23 <- phinterval(t2, t3)
  int34 <- phinterval(t3, t4)
  int45 <- phinterval(t4, t5)
  int11 <- phinterval(t1, t1)
  int22 <- phinterval(t2, t2)
  int33 <- phinterval(t3, t3)
  int44 <- phinterval(t4, t4)
  empty <- phinterval(tzone = "UTC")

  # Single instant
  expect_equal(datetime_flatten(t2, t2), int22)
  expect_equal(datetime_flatten(t2, t2, what = "holes"), empty)

  # Multiple instants: what = "spans" returns each distinct instant
  expect_equal(datetime_flatten(c(t2, t3), c(t2, t3)), c(int22, int33))
  expect_equal(datetime_flatten(rep(t2, 3), rep(t2, 3)), int22)

  # Instant within a span
  expect_equal(datetime_flatten(c(t1, t2), c(t2, t2), what = "spans"), int12)
  expect_equal(datetime_flatten(c(t1, t2), c(t2, t2), what = "holes"), empty)

  # Instant between two spans: what = "holes" ignores the instant
  expect_equal(
    datetime_flatten(c(t1, t3, t4), c(t2, t3, t5), what = "holes"),
    phinterval(t2, t4)
  )

  # Instant at the boundary of a span: what = "holes" returns the gap on the
  # other side only
  expect_equal(datetime_flatten(c(t1, t3), c(t1, t4), what = "holes"), phinterval(t1, t3))
  expect_equal(datetime_flatten(c(t1, t3), c(t2, t3), what = "holes"), phinterval(t2, t3))

  # Only instants
  expect_equal(
    datetime_flatten(c(t2, t3), c(t2, t3), what = "holes"),
    phinterval(t2, t3)
  )
  expect_equal(
    datetime_flatten(c(t1, t2, t3), c(t1, t2, t3), what = "holes"),
    phinterval(t1, t3)
  )
  expect_equal(
    datetime_flatten(c(t1, t2, t3, t4), c(t1, t2, t3, t4), what = "holes"),
    phinterval(t1, t4)
  )
})
