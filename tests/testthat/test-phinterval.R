# phinterval -------------------------------------------------------------------

test_that("phinterval() returns empty vector", {
  phint <- phinterval()
  expect_s3_class(phint, "phinterval")
  expect_length(phint, 0L)

  phint <- phinterval(list())
  expect_s3_class(phint, "phinterval")
  expect_length(phint, 0L)
})

test_that("NA intervals result in NA output", {
  date <- as.Date("2020-01-01")
  phint <- as_phinterval(interval(
    c(date, NA, date, NA),
    c(date, NA, NA, date)
  ))
  expect_equal(is.na(phint), c(FALSE, TRUE, TRUE, TRUE))
})

test_that("phinterval() errors on invalid inputs", {
  int <- interval(as.Date("2021-01-01"), as.Date("2021-02-01"))

  expect_error(phinterval(list(int, 10)))
  expect_error(phinterval(10))
  expect_error(phinterval(as.Date("2020-01-01")))
  expect_error(phinterval(list(int), tzone = 1))
  expect_error(phinterval(list(int), tzone = "notazone"))
  expect_error(phinterval(tzone = c("UTC", "EST")))
  expect_error(phinterval(tzone = "notazone"))
})

test_that("phinterval() tzone argument overrides intervals timezone", {
  int <- interval(as.Date("2021-01-01"), as.Date("2021-02-01"), tzone = "UTC")
  phint <- phinterval(list(int), tzone = "EST")

  expect_equal(attr(phint, "tzone"), "EST")
  expect_equal(
    as.numeric(lubridate::int_start(int)),
    as.numeric(phint_start(phint))
  )
  expect_equal(
    as.numeric(lubridate::int_end(int)),
    as.numeric(phint_end(phint))
  )
})

test_that("phinterval() handles DST transitions correctly", {
  # Only 1 hour elapses between 1:30 and 3:30 due to DST
  t1 <- as.POSIXct("2021-03-14 01:30:00", tz = "America/Toronto")
  t2 <- as.POSIXct("2021-03-14 03:30:00", tz = "America/Toronto")

  int <- interval(t1, t2)
  phint <- phinterval(list(int), tzone = "UTC")

  expect_equal(get_tzone(phint), "UTC")
  expect_equal(
    as.numeric(phint_end(phint) - phint_start(phint)),
    as.numeric(t2 - t1)
  )
})

test_that("phinterval() merges overlapping and adjacent intervals", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")

  overlapping <- interval(c(t1, t2), c(t3, t4))
  non_lapping <- interval(t1, t4)
  adjacent <- interval(c(t1, t2), c(t2, t3))
  non_adj <- interval(t1, t3)

  phint <- phinterval(list(overlapping, adjacent))
  expect_equal(phint_to_spans(phint), list(non_lapping, non_adj))
})

test_that("phinterval() standardizes mixed timezones", {
  int_utc <- interval(
    as.POSIXct("2021-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2021-01-02 00:00:00", tz = "UTC")
  )
  int_est <- interval(
    as.POSIXct("2021-01-03 00:00:00", tz = "EST"),
    as.POSIXct("2021-01-04 00:00:00", tz = "EST")
  )

  phint_rok <- phinterval(list(int_est, int_utc), tzone = "ROK")
  phint_est <- phinterval(list(int_est, int_utc), tzone = "EST")
  phint_utc <- phinterval(list(int_est, int_utc), tzone = "UTC")

  expect_true(all(phint_rok == phint_est))
  expect_true(all(phint_utc == phint_est))
  expect_equal(get_tzone(phint_rok), "ROK")
  expect_equal(get_tzone(phint_est), "EST")
  expect_equal(get_tzone(phint_utc), "UTC")

  # First timezone is used by default
  expect_phint_equal(phinterval(list(int_est, int_utc)), phint_est)
})

test_that("phinterval() standardizes input intervals", {
  int <- interval(as.Date("2020-01-01"), as.Date("2021-01-01"))
  expect_equal(
    phinterval(list(int)),
    phinterval(list(lubridate::int_flip(int)))
  )
})

test_that("phinterval() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "EST")
  t2 <- as.POSIXct("2021-01-01 00:00:30", tz = "EST")
  t3 <- as.POSIXct("2021-01-01 00:10:20", tz = "EST")
  t4 <- as.POSIXct("2021-01-01 00:20:30", tz = "EST")
  t5 <- as.POSIXct("2021-01-02 00:00:00", tz = "EST")
  int1 <- interval(t1, t2, tzone = "EST")
  int2 <- interval(c(t1, t3), c(t2, t5), tzone = "EST")
  int3 <- interval(c(t1, t3, t5), c(t2, t4, t5), tzone = "EST")
  int4 <- interval(t2, t2, tzone = "EST")

  intervals <- list(int1, int2, int3, int4)
  phint <- phinterval(intervals)

  expect_equal(phint_to_spans(phint), intervals)
  expect_length(phint, length(intervals))
  expect_s3_class(phint, "phinterval")
  expect_equal(attr(phint, "tzone"), "EST")
})

# as_phinterval ----------------------------------------------------------------

test_that("as_phinterval() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:30", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:20", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:20:30", tz = "UTC")

  na_int1 <- interval(NA, NA)
  na_int2 <- interval(t1, NA)
  na_int3 <- interval(NA, t2)
  int0 <- interval()
  int1 <- interval(t1, t2)
  int2 <- interval(c(t1, t3), c(t2, t4))

  phint0 <- phinterval()
  phint1 <- phinterval(list(int1))
  phint2 <- phinterval(list(interval(t1, t2), interval(t3, t4)))

  expect_equal(as_phinterval(int0), phinterval())
  expect_equal(as_phinterval(int1), phint1)
  expect_equal(as_phinterval(int2), phint2)
  expect_equal(
    as_phinterval(c(na_int1, na_int2, na_int3)),
    phinterval(list(na_int1, na_int1, na_int1))
  )

  # Time zones are respected
  int3 <- interval(lubridate::with_tz(t1, "EST"), lubridate::with_tz(t2, "EST"))
  expect_equal(as_phinterval(int3), phinterval(interval(t1, t2), tzone = "EST"))
  expect_equal(get_tzone(as_phinterval(int2)), "UTC")
  expect_equal(get_tzone(as_phinterval(int3)), "EST")
})

# phint_start ------------------------------------------------------------------

test_that("phint_start() empty input results in empty output", {
  phint <- phinterval(tzone = "EST")
  start <- phint_start(phint)

  expect_s3_class(start, "POSIXct")
  expect_length(start, 0L)
  expect_equal(lubridate::tz(start), "EST")
})

test_that("phint_start() NA input results in NA output", {
  int1 <- interval(NA, NA)
  int2 <- rep(int1, 3)
  phint <- phinterval(list(int1, int2), tzone = "GMT")
  start <- phint_start(phint)

  expect_true(all(is.na(start)))
  expect_s3_class(start, "POSIXct")
  expect_length(start, 2L)
  expect_equal(lubridate::tz(start), "GMT")
})

test_that("phint_start() starts are correct", {
  t1 <- as.POSIXct("2021-01-01 00:00:00")
  t2 <- as.POSIXct("2021-01-01 00:10:00")
  t3 <- as.POSIXct("2021-01-01 00:10:30")
  t4 <- as.POSIXct("2021-01-01 00:10:50")

  int1 <- interval(c(t3, t1), c(t4, t2))
  int2 <- interval(t2, t4)
  phint <- phinterval(list(int1, int2), tzone = "EST")
  start <- phint_start(phint)

  expect_equal(as.numeric(start), as.numeric(c(t1, t2)))
  expect_equal(lubridate::tz(start), "EST")
})

# phint_starts -----------------------------------------------------------------

test_that("phint_starts() empty input results in empty list", {
  phint <- phinterval()
  starts <- phint_starts(phint)

  expect_identical(starts, list())
  expect_length(starts, 0L)
})

test_that("phint_starts() NA input results in NA output", {
  int1 <- interval(NA, NA)
  int2 <- rep(int1, 3)
  phint <- phinterval(list(int1, int2), tzone = "GMT")
  starts <- phint_starts(phint)

  expect_true(all(map_lgl(starts, is.na)))
  expect_true(all(map_lgl(starts, lubridate::is.POSIXct)))
  expect_length(starts, 2L)
  expect_true(all(map_lgl(starts, \(s) lubridate::tz(s) == "GMT")))
})

test_that("phint_starts() starts are correct", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tzone = "EST")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tzone = "EST")
  t3 <- as.POSIXct("2021-01-01 00:10:30", tzone = "EST")
  t4 <- as.POSIXct("2021-01-01 00:10:50", tzone = "EST")

  int <- interval(c(t3, t1), c(t4, t2), tzone = "EST")
  phint <- phint_squash(int)
  starts <- phint_starts(phint)

  expect_true(all(map_lgl(starts, \(s) lubridate::tz(s) == "EST")))
  expect_equal(map(starts, as.numeric), list(as.numeric(c(t1, t3))))
})

# phint_end --------------------------------------------------------------------

test_that("phint_end() empty input results in empty output", {
  phint <- phinterval(tzone = "EST")
  end <- phint_end(phint)

  expect_s3_class(end, "POSIXct")
  expect_length(end, 0L)
  expect_equal(lubridate::tz(end), "EST")
})

test_that("phint_end() NA input results in NA output", {
  int1 <- interval(NA, NA)
  int2 <- rep(int1, 3)
  phint <- phinterval(list(int1, int2), tzone = "GMT")
  end <- phint_end(phint)

  expect_true(all(is.na(end)))
  expect_s3_class(end, "POSIXct")
  expect_length(end, 2L)
  expect_equal(lubridate::tz(end), "GMT")
})

test_that("phint_end() ends are correct", {
  t1 <- as.POSIXct("2021-01-01 00:00:00")
  t2 <- as.POSIXct("2021-01-01 00:10:00")
  t3 <- as.POSIXct("2021-01-01 00:10:30")
  t4 <- as.POSIXct("2021-01-01 00:10:50")

  int1 <- interval(c(t3, t1), c(t4, t2))
  int2 <- interval(t2, t4)
  phint <- phinterval(list(int1, int2), tzone = "EST")
  end <- phint_end(phint)

  expect_equal(as.numeric(end), as.numeric(c(t4, t4)))
  expect_equal(lubridate::tz(end), "EST")
})

# phint_ends -------------------------------------------------------------------

test_that("phint_ends() empty input results in empty list", {
  phint <- phinterval()
  ends <- phint_ends(phint)

  expect_identical(ends, list())
  expect_length(ends, 0L)
})

test_that("phint_ends() NA input results in NA output", {
  int1 <- interval(NA, NA)
  int2 <- rep(int1, 3)
  phint <- phinterval(list(int1, int2), tzone = "GMT")
  ends <- phint_ends(phint)

  expect_true(all(map_lgl(ends, is.na)))
  expect_true(all(map_lgl(ends, lubridate::is.POSIXct)))
  expect_length(ends, 2L)
  expect_true(all(map_lgl(ends, \(e) lubridate::tz(e) == "GMT")))
})

test_that("phint_ends() ends are correct", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tzone = "EST")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tzone = "EST")
  t3 <- as.POSIXct("2021-01-01 00:10:30", tzone = "EST")
  t4 <- as.POSIXct("2021-01-01 00:10:50", tzone = "EST")

  int <- interval(c(t3, t1), c(t4, t2), tzone = "EST")
  phint <- phint_squash(int)
  ends <- phint_ends(phint)

  expect_true(all(map_lgl(ends, \(e) lubridate::tz(e) == "EST")))
  expect_equal(map(ends, as.numeric), list(as.numeric(c(t2, t4))))
})

# phint_squash -----------------------------------------------------------------

# TODO:

# phint_union ------------------------------------------------------------------

test_that("phint_union() recycles inputs", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:30:00", tz = "UTC")

  phint0 <- phinterval()
  phint1 <- phinterval(interval(t1, t2))
  phint3 <- as_phinterval(interval(c(t1, t3, t4), c(t2, t4, t4)))

  expect_equal(
    phint_union(phint1, phint3),
    phint_union(rep(phint1, 3), phint3)
  )
  expect_equal(
    phint_union(phint0, phint1),
    phint0
  )
  expect_error(phint_union(rep(phint1, 2), phint3))
  expect_error(phint_union(phint3, phint0))
})

test_that("phint_union() empty input results in empty output", {
  out <- phint_union(phinterval(), phinterval())

  expect_s3_class(out, "phinterval")
  expect_length(out, 0L)
  expect_identical(phint_to_spans(out), list())
})

test_that("phint_union() NA input results in NA output", {
  t1 <- as.POSIXct("2021-01-01", tz = "UTC")
  t2 <- as.POSIXct("2021-01-02", tz = "UTC")

  int <- interval(t1, t2)
  na_int <- interval(NA, NA)
  phint <- phinterval(list(na_int, int))

  expect_equal(is.na(phint_union(int, phint)), c(TRUE, FALSE))
  expect_equal(is.na(phint_union(phint, na_int)), c(TRUE, TRUE))
})

test_that("phint_union() errors on invalid inputs", {
  int <- interval(as.Date("2021-01-01"), as.Date("2021-02-01"))
  phint <- as_phinterval(int)

  expect_error(phint_union(int, 10))
  expect_error(phint_union("A", phint))
})

test_that("phint_union() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:04:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:08:00", tz = "UTC")

  int12 <- interval(t1, t2)
  int23 <- interval(t2, t3)
  int34 <- interval(t3, t4)
  int25 <- interval(t2, t5)
  int36 <- interval(t3, t6)

  int22 <- interval(t2, t2)

  hole <- phinterval(list(interval()))

  # Self union
  expect_phint_equal(phint_union(int12, int12), int12)
  expect_phint_equal(phint_union(int22, int22), int22)

  # Union with hole
  expect_phint_equal(phint_union(int12, hole), int12)
  expect_phint_equal(phint_union(hole, int12), int12)
  expect_phint_equal(phint_union(rep(phinterval(int12), 3), rep(hole, 3)), rep(int12, 3))
  expect_phint_equal(phint_union(rep(hole, 3), rep(phinterval(int12), 3)), rep(int12, 3))

  # Union of non-overlapping
  expect_phint_equal(phint_union(int12, int34), phint_squash(c(int12, int34)))
  expect_phint_equal(
    phint_union(
      phint_squash(c(int12, int34)),
      int25
    ),
    phint_squash(c(interval(t1, t5), int34))
  )

  # Union of overlapping or abutting
  expect_phint_equal(phint_union(int12, interval(t1, t3)), interval(t1, t3))
  expect_phint_equal(phint_union(int25, int36), interval(t2, t6))
  expect_phint_equal(phint_union(int23, int34), interval(t2, t4))

  # Union with instant
  expect_phint_equal(phint_union(int22, int12), int12)
  expect_phint_equal(phint_union(int12, int22), int12)
  expect_phint_equal(phint_union(int12, int34), phint_squash(c(int12, int34)))
})

# phint_setdiff ----------------------------------------------------------------

test_that("phint_setdiff() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:04:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:08:00", tz = "UTC")

  int12 <- interval(t1, t2)
  int23 <- interval(t2, t3)
  int34 <- interval(t3, t4)
  int45 <- interval(t4, t5)
  int56 <- interval(t5, t6)

  int13 <- interval(t1, t3)
  int25 <- interval(t2, t5)
  int36 <- interval(t3, t6)
  int22 <- interval(t2, t2)

  hole <- phinterval(list(interval()))

  # Self difference
  expect_phint_equal(phint_setdiff(int12, int12), hole)
  expect_phint_equal(phint_setdiff(rep(int12, 3), rep(int12, 3)), rep(hole, 3))

  # Difference with hole
  expect_phint_equal(phint_setdiff(int12, hole), int12)
  expect_phint_equal(phint_setdiff(hole, int12), hole)
  expect_phint_equal(phint_setdiff(rep(int12, 3), rep(hole, 3)), rep(int12, 3))
  expect_phint_equal(phint_setdiff(rep(hole, 3), rep(int12, 3)), rep(hole, 3))

  # Difference with non-overlapping
  expect_phint_equal(phint_setdiff(int23, int45), int23)
  expect_phint_equal(phint_setdiff(int45, int23), int45)
  expect_phint_equal(
    phint_setdiff(phint_squash(c(int12, int56)), int34),
    phint_squash(c(int12, int56))
  )

  # Difference with overlapping
  expect_phint_equal(phint_setdiff(int13, int12), int23)
  expect_phint_equal(phint_setdiff(int12, int13), hole)
  expect_phint_equal(
    phint_setdiff(phint_squash(c(int12, int45)), int12),
    int45
  )
  expect_phint_equal(
    phint_setdiff(phint_squash(c(int13, int45)), int12),
    phint_squash(c(int23, int45))
  )
  expect_phint_equal(
    phint_setdiff(int25, int34),
    phint_squash(c(int23, int45))
  )
  expect_phint_equal(phint_setdiff(int25, int36), int23)
  expect_phint_equal(phint_setdiff(int36, int25), int56)

  # Difference with instants
  expect_phint_equal(phint_setdiff(int12, int22), int12)
  expect_phint_equal(phint_setdiff(int22, int12), hole)
  expect_phint_equal(phint_setdiff(int13, int22), int13)
  expect_phint_equal(phint_setdiff(int22, int13), hole)
})

# phint_intersect --------------------------------------------------------------

test_that("phint_intersect() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:04:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:08:00", tz = "UTC")

  int12 <- interval(t1, t2)
  int23 <- interval(t2, t3)
  int34 <- interval(t3, t4)
  int45 <- interval(t4, t5)
  int56 <- interval(t5, t6)

  int13 <- interval(t1, t3)
  int25 <- interval(t2, t5)
  int35 <- interval(t3, t5)
  int36 <- interval(t3, t6)
  int16 <- interval(t1, t6)

  int11 <- interval(t1, t1)
  int22 <- interval(t2, t2)

  hole <- phinterval(list(interval()))

  # Self intersection
  expect_phint_equal(phint_intersect(int12, int12), int12)
  expect_phint_equal(phint_intersect(rep(int12, 3), rep(int12, 3)), rep(int12, 3))

  # Intersection with hole
  expect_phint_equal(phint_intersect(int12, hole), hole)
  expect_phint_equal(phint_intersect(hole, int12), hole)
  expect_phint_equal(phint_intersect(rep(int12, 3), rep(hole, 3)), rep(hole, 3))
  expect_phint_equal(phint_intersect(rep(hole, 3), rep(int12, 3)), rep(hole, 3))

  # Intersection with non-overlapping
  expect_phint_equal(phint_intersect(int23, int45), hole)
  expect_phint_equal(phint_intersect(int45, int23), hole)
  expect_phint_equal(
    phint_intersect(phint_squash(c(int12, int56)), int34),
    hole
  )
  expect_phint_equal(
    phint_intersect(
      c(phint_squash(c(int12, int56)), int34),
      c(int34, int12)
    ),
    c(hole, hole)
  )

  # Intersection with overlapping
  expect_phint_equal(phint_intersect(int13, int12), int12)
  expect_phint_equal(phint_intersect(int12, int13), int12)
  expect_phint_equal(phint_intersect(int25, int36), int35)
  expect_phint_equal(phint_intersect(int36, int25), int35)
  expect_phint_equal(
    phint_intersect(phint_squash(c(int12, int45)), int12),
    int12
  )
  expect_phint_equal(
    phint_intersect(phint_squash(c(int13, int45)), phint_squash(c(int12, int45))),
    phint_squash(c(int12, int45))
  )
  expect_phint_equal(phint_intersect(int25, int34), int34)

  expect_phint_equal(
    phint_intersect(int16, phint_squash(c(int12, int34, int56))),
    phint_squash(c(int12, int34, int56))
  )
  expect_phint_equal(
    phint_intersect(phint_squash(c(int12, int34, int56)), int16),
    phint_squash(c(int12, int34, int56))
  )

  # Intersection with instants
  expect_phint_equal(phint_intersect(int22, int22), int22)
  expect_phint_equal(phint_intersect(int12, int22), int22)
  expect_phint_equal(phint_intersect(int22, int12), int22)
  expect_phint_equal(phint_intersect(int13, int22), int22)
  expect_phint_equal(phint_intersect(int22, int13), int22)

  # Intersection with abutting
  # NOTE: Intersection is endpoint inclusive, so abutting intervals have
  #       instantaneous intersections.
  expect_phint_equal(phint_intersect(int23, int12), int22)
  expect_phint_equal(phint_intersect(int12, int23), int22)
  expect_phint_equal(
    phint_intersect(int12, phint_complement(int12)),
    phint_squash(c(int11, int22))
  )
})

# phint_complement -------------------------------------------------------------

# TODO:

# phint_overlaps ---------------------------------------------------------------

# TODO:

# phint_within -----------------------------------------------------------------

# TODO: phint_within() isn't implemented yet
if (FALSE) {

test_that("phint_within() empty input results in empty output", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  int <- interval(t1, t2)
  phint <- as_phinterval(int)
  empty_posixct <- lubridate::POSIXct()

  expect_identical(phint_within(empty_posixct, phint), logical())
  expect_identical(phint_within(t1, phinterval()), logical())
  expect_identical(phint_within(empty_posixct, phinterval()), logical())

  expect_identical(phint_within(int, phinterval()), logical())
  expect_identical(phint_within(phint, interval()), logical())
  expect_identical(phint_within(phinterval(), interval()), logical())
  expect_identical(phint_within(interval(), phinterval()), logical())
})

test_that("phint_within() NA input results in NA output", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t_na <- na_posixct(tzone = "UTC")

  int <- interval(t1, t2, tzone = "UTC")
  int_na <- interval(t_na, t_na, tzone = "UTC")

  phint <- as_phinterval(int)
  ph_na <- na_phinterval(tzone = "UTC")

  expect_true(phint_within(is.na(t1, ph_na)))
  expect_true(phint_within(is.na(t_na, phint)))
  expect_true(phint_within(is.na(t_na, ph_na)))
  expect_equal(phint_within(is.na(t1, c(phint, ph_na))), c(FALSE, TRUE))
  expect_equal(phint_within(is.na(t_na, c(phint, ph_na))), c(TRUE, TRUE))
  expect_equal(phint_within(is.na(t_na, c(ph_na, ph_na))), c(TRUE, TRUE))

  expect_true(phint_within(is.na(phint, int_na)))
  expect_true(phint_within(is.na(int, ph_na)))
  expect_true(phint_within(is.na(ph_na, int_na)))
  expect_true(phint_within(is.na(int_na, ph_na)))
  expect_equal(phint_within(is.na(int, c(ph_na, phint))), c(TRUE, FALSE))
  expect_equal(phint_within(is.na(int_na, c(phint, ph_na))), c(TRUE, TRUE))
  expect_equal(phint_within(is.na(int_na, c(ph_na, ph_na))), c(TRUE, TRUE))

  expect_true(phint_within(is.na(ph_na, phint)))
  expect_true(phint_within(is.na(phint, ph_na)))
  expect_true(phint_within(is.na(ph_na, ph_na)))
  expect_equal(phint_within(is.na(phint, c(ph_na, phint))), c(TRUE, FALSE))
  expect_equal(phint_within(is.na(ph_na, c(phint, phint))), c(TRUE, TRUE))
  expect_equal(phint_within(is.na(ph_na, c(phint, ph_na))), c(TRUE, TRUE))
})

test_that("phint_within(<phinterval>, <phinterval>) works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:02:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:03:00", tz = "UTC")

  int12 <- interval(t1, t2)
  int23 <- interval(t2, t3)
  int34 <- interval(t3, t4)
  int45 <- interval(t4, t5)
  int56 <- interval(t5, t6)
  int14 <- interval(t1, t4)
  int24 <- interval(t2, t4)
  int25 <- interval(t2, t5)
  int36 <- interval(t3, t6)

  # Self within self
  expect_true(phint_within(int12, int12))
  expect_true(phint_within(phinterval(c(int12, int45)), phinterval(c(int12, int45))))

  expect_equal(
    phint_within(rep(int12, 2), rep(int12, 2)),
    c(TRUE, TRUE)
  )
  expect_equal(
    phint_within(phinterval(c(int12, int45)), rep(phinterval(c(int12, int45)), 2)),
    c(TRUE, TRUE)
  )
  expect_equal(
    phint_within(rep(phinterval(c(int12, int45)), 2), phinterval(c(int12, int45))),
    c(TRUE, TRUE)
  )

  # Within a portion of a larger phinterval
  expect_true(phint_within(int12, phinterval(c(int12, int45))))
  expect_equal(
    phint_within(rep(int12, 3), phinterval(c(int12, int45))),
    c(TRUE, TRUE, TRUE)
  )
  expect_equal(
    phint_within(int12, rep(phinterval(c(int12, int45)), 2)),
    c(TRUE, TRUE)
  )

  # Partial intersection (not fully within)
  expect_false(phint_within(int36, int25))
  expect_false(phint_within(phinterval(c(int24, int56)), int25))
  expect_false(phint_within(int25, phinterval(c(int24, int56))))

  # No intersection
  expect_false(phint_within(int12, int56))
  expect_false(phint_within(phinterval(c(int12, int34)), int56))
  expect_false(phint_within(int56, phinterval(c(int12, int34))))
})

test_that("phint_within(<datetime>, <phinterval>) works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-07 00:02:00", tz = "UTC")

  time_in <- as.POSIXct("2021-01-03 00:00:00", tz = "UTC")
  time_out <- as.POSIXct("2021-06-13 00:00:45", tz = "UTC")
  date_in <- as.Date("2021-01-03")
  date_out <- as.Date("2021-06-13")

  phint <- phinterval(interval(t1, t2))

  expect_true(phint_within(date_in, phint))
  expect_true(phint_within(time_in, phint))
  expect_false(phint_within(date_out, phint))
  expect_false(phint_within(time_out, phint))
  expect_true(phint_within(t1, phint))
  expect_true(phint_within(t2, phint))
})

}

# is_hole ----------------------------------------------------------------------

# TODO

# n_spans ----------------------------------------------------------------------

# TODO

# phint_to_spans ---------------------------------------------------------------

# TODO

# phint_lengths ----------------------------------------------------------------

# TODO
