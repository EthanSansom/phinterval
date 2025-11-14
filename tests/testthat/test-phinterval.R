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

  expect_equal(get_tzone(phint), "EST")
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

  # Mixed inputs of <phinterval> and <Interval> are handled identically
  phint1 <- phinterval(intervals)
  phint2 <- phinterval(map(intervals, as_phinterval))
  phint3 <- phinterval(c(intervals[1:3], list(as_phinterval(intervals[[4]]))))
  phint4 <- phinterval(c(list(as_phinterval(intervals[[1]])), intervals[2:4]))

  expect_equal(phint_to_spans(phint1), intervals)
  expect_length(phint1, length(intervals))
  expect_s3_class(phint1, "phinterval")
  expect_equal(get_tzone(phint1), "EST")
  expect_equal(phint1, phint2)
  expect_equal(phint2, phint3)
  expect_equal(phint3, phint4)
})

# print ------------------------------------------------------------------------

test_that("phintervals are formatted as expected", {
  origin <- lubridate::origin
  phint1 <- phinterval(interval(origin, origin + 10))
  phint2 <- phinterval(interval(origin + seq(0, 20, 10), origin + 5 + seq(0, 20, 10)))
  na_phint <- phinterval(interval(NA, NA))
  hole <- phinterval(interval())

  # Generic printing
  expect_snapshot(print(c(phint1, phint2, na_phint, hole)))
  expect_snapshot(print(phinterval(interval(origin, origin + 86400))))

  # Full width
  expect_snapshot(print(phint2, max_width = 9999))

  # Truncating printed output
  op <- options(max.print = 1)
  on.exit(options(op), add = TRUE, after = FALSE)
  expect_snapshot(print(c(phint1, phint2, na_phint, hole)))

  # Invalid max_width options
  expect_error(format(phinterval(), max_width = "A"))
  expect_error(format(phinterval(), max_width = 10.5))
})

# arithmetic -------------------------------------------------------------------

test_that("<phinterval> / <Duration> works as expected.", {
  three_days <- phinterval(interval(as.Date("2021-01-01"), as.Date("2021-01-04")))
  ten_seconds <- phinterval(interval(lubridate::origin, lubridate::origin + 10))

  expect_equal(three_days / duration(days = 1), 3)
  expect_equal(ten_seconds / duration(seconds = 1), 10)
  expect_equal(
    phint_squash(c(three_days, ten_seconds)) / duration(seconds = 1),
    (86400 * 3) + 10
  )

  expect_equal(three_days %/% duration(days = 2), 1)
  expect_equal(ten_seconds %/% duration(seconds = 3), 3)

  expect_error(ten_seconds + duration(seconds = 3))
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

  # Errors on bad inputs
  expect_error(as_phinterval(10))
  expect_error(as_phinterval(t1))
})

# as_duration ------------------------------------------------------------------

test_that("as_duration() works as expected", {
  origin <- lubridate::origin
  int00 <- interval(origin, origin)
  int10 <- interval(origin + 5, origin + 15)
  int20 <- interval(origin + 20, origin + 40)
  na_int <- interval(NA, NA)
  hole <- phinterval(interval())

  expect_s4_class(as_duration(int00), "Duration")
  expect_s4_class(as_duration(phinterval(c(int10, int20))), "Duration")

  expect_equal(
    as_duration(c(int00, int10, na_int)),
    lubridate::as.duration(c(int00, int10, na_int))
  )
  expect_equal(
    as_duration(phinterval(c(int10, int20))),
    lubridate::dseconds(10 + 20)
  )
  expect_equal(
    as_duration(phinterval(list(hole, na_int, int20, int00))),
    lubridate::dseconds(c(0, NA, 20, 0))
  )
})

# is_phinterval ----------------------------------------------------------------

test_that("is_phinterval() works as expected", {
  expect_true(is_phinterval(phinterval()))
  expect_false(is_phinterval(interval()))
})

# is_phintish ------------------------------------------------------------------

test_that("is_phintish() works as expected", {
  expect_true(is_phintish(phinterval()))
  expect_true(is_phintish(interval()))
  expect_false(is_phintish(lubridate::origin))
})

# is_hole ----------------------------------------------------------------------

test_that("is_hole() works as expected", {
  hole <- phinterval(interval())
  phint <- phinterval(interval(as.Date("2021-01-01"), as.Date("2021-01-02")))
  na_phint <- phinterval(interval(NA, NA))

  expect_equal(is_hole(c(hole, phint, na_phint)), c(TRUE, FALSE, NA))
  expect_equal(is_hole(phinterval()), logical())
  expect_error(is_hole(as.Date("2021-01-01")))
})

# phint_start ------------------------------------------------------------------

test_that("phint_start() empty input results in empty output", {
  phint <- phinterval(tzone = "EST")
  start <- phint_start(phint)

  expect_s3_class(start, "POSIXct")
  expect_length(start, 0L)
  expect_equal(get_tzone(start), "EST")
})

test_that("phint_start() NA input results in NA output", {
  int1 <- interval(NA, NA)
  int2 <- rep(int1, 3)
  phint <- phinterval(list(int1, int2), tzone = "GMT")
  start <- phint_start(phint)

  expect_true(all(is.na(start)))
  expect_s3_class(start, "POSIXct")
  expect_length(start, 2L)
  expect_equal(get_tzone(start), "GMT")
})

test_that("phint_start() starts are correct", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:30", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:10:50", tz = "UTC")

  int1 <- interval(c(t3, t1), c(t4, t2))
  int2 <- interval(t2, t4)
  phint <- phinterval(list(int1, int2))
  start <- phint_start(phint)

  expect_equal(phint_start(phint), c(t1, t2))
  expect_equal(phint_start(int1), c(t3, t1))
  expect_equal(get_tzone(start), "UTC")

  expect_error(phint_start(as.Date("2020-01-05")))
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
  expect_true(all(map_lgl(starts, \(s) get_tzone(s) == "GMT")))
})

test_that("phint_starts() starts are correct", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "EST")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "EST")
  t3 <- as.POSIXct("2021-01-01 00:10:30", tz = "EST")
  t4 <- as.POSIXct("2021-01-01 00:10:50", tz = "EST")

  int <- interval(c(t3, t1), c(t4, t2))
  phint <- phint_squash(int)

  expect_equal(phint_starts(phint), list(c(t1, t3)))
  expect_equal(phint_starts(int), list(t3, t1))

  expect_error(phint_starts(as.Date("2020-01-05")))
})

# phint_end --------------------------------------------------------------------

test_that("phint_end() empty input results in empty output", {
  phint <- phinterval(tzone = "EST")
  end <- phint_end(phint)

  expect_s3_class(end, "POSIXct")
  expect_length(end, 0L)
  expect_equal(get_tzone(end), "EST")
})

test_that("phint_end() NA input results in NA output", {
  int1 <- interval(NA, NA)
  int2 <- rep(int1, 3)
  phint <- phinterval(list(int1, int2), tzone = "GMT")
  end <- phint_end(phint)

  expect_true(all(is.na(end)))
  expect_s3_class(end, "POSIXct")
  expect_length(end, 2L)
  expect_equal(get_tzone(end), "GMT")
})

test_that("phint_end() ends are correct", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:30", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:10:50", tz = "UTC")

  int1 <- interval(c(t3, t1), c(t4, t2))
  int2 <- interval(t2, t4)
  phint <- phinterval(list(int2, int1), tzone = "EST")
  end <- phint_end(phint)

  expect_equal(as.numeric(end), as.numeric(c(t4, t4)))
  expect_equal(get_tzone(end), "EST")

  expect_error(phint_end(as.Date("2020-01-05")))
})

# phint_ends -------------------------------------------------------------------

test_that("phint_ends() empty input results in empty list", {
  expect_identical(phint_ends(phinterval()), list())
})

test_that("phint_ends() NA input results in NA output", {
  int1 <- interval(NA, NA)
  int2 <- rep(int1, 3)
  phint <- phinterval(list(int1, int2), tzone = "GMT")
  ends <- phint_ends(phint)

  expect_true(all(map_lgl(ends, is.na)))
  expect_true(all(map_lgl(ends, lubridate::is.POSIXct)))
  expect_length(ends, 2L)
  expect_true(all(map_lgl(ends, \(e) get_tzone(e) == "GMT")))
})

test_that("phint_ends() ends are correct", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "EST")
  t2 <- as.POSIXct("2023-01-01 00:00:00", tz = "EST")
  t3 <- as.POSIXct("2024-11-15 00:00:15", tz = "EST")
  t4 <- as.POSIXct("2025-01-01 00:10:50", tz = "EST")

  int <- interval(c(t3, t1), c(t4, t2))
  phint <- phint_squash(int)

  expect_equal(phint_ends(phint), list(c(t2, t4)))
  expect_equal(phint_ends(int), list(t4, t2))

  expect_error(phint_ends(as.Date("2020-01-05")))
})

# phint_length -----------------------------------------------------------------

test_that("phint_length() empty input results in empty output", {
  expect_identical(phint_length(interval()), numeric())
  expect_identical(phint_length(phinterval()), numeric())
})

test_that("phint_length() NA input results in NA output", {
  expect_identical(phint_length(interval(NA, NA)), NA_real_)
  expect_identical(phint_length(phinterval(interval(NA, NA))), NA_real_)
})

test_that("phint_length() works as expected", {
  origin <- lubridate::origin
  int00 <- interval(origin, origin)
  int10 <- interval(origin, origin + 10)
  int20 <- interval(origin + 20, origin + 40)
  na_int <- interval(NA, NA)
  hole <- phinterval(interval())

  expect_identical(phint_length(c(int00, int10, na_int)), c(0, 10, NA))
  expect_identical(phint_length(as_phinterval(c(int00, int10, na_int))), c(0, 10, NA))
  expect_identical(phint_length(phint_squash(c(int10, int20))), 10 + 20)
  expect_identical(phint_length(phint_complement(hole)), Inf)
  expect_identical(phint_length(hole), 0)

  expect_error(phint_length("A"))
})

# phint_lengths -----------------------------------------------------------------

test_that("phint_lengths() empty input results in empty list", {
  expect_identical(phint_lengths(interval()), list())
  expect_identical(phint_lengths(phinterval()), list())
})

test_that("phint_lengths() NA input results in NA output", {
  expect_identical(phint_lengths(interval(NA, NA)), list(NA_real_))
  expect_identical(phint_lengths(phinterval(interval(NA, NA))), list(NA_real_))
})

test_that("phint_lengths() works as expected", {
  origin <- lubridate::origin
  int00 <- interval(origin, origin)
  int10 <- interval(origin + 5, origin + 15)
  int20 <- interval(origin + 20, origin + 40)
  na_int <- interval(NA, NA)
  hole <- phinterval(interval())

  expect_identical(phint_lengths(c(int00, int10, na_int)), list(0, 10, NA_real_))
  expect_identical(phint_lengths(phint_squash(c(int00, int10, int20))), list(c(0, 10, 20)))
  expect_identical(phint_lengths(hole), list(0))

  expect_error(phint_lengths("A"))
})

# n_spans ----------------------------------------------------------------------

test_that("n_spans() works as expected", {
  int1 <- interval(as.Date("2021-01-01"), as.Date("2021-01-02"))
  int2 <- interval(as.Date("2021-01-05"), as.Date("2021-01-20"))

  hole <- phinterval(interval())
  phint1 <- phinterval(int1)
  phint2 <- phinterval(c(int1, int2))
  na_phint <- phinterval(interval(NA, NA))

  expect_equal(n_spans(c(hole, phint1, phint2, na_phint)), c(0L, 1L, 2L, NA))
  expect_equal(n_spans(c(int1, int2, interval(NA, NA))), c(1L, 1L, NA))

  expect_identical(n_spans(phinterval()), integer())
  expect_identical(n_spans(interval()), integer())

  expect_error(n_spans(as.Date("2021-01-01")))
})

# phint_to_spans ---------------------------------------------------------------

test_that("phint_to_spans() empty input results in empty list", {
  expect_identical(phint_to_spans(interval()), list())
  expect_identical(phint_to_spans(phinterval()), list())
})

test_that("phint_to_spans() NA input results in NA output", {
  expect_identical(phint_to_spans(interval(NA, NA)), list(interval(NA, NA)))
  expect_identical(phint_to_spans(phinterval(interval(NA, NA))), list(interval(NA, NA)))
})

test_that("phint_to_spans() errors on invalid inputs", {
  expect_error(phint_to_spans(as.Date("2021-01-01")))
  expect_error(phint_to_spans(interval(NA, NA), hole_to = "span"))
})

# TODO
test_that("phint_to_spans() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")

  int12 <- interval(t1, t2)
  int23 <- interval(t2, t3)
  int34 <- interval(t3, t4)
  hole <- phinterval(interval())

  # Spans
  expect_identical(phint_to_spans(c(int12, int23)), list(int12, int23))
  expect_identical(
    phint_to_spans(c(phint_squash(c(int12, int34)), interval(t1, t1))),
    list(c(int12, int34), interval(t1, t1))
  )

  # Holes
  expect_identical(phint_to_spans(hole), list(interval()))
  expect_identical(phint_to_spans(hole, hole_to = "empty"), list(interval()))
  expect_identical(phint_to_spans(hole, hole_to = "na"), list(interval(NA, NA)))
  expect_identical(phint_to_spans(hole, hole_to = "null"), list(NULL))
})

# phint_sift -------------------------------------------------------------------

test_that("phint_sift() empty input results in empty output", {
  expect_phint_equal(phint_sift(interval()), phinterval())
  expect_phint_equal(phint_sift(phinterval()), phinterval())
})

test_that("phint_sift() NA input results in NA output", {
  expect_phint_equal(phint_sift(interval(NA, NA)), interval(NA, NA))
  expect_phint_equal(phint_sift(phinterval(interval(NA, NA))), interval(NA, NA))
})

test_that("phint_sift() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:30", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:10:50", tz = "UTC")

  int12 <- interval(t1, t2)
  int34 <- interval(t3, t4)
  hole <- phinterval(interval())

  # No instants
  expect_phint_equal(phint_sift(int12), int12)
  expect_phint_equal(
    phint_sift(phinterval(c(int12, int34))),
    phinterval(c(int12, int34))
  )

  # With instants
  expect_phint_equal(phint_sift(interval(t1, t1)), hole)
  expect_phint_equal(
    phint_sift(phinterval(interval(c(t1, t4), c(t1, t4)))),
    hole
  )
  expect_phint_equal(
    phint_sift(phinterval(interval(c(t1, t2, t3), c(t1, t2, t4)))),
    int34
  )
})

# phint_invert -----------------------------------------------------------------

test_that("phint_invert() empty input results in empty output", {
  expect_phint_equal(phint_invert(interval()), phinterval())
  expect_phint_equal(phint_invert(phinterval()), phinterval())
})

test_that("phint_invert() NA input results in NA output", {
  expect_phint_equal(phint_invert(interval(NA, NA)), interval(NA, NA))
  expect_phint_equal(phint_invert(phinterval(interval(NA, NA))), interval(NA, NA))
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
  hole <- phinterval(interval())

  # Single spans
  expect_phint_equal(phint_invert(c(int23, int34, int11)), rep(hole, 3))
  expect_phint_equal(phint_invert(as_phinterval(c(int23, int34, int11))), rep(hole, 3))

  # Holey intervals
  expect_phint_equal(phint_invert(phint_squash(c(int12, int34))), int23)
  expect_phint_equal(phint_invert(phint_squash(c(int11, int34))), interval(t1, t3))
  expect_phint_equal(phint_invert(phint_squash(c(int23, int44))), interval(t3, t4))
  expect_phint_equal(
    phint_invert(phint_squash(c(int12, int34, interval(t5, t6)))),
    phint_squash(c(int23, interval(t4, t5)))
  )

  # Holes
  expect_phint_equal(phint_invert(hole), hole)
  expect_phint_equal(phint_invert(hole, hole_to = "hole"), hole)
  expect_phint_equal(phint_invert(hole, hole_to = "na"), interval(NA, NA))
  expect_phint_equal(phint_invert(hole, hole_to = "inf"), phint_complement(hole))
})

# phint_squash -----------------------------------------------------------------

test_that("phint_squash() respects empty_to argument", {
  phint <- phinterval()
  int <- interval()

  empty <- phinterval()
  na <- phinterval(interval(NA, NA))
  hole <- phinterval(interval())

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
  na_phint <- phinterval(na_int)
  phint <- phinterval(c(int1, int2))

  expect_phint_equal(phint_squash(na_int, na.rm = TRUE), na_int)
  expect_phint_equal(phint_squash(na_int, na.rm = FALSE), na_int)
  expect_phint_equal(phint_squash(na_phint, na.rm = TRUE), na_phint)
  expect_phint_equal(phint_squash(na_phint, na.rm = FALSE), na_phint)

  expect_phint_equal(phint_squash(c(int1, int2, na_int), na.rm = TRUE), phint)
  expect_phint_equal(phint_squash(c(int1, int2, na_int), na.rm = FALSE), na_int)
  expect_phint_equal(phint_squash(c(na_int, int1), na.rm = TRUE), int1)
  expect_phint_equal(phint_squash(c(na_int, int1), na.rm = FALSE), na_int)
})

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

  hole <- phinterval(interval())

  # Squash scalar
  expect_phint_equal(phint_squash(int22), int22)
  expect_phint_equal(phint_squash(int12), int12)
  expect_phint_equal(phint_squash(phinterval(int12)), int12)
  expect_identical(phint_squash(hole), hole)

  # Squash abutting
  expect_phint_equal(phint_squash(c(int12, int23)), interval(t1, t3))
  expect_phint_equal(phint_squash(phinterval(c(int46, int14))), interval(t1, t6))

  # Squash overlapping
  expect_phint_equal(phint_squash(c(int12, int14)), int14)
  expect_phint_equal(phint_squash(c(int12, int14)), int14)
  expect_phint_equal(phint_squash(phinterval(c(int12, int22))), int12)
  expect_phint_equal(phint_squash(c(int14, int35)), interval(t1, t5))
  expect_phint_equal(phint_squash(c(int46, int35)), interval(t3, t6))

  # Squash non-overlapping
  expect_phint_equal(phint_squash(c(int12, int34, int45)), phinterval(c(int12, int34, int45)))
  expect_phint_equal(phint_squash(c(hole, int12, int34)), phinterval(c(int12, int34)))
  expect_phint_equal(phint_squash(c(int45, int22)), phinterval(c(int22, int45)))
})

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
  expect_phint_equal(phint_union(phinterval(), phinterval()), phinterval())
  expect_phint_equal(phint_union(interval(), interval()), phinterval())
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

  hole <- phinterval(interval())

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

test_that("phint_setdiff() empty input results in empty output", {
  expect_phint_equal(phint_setdiff(interval(), interval()), interval())
})

test_that("phint_setdiff() NA input results in NA output", {
  int <- interval(as.Date("2021-01-01"), as.Date("2021-02-01"))
  expect_phint_equal(phint_setdiff(interval(NA, NA), int), interval(NA, NA))
  expect_phint_equal(phint_setdiff(int, interval(NA, NA)), interval(NA, NA))
})

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

  hole <- phinterval(interval())

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
    phint_setdiff(interval(t2, t5), phint_squash(c(int13, interval(t4, t6)))),
    phint_squash(int34)
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

test_that("phint_intersect() empty input results in empty output", {
  expect_phint_equal(phint_intersect(interval(), interval()), interval())
})

test_that("phint_intersect() NA input results in NA output", {
  int <- interval(as.Date("2021-01-01"), as.Date("2021-02-01"))
  expect_phint_equal(phint_intersect(interval(NA, NA), int), interval(NA, NA))
  expect_phint_equal(phint_intersect(int, interval(NA, NA)), interval(NA, NA))
})

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

  hole <- phinterval(interval())

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

  # Intersection with abutting.
  # Intersection is endpoint inclusive, so abutting intervals intersect.
  expect_phint_equal(phint_intersect(int23, int12), int22)
  expect_phint_equal(phint_intersect(int12, int23), int22)
  expect_phint_equal(
    phint_intersect(int12, phint_complement(int12)),
    phint_squash(c(int11, int22))
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
  expect_phint_equal(
    phint_complement(int23),
    manual_phinterval(
      starts = c(-Inf, as.numeric(t3)),
      ends = c(as.numeric(t2), Inf)
    )
  )
  expect_phint_equal(
    phint_complement(phint_squash(c(int12, int34))),
    manual_phinterval(
      starts = c(-Inf, as.numeric(t2), as.numeric(t4)),
      ends = c(as.numeric(t1), as.numeric(t3), Inf)
    )
  )

  # Input with one infinite endpoint
  expect_phint_equal(
    phint_complement(manual_phinterval(as.numeric(t1), Inf)),
    manual_phinterval(-Inf, as.numeric(t1))
  )
  expect_phint_equal(
    phint_complement(manual_phinterval(-Inf, as.numeric(t1))),
    manual_phinterval(as.numeric(t1), Inf)
  )

  # Holes and infinite intervals
  expect_phint_equal(phint_complement(hole), manual_phinterval(-Inf, Inf))
  expect_phint_equal(phint_complement(manual_phinterval(-Inf, Inf)), hole)
})

# phint_overlaps ---------------------------------------------------------------

test_that("phint_overlaps() empty input results in empty output", {
  expect_identical(phint_within(interval(), interval()), logical())
  expect_identical(phint_within(phinterval(), phinterval()), logical())
})

test_that("phint_overlaps() NA input results in NA output", {
  na_int <- interval(NA, NA)
  int <- interval(as.Date("2020-01-01"), as.Date("2022-01-02"))

  expect_equal(
    is.na(phint_overlaps(c(int, na_int, int), c(na_int, na_int, int))),
    c(TRUE, TRUE, FALSE)
  )
})

test_that("phint_overlaps() recycles inputs", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:30:00", tz = "UTC")

  phint0 <- phinterval()
  phint1 <- phinterval(interval(t1, t2))
  phint3 <- as_phinterval(interval(c(t1, t3, t4), c(t2, t4, t4)))

  expect_equal(
    phint_overlaps(phint1, phint3),
    phint_overlaps(rep(phint1, 3), phint3)
  )
  expect_equal(
    phint_overlaps(phint0, phint1),
    logical()
  )
  expect_error(phint_overlaps(rep(phint1, 2), phint3))
  expect_error(phint_overlaps(phint3, phint0))
})

test_that("phint_overlaps() works as expected", {
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
  int25 <- interval(t2, t5)
  int36 <- interval(t3, t6)
  int22 <- interval(t2, t2)

  hole <- phinterval(interval())

  # Self overlap
  expect_true(phint_overlaps(int12, int12))
  expect_true(phint_overlaps(int22, int22))
  expect_true(phint_overlaps(phinterval(c(int12, int45)), phinterval(c(int12, int45))))
  expect_false(phint_overlaps(hole, hole))

  # Partial overlap
  expect_true(phint_overlaps(int12, int14))
  expect_true(phint_overlaps(int14, int12))
  expect_true(phint_overlaps(int25, int36))
  expect_true(phint_overlaps(int36, int25))

  # Non-overlapping
  expect_false(phint_overlaps(int12, int34))
  expect_false(phint_overlaps(int12, int56))
  expect_false(phint_overlaps(phinterval(c(int12, int34)), int56))
  expect_false(phint_overlaps(int56, phinterval(c(int12, int34))))

  # Abutting
  expect_true(phint_overlaps(int12, int23))
  expect_true(phint_overlaps(int23, int12))
  expect_true(phint_overlaps(int22, int22))

  # Hole
  expect_false(phint_overlaps(hole, int22))
  expect_false(phint_overlaps(int23, hole))
})

# phint_within -----------------------------------------------------------------

test_that("phint_within() recycles inputs", {
  t0 <- lubridate::POSIXct()
  d0 <- lubridate::as_date(t0)
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:30:00", tz = "UTC")

  phint0 <- phinterval()
  phint1 <- phinterval(interval(t1, t2))
  phint3 <- as_phinterval(interval(c(t1, t3, t4), c(t2, t4, t4)))

  expect_equal(
    phint_within(phint1, phint3),
    phint_within(rep(phint1, 3), phint3)
  )
  expect_equal(
    phint_within(t1, phint3),
    phint_within(rep(t1, 3), phint3)
  )
  expect_equal(phint_within(t0, phint1), logical())
  expect_equal(phint_within(d0, phint1), logical())
  expect_equal(phint_within(phint0, phint1), logical())

  expect_error(phint_within(rep(t1, 2), phint3))
  expect_error(phint_within(rep(phint1, 2), phint3))
  expect_error(phint_within(phint3, phint0))
})

test_that("phint_within() empty input results in empty output", {
  empty_posixct <- lubridate::POSIXct()
  empty_date <- lubridate::as_date(empty_posixct)
  empty_int <- interval()

  expect_identical(phint_within(empty_posixct, empty_int), logical())
  expect_identical(phint_within(empty_date, empty_int), logical())
  expect_identical(phint_within(empty_int, empty_int), logical())
})

test_that("phint_within() NA input results in NA output", {
  t <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  na_t <- lubridate::NA_POSIXct_
  int <- interval(as.Date("2020-01-01"), as.Date("2020-01-02"))
  na_int <- interval(na_t, na_t)

  expect_equal(
    is.na(phint_within(c(int, na_int, int), c(na_int, na_int, int))),
    c(TRUE, TRUE, FALSE)
  )
  expect_equal(
    is.na(phint_within(c(t, na_t, t), c(na_int, int, int))),
    c(TRUE, TRUE, FALSE)
  )
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

test_that("phint_within(<datetime>, <phinterval>) works with large phintervals", {
  origin <- lubridate::origin
  phint <- phint_squash(interval(origin + seq(0, 100, 10), origin + 5 + seq(0, 100, 10)))

  time_in <- origin + 2
  time_out <- origin + 27
  date_in <- as.Date(origin)
  date_out <- as.Date("2021-06-13")

  expect_true(phint_within(date_in, phint))
  expect_true(phint_within(time_in, phint))
  expect_false(phint_within(date_out, phint))
  expect_false(phint_within(time_out, phint))
  expect_true(phint_within(origin, phint))
})
