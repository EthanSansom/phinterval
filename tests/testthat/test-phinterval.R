# phinterval -------------------------------------------------------------------

test_that("phinterval() returns empty vector", {
  phint <- phinterval()
  expect_s3_class(phint, "phinterval")
  expect_length(phint, 0L)

  phint <- phinterval(as.Date(character()), as.Date(character()))
  expect_s3_class(phint, "phinterval")
  expect_length(phint, 0L)
})

test_that("NA intervals result in NA output", {
  date <- as.Date("2020-01-01")
  phint <- phinterval(
    c(date, NA, date, NA),
    c(date, NA, NA, date)
  )
  expect_equal(is.na(phint), c(FALSE, TRUE, TRUE, TRUE))
})

test_that("phinterval() errors on invalid inputs", {
  date1 <- as.Date(1)
  date2 <- as.Date(1:2)
  date3 <- as.Date(1:3)

  # `start` and `end`
  expect_error(phinterval(date1, "A"))   # Type
  expect_error(phinterval(10, date1))    # Type
  expect_error(phinterval(date2, date3)) # Recycling

  # `tzone`
  expect_error(phinterval(date1, date1, tzone = letters))       # Length
  expect_error(phinterval(date1, date1, tzone = NA_character_)) # Missing
  expect_error(phinterval(date1, date1, tzone = 10))            # Type

  # `by`
  expect_error(phinterval(date1, date1, by = numeric())) # Recycling
  expect_error(phinterval(date1, date1, by = 1:2))       # Recycling
  expect_error(phinterval(date1, date1, by = mean))      # Type
})

test_that("phinterval() tzone argument overrides intervals timezone", {
  point <- as.POSIXct(0, tz = "UTC")
  phint <- phinterval(point, point, tzone = "EST")

  expect_equal(get_tzone(phint), "EST")
  expect_equal(
    as.numeric(point),
    as.numeric(phint_start(phint))
  )
  expect_equal(
    as.numeric(point),
    as.numeric(phint_end(phint))
  )
})

test_that("phinterval() handles DST transitions correctly", {
  # Only 1 hour elapses between 1:30 and 3:30 due to DST
  t1 <- as.POSIXct("2021-03-14 01:30:00", tz = "America/Toronto")
  t2 <- as.POSIXct("2021-03-14 03:30:00", tz = "America/Toronto")

  phint <- phinterval(t1, t2, tzone = "UTC")

  expect_equal(get_tzone(phint), "UTC")
  expect_equal(
    as.numeric(phint_end(phint) - phint_start(phint)),
    as.numeric(t2 - t1)
  )
})

test_that("phinterval() standardizes mixed timezones", {
  t_utc <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t_est <- as.POSIXct("2021-01-03 00:00:00", tz = "EST")

  phint_rok <- phinterval(t_utc, t_est, tzone = "ROK")
  phint_est <- phinterval(t_est, t_utc, tzone = "EST")
  phint_utc <- phinterval(t_est, t_utc, tzone = "UTC")

  expect_true(all(phint_rok == phint_est))
  expect_true(all(phint_utc == phint_est))
  expect_equal(get_tzone(phint_rok), "ROK")
  expect_equal(get_tzone(phint_est), "EST")
  expect_equal(get_tzone(phint_utc), "UTC")

  # First timezone is used by default
  expect_equal(phinterval(t_est, t_utc), phint_est)
})

test_that("phinterval() standardizes input times", {
  t00 <- as.POSIXct(0)
  t20 <- as.POSIXct(20)
  tna <- as.POSIXct(NA)

  expect_equal(
    phinterval(c(t20, t00, tna, tna, t20), c(t00, t20, t00, tna, tna)),
    phinterval(c(t00, t00, tna, tna, tna), c(t20, t20, tna, tna, tna))
  )
})

# TODO
test_that("phinterval() works as expected", {

})

# TODO
test_that("phinterval() with `by` works as expected", {

})

# print ------------------------------------------------------------------------

# TODO:
test_that("print() works as expected", {

})

# TODO:
test_that("pillar_shaft() works as expected", {

})

# arithmetic -------------------------------------------------------------------

test_that("<phinterval> / <Duration> works as expected.", {
  three_days <- phinterval(as.Date("2021-01-01"), as.Date("2021-01-04"))
  ten_seconds <- phinterval(lubridate::origin, lubridate::origin + 10)

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
  d1 <- as.Date("2021-01-01")
  t1 <- as.POSIXct(d1, tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:30", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:20", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:20:30", tz = "UTC")

  na_int1 <- interval(NA, NA)
  na_int2 <- interval(t1, NA)
  na_int3 <- interval(NA, t2)
  int0 <- interval()
  int1 <- interval(t1, t2)
  int2 <- interval(c(t1, t3), c(t2, t4))

  ## <Interval>

  phint0 <- phinterval()
  phint1 <- phinterval(t1, t2)
  phint2 <- phinterval(c(t1, t3), c(t2, t4))

  expect_equal(as_phinterval(int0), phinterval())
  expect_equal(as_phinterval(int1), phint1)
  expect_equal(as_phinterval(int2), phint2)
  expect_equal(
    as_phinterval(c(na_int1, na_int2, na_int3)),
    phinterval(rep(NA_POSIXct_, 3), rep(NA_POSIXct_, 3))
  )

  ## Instant
  expect_equal(as_phinterval(d1), as_phinterval(t1))
  expect_equal(as_phinterval(t1), phinterval(t1, t1))
  expect_equal(phint_start(as_phinterval(t1)), t1)
  expect_equal(phint_end(as_phinterval(t1)), t1)
  expect_equal(as_phinterval(c(t1, t2, NA)), phinterval(c(t1, t2, NA), c(t1, t2, NA)))
  expect_equal(as_phinterval(as.POSIXct(numeric(), tz = "UTC")), phinterval(tzone = "UTC"))

  ## Errors on bad inputs
  expect_error(as_phinterval(10))
  expect_error(as_phinterval("A"))

  ## Time zones are respected
  int3 <- interval(lubridate::with_tz(t1, "EST"), lubridate::with_tz(t2, "EST"))
  expect_equal(as_phinterval(int3), phinterval(t1, t2, tzone = "EST"))
  expect_equal(get_tzone(as_phinterval(int2)), "UTC")
  expect_equal(get_tzone(as_phinterval(int3)), "EST")
})

# as_duration ------------------------------------------------------------------

test_that("as_duration() works as expected", {
  origin <- lubridate::origin
  int00 <- interval(origin, origin)
  int10 <- interval(origin + 5, origin + 15)
  int20 <- interval(origin + 20, origin + 40)
  na_int <- interval(NA, NA)

  expect_s4_class(as_duration(int00), "Duration")
  expect_s4_class(as_duration(as_phinterval(c(int10, int20))), "Duration")

  expect_equal(
    as_duration(c(int00, int10, na_int)),
    lubridate::as.duration(c(int00, int10, na_int))
  )
  expect_equal(
    as_duration(phint_squash(c(int10, int20))),
    lubridate::dseconds(10 + 20)
  )
  expect_equal(
    as_duration(phinterval(origin, origin + c(0, NA, 20))),
    lubridate::dseconds(c(0, NA, 20))
  )
  expect_equal(as_duration(hole(2)), lubridate::dseconds(c(0, 0)))
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
  hole <- hole()
  phint <- phinterval(as.Date("2021-01-01"), as.Date("2021-01-02"))
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_)

  expect_equal(is_hole(c(hole, phint, na_phint)), c(TRUE, FALSE, NA))
  expect_equal(is_hole(phinterval()), logical())
  expect_error(is_hole(as.Date("2021-01-01")))
})

# phint_start/end --------------------------------------------------------------

test_that("phint_start/end() empty input results in empty output", {
  phint <- phinterval(tzone = "EST")
  start <- phint_start(phint)
  end <- phint_end(phint)

  expect_s3_class(start, "POSIXct")
  expect_length(start, 0L)
  expect_equal(get_tzone(start), "EST")

  expect_s3_class(end, "POSIXct")
  expect_length(end, 0L)
  expect_equal(get_tzone(end), "EST")
})

test_that("phint_start/end() NA input results in NA output", {
  nas <- c(NA_POSIXct_, NA_POSIXct_)
  phint <- phinterval(nas, nas, tzone = "GMT")
  start <- phint_start(phint)
  end <- phint_end(phint)

  expect_true(all(is.na(start)))
  expect_s3_class(start, "POSIXct")
  expect_length(start, 2L)
  expect_equal(get_tzone(start), "GMT")

  expect_true(all(is.na(end)))
  expect_s3_class(end, "POSIXct")
  expect_length(end, 2L)
  expect_equal(get_tzone(end), "GMT")
})

test_that("phint_start/end() starts/ends are correct", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:30", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:10:50", tz = "UTC")

  intvl <- phinterval(c(t3, t1), c(t4, t2))
  phint <- phinterval(c(t3, t1, t2), c(t4, t2, t4), by = c(2, 2, 1))
  start <- phint_start(phint)

  expect_equal(phint_start(phint), c(t1, t2))
  expect_equal(phint_start(intvl), c(t3, t1))
  expect_equal(get_tzone(start), "UTC")

  expect_equal(phint_end(phint), c(t4, t4))
  expect_equal(phint_end(intvl), c(t4, t2))
  expect_equal(get_tzone(start), "UTC")

  expect_error(phint_start(as.Date("2020-01-05")))
  expect_error(phint_end(as.Date("2020-01-05")))
})

# phint_starts/ends ------------------------------------------------------------

test_that("phint_starts/ends() empty input results in empty list", {
  expect_identical(phint_starts(phinterval()), list())
  expect_identical(phint_ends(phinterval()), list())
})

test_that("phint_starts/ends() NA input results in NA output", {
  nas <- c(NA_POSIXct_, NA_POSIXct_)
  phint <- phinterval(nas, nas, tzone = "GMT")
  starts <- phint_starts(phint)
  ends <- phint_ends(phint)

  expect_true(all(map_lgl(starts, is.na)))
  expect_true(all(map_lgl(starts, lubridate::is.POSIXct)))
  expect_length(starts, 2L)
  expect_true(all(map_lgl(starts, \(s) get_tzone(s) == "GMT")))

  expect_true(all(map_lgl(ends, is.na)))
  expect_true(all(map_lgl(ends, lubridate::is.POSIXct)))
  expect_length(ends, 2L)
  expect_true(all(map_lgl(ends, \(s) get_tzone(s) == "GMT")))
})

test_that("phint_starts/ends() starts are correct", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "EST")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "EST")
  t3 <- as.POSIXct("2021-01-01 00:10:30", tz = "EST")
  t4 <- as.POSIXct("2021-01-01 00:10:50", tz = "EST")

  intvl <- interval(c(t3, t1), c(t4, t2))
  phint <- phint_squash(intvl)

  expect_equal(phint_starts(phint), list(c(t1, t3)))
  expect_equal(phint_starts(intvl), list(t3, t1))
  expect_equal(phint_ends(phint), list(c(t2, t4)))
  expect_equal(phint_ends(intvl), list(t4, t2))

  expect_error(phint_starts(as.Date("2020-01-05")))
  expect_error(phint_ends(as.Date("2020-01-05")))
})

# phint_length -----------------------------------------------------------------

test_that("phint_length() empty input results in empty output", {
  expect_identical(phint_length(interval()), numeric())
  expect_identical(phint_length(phinterval()), numeric())
})

test_that("phint_length() NA input results in NA output", {
  expect_identical(phint_length(interval(NA, NA)), NA_real_)
  expect_identical(phint_length(as_phinterval(NA_POSIXct_)), NA_real_)
})

test_that("phint_length() works as expected", {
  origin <- lubridate::origin
  int00 <- interval(origin, origin)
  int10 <- interval(origin, origin + 10)
  int20 <- interval(origin + 20, origin + 40)
  na_int <- interval(NA, NA)

  expect_identical(phint_length(c(int00, int10, na_int)), c(0, 10, NA))
  expect_identical(phint_length(as_phinterval(c(int00, int10, na_int))), c(0, 10, NA))
  expect_identical(phint_length(phint_squash(c(int10, int20))), 10 + 20)
  expect_identical(phint_length(phint_complement(hole())), Inf)
  expect_identical(phint_length(hole(2)), c(0, 0))

  expect_error(phint_length("A"))
})

# phint_lengths -----------------------------------------------------------------

test_that("phint_lengths() empty input results in empty list", {
  expect_identical(phint_lengths(interval()), list())
  expect_identical(phint_lengths(phinterval()), list())
})

test_that("phint_lengths() NA input results in NA output", {
  expect_identical(phint_lengths(interval(NA, NA)), list(NA_real_))
  expect_identical(phint_lengths(phinterval(NA_POSIXct_, NA_POSIXct_)), list(NA_real_))
})

test_that("phint_lengths() works as expected", {
  origin <- lubridate::origin
  int00 <- interval(origin, origin)
  int10 <- interval(origin + 5, origin + 15)
  int20 <- interval(origin + 20, origin + 40)
  na_int <- interval(NA, NA)

  expect_identical(
    phint_lengths(c(int00, int10, na_int)),
    list(0, 10, NA_real_)
  )
  expect_identical(
    phint_lengths(as_phinterval(c(int00, int10, na_int))),
    list(0, 10, NA_real_)
  )
  expect_identical(
    phint_lengths(phint_squash(c(int00, int10, int20))),
    list(c(0, 10, 20))
  )
  expect_identical(phint_lengths(hole(2)), list(0, 0))
  expect_identical(phint_lengths(phint_complement(hole(2))), list(Inf, Inf))

  expect_error(phint_lengths("A"))
})

# n_spans ----------------------------------------------------------------------

test_that("n_spans() works as expected", {
  int1 <- interval(as.Date("2021-01-01"), as.Date("2021-01-02"))
  int2 <- interval(as.Date("2021-01-05"), as.Date("2021-01-20"))

  hole <- hole()
  phint1 <- as_phinterval(int1)
  phint2 <- phint_squash(c(int1, int2))
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_)

  expect_equal(n_spans(c(hole, phint1, phint2, na_phint)), c(0L, 1L, 2L, NA))
  expect_equal(n_spans(c(int1, int2, interval(NA, NA))), c(1L, 1L, NA))

  expect_identical(n_spans(phinterval()), integer())
  expect_identical(n_spans(interval()), integer())

  expect_error(n_spans(as.Date("2021-01-01")))
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

# binary dispatch --------------------------------------------------------------

test_that("Binary operations work on <Interval> x <phinterval> combinations", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")

  intvl13 <- interval(t1, t3)
  intvl24 <- interval(t2, t4)
  phint13 <- phinterval(t1, t3)
  phint24 <- phinterval(t2, t4)

  ## Set operations
  res_union <- list(
    phint_phint = phint_union(phint13, phint24),
    phint_intvl = phint_union(phint13, intvl24),
    intvl_phint = phint_union(intvl13, phint24),
    intvl_intvl = phint_union(intvl13, intvl24)
  )
  res_intersect <- list(
    phint_phint = phint_intersect(phint13, phint24),
    phint_intvl = phint_intersect(phint13, intvl24),
    intvl_phint = phint_intersect(intvl13, phint24),
    intvl_intvl = phint_intersect(intvl13, intvl24)
  )
  res_setdiff <- list(
    phint_phint = phint_setdiff(phint13, phint24),
    phint_intvl = phint_setdiff(phint13, intvl24),
    intvl_phint = phint_setdiff(intvl13, phint24),
    intvl_intvl = phint_setdiff(intvl13, intvl24)
  )

  expect_all_true(map_lgl(res_union[-1], ~ .x == res_union[[1]]))
  expect_all_true(map_lgl(res_intersect[-1], ~ .x == res_intersect[[1]]))
  expect_all_true(map_lgl(res_setdiff[-1], ~ .x == res_setdiff[[1]]))

  expect_error(phint_union(phint13, t1))
  expect_error(phint_union(t1, phint13))
  expect_error(phint_intersect(phint13, t1))
  expect_error(phint_intersect(t1, phint13))
  expect_error(phint_setdiff(phint13, t1))
  expect_error(phint_setdiff(t1, phint13))

  ## Relations
  res_overlaps <- list(
    phint_phint = phint_overlaps(phint13, phint24),
    phint_intvl = phint_overlaps(phint13, intvl24),
    intvl_phint = phint_overlaps(intvl13, phint24),
    intvl_intvl = phint_overlaps(intvl13, intvl24)
  )
  res_within <- list(
    phint_phint = phint_within(phint13, phint24),
    phint_intvl = phint_within(phint13, intvl24),
    intvl_phint = phint_within(intvl13, phint24),
    intvl_intvl = phint_within(intvl13, intvl24)
  )

  expect_all_true(map_lgl(res_overlaps[-1], ~ .x == res_overlaps[[1]]))
  expect_all_true(map_lgl(res_within[-1], ~ .x == res_within[[1]]))

  expect_error(phint_overlaps(phint13, t1))
  expect_error(phint_overlaps(t1, phint13))
})

test_that("phint_within(<datetime>, <phintish>) combinations work", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")

  expect_true(phint_within(t2, phinterval(t1, t3)))
  expect_true(phint_within(t2, interval(t1, t3)))
  expect_false(phint_within(t4, phinterval(t1, t3)))
  expect_false(phint_within(t4, interval(t1, t3)))

  expect_error(phint_within(phinterval(t1, t3), t2))
  expect_error(phint_within(interval(t1, t3), t2))
})

# binary operations ------------------------------------------------------------

test_that("Binary operations empty input results in empty output", {
  expect_all_true(
    map_lgl(
      list(
        phint_union(phinterval(), phinterval()),
        phint_intersect(phinterval(), phinterval()),
        phint_setdiff(phinterval(), phinterval()),
      ),
      identical,
      phinterval()
    )
  )

  expect_all_true(
    map_lgl(
      list(
        phint_within(phinterval(), phinterval()),
        phint_within(lubridate::POSIXct(), interval()),
        phint_overlaps(phinterval(), phinterval()),
      ),
      identical,
      logical()
    )
  )
})

test_that("Binary operations NA input results in NA output", {
  t1 <- as.POSIXct("2021-01-01", tz = "UTC")
  t2 <- as.POSIXct("2021-01-02", tz = "UTC")

  point <- c(t1, NA, NA, NA, NA)
  intvl <- interval(c(t1, t1, NA, t1, NA), c(t2, t2, t2, NA, NA))
  phint <- phinterval(c(t1, NA, t1, NA, NA), c(t2, NA, t2, NA, NA))

  expect_all_true(
    map_lgl(
      list(
        is.na(phint_union(intvl, phint)),
        is.na(phint_intersect(intvl, phint)),
        is.na(phint_setdiff(intvl, phint)),
        is.na(phint_within(intvl, phint)),
        is.na(phint_overlaps(intvl, phint)),
        is.na(phint_union(phint, intvl)),
        is.na(phint_intersect(phint, intvl)),
        is.na(phint_setdiff(phint, intvl)),
        is.na(phint_within(phint, intvl)),
        is.na(phint_overlaps(phint, intvl)),
        is.na(phint_within(point, phint)),
        is.na(phint_within(point, intvl))
      ),
      identical,
      c(FALSE, TRUE, TRUE, TRUE, TRUE)
    )
  )
})

test_that("Binary operations recycle inputs", {
  t0 <- lubridate::POSIXct(tz = "UTC")
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:30:00", tz = "UTC")

  phint0 <- phinterval()
  phint1 <- phinterval(t1, t2)
  phint3 <- phinterval(c(t1, t3, t4), c(t2, t4, t4))

  expect_equal(phint_union(phint1, phint3), phint_union(rep(phint1, 3), phint3))
  expect_equal(phint_union(phint3, phint1), phint_union(phint3, rep(phint1, 3)))
  expect_equal(phint_union(phint0, phint1), phint0)
  expect_equal(phint_union(phint1, phint0), phint0)

  expect_equal(phint_intersect(phint1, phint3), phint_intersect(rep(phint1, 3), phint3))
  expect_equal(phint_intersect(phint3, phint1), phint_intersect(phint3, rep(phint1, 3)))
  expect_equal(phint_intersect(phint0, phint1), phint0)
  expect_equal(phint_intersect(phint1, phint0), phint0)

  expect_equal(phint_setdiff(phint1, phint3), phint_setdiff(rep(phint1, 3), phint3))
  expect_equal(phint_setdiff(phint3, phint1), phint_setdiff(phint3, rep(phint1, 3)))
  expect_equal(phint_setdiff(phint0, phint1), phint0)
  expect_equal(phint_setdiff(phint1, phint0), phint0)

  expect_equal(phint_overlaps(phint1, phint3), phint_overlaps(rep(phint1, 3), phint3))
  expect_equal(phint_overlaps(phint3, phint1), phint_overlaps(phint3, rep(phint1, 3)))
  expect_equal(phint_overlaps(phint0, phint1), logical())
  expect_equal(phint_overlaps(phint1, phint0), logical())

  expect_equal(phint_within(phint1, phint3), phint_within(rep(phint1, 3), phint3))
  expect_equal(phint_within(phint3, phint1), phint_within(rep(phint1, 3), phint3))
  expect_equal(phint_within(t1, phint3), phint_within(rep(t1, 3), phint3))
  expect_equal(phint_within(phint0, phint1), logical())
  expect_equal(phint_within(phint1, phint0), logical())
  expect_equal(phint_within(t0, phint1), logical())

  expect_error(phint_union(rep(phint1, 2), phint3))
  expect_error(phint_union(phint3, phint0))
  expect_error(phint_within(rep(phint1, 2), phint3))
  expect_error(phint_within(phint3, phint0))
})

# phint_union ------------------------------------------------------------------

test_that("phint_union() errors on invalid inputs", {
  int <- interval(as.Date("2021-01-01"), as.Date("2021-02-01"))
  phint <- as_phinterval(int)

  expect_error(phint_union(int, 10))
  expect_error(phint_union("A", phint))
  expect_error(phint_union(phint))
})

test_that("phint_union() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:04:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:08:00", tz = "UTC")

  int12 <- phinterval(t1, t2)
  int23 <- phinterval(t2, t3)
  int34 <- phinterval(t3, t4)
  int25 <- phinterval(t2, t5)
  int36 <- phinterval(t3, t6)

  int22 <- phinterval(t2, t2)

  hole <- hole(tzone = "UTC")

  # Self union
  expect_equal(phint_union(int12, int12), int12)
  expect_equal(phint_union(int22, int22), int22)

  # Union with hole
  expect_equal(phint_union(int12, hole), int12)
  expect_equal(phint_union(hole, int12), int12)
  expect_equal(phint_union(rep(phinterval(int12), 3), rep(hole, 3)), rep(int12, 3))
  expect_equal(phint_union(rep(hole, 3), rep(phinterval(int12), 3)), rep(int12, 3))

  # Union of non-overlapping
  expect_equal(phint_union(int12, int34), phint_squash(c(int12, int34)))
  expect_equal(
    phint_union(
      phint_squash(c(int12, int34)),
      int25
    ),
    phint_squash(c(phinterval(t1, t5), int34))
  )

  # Union of overlapping or abutting
  expect_equal(phint_union(int12, phinterval(t1, t3)), phinterval(t1, t3))
  expect_equal(phint_union(int25, int36), phinterval(t2, t6))
  expect_equal(phint_union(int23, int34), phinterval(t2, t4))

  # Union with instant
  expect_equal(phint_union(int22, int12), int12)
  expect_equal(phint_union(int12, int22), int12)
  expect_equal(phint_union(int12, int34), phint_squash(c(int12, int34)))
})

# phint_setdiff ----------------------------------------------------------------

# Set-difference assumes exclusive `()` intervals (unlike union and intersection)
test_that("phint_setdiff() works as expected", {
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
  int36 <- phinterval(t3, t6)
  int22 <- phinterval(t2, t2)

  hole <- hole(tzone = "UTC")

  # Self difference
  expect_equal(phint_setdiff(int12, int12), hole)
  expect_equal(phint_setdiff(rep(int12, 3), rep(int12, 3)), rep(hole, 3))

  # Difference with hole
  expect_equal(phint_setdiff(int12, hole), int12)
  expect_equal(phint_setdiff(hole, int12), hole)
  expect_equal(phint_setdiff(rep(int12, 3), rep(hole, 3)), rep(int12, 3))
  expect_equal(phint_setdiff(rep(hole, 3), rep(int12, 3)), rep(hole, 3))

  # Difference with non-overlapping
  expect_equal(phint_setdiff(int23, int45), int23)
  expect_equal(phint_setdiff(int45, int23), int45)
  expect_equal(
    phint_setdiff(phint_squash(c(int12, int56)), int34),
    phint_squash(c(int12, int56))
  )

  # Difference with overlapping
  expect_equal(phint_setdiff(int13, int12), int23)
  expect_equal(phint_setdiff(int12, int13), hole)
  expect_equal(
    phint_setdiff(phint_squash(c(int12, int45)), int12),
    int45
  )
  expect_equal(
    phint_setdiff(phint_squash(c(int13, int45)), int12),
    phint_squash(c(int23, int45))
  )
  expect_equal(
    phint_setdiff(phinterval(t2, t5), phint_squash(c(int13, phinterval(t4, t6)))),
    phint_squash(int34)
  )
  expect_equal(
    phint_setdiff(int25, int34),
    phint_squash(c(int23, int45))
  )
  expect_equal(phint_setdiff(int25, int36), int23)
  expect_equal(phint_setdiff(int36, int25), int56)

  # Difference with instants
  expect_equal(phint_setdiff(int12, int22), int12)
  expect_equal(phint_setdiff(int22, int12), int22)
  expect_equal(phint_setdiff(int13, int22), int13)
  expect_equal(phint_setdiff(int22, int13), hole)
})

# phint_intersect --------------------------------------------------------------

# TODO: YOU ARE HERE, there are some failing cases
test_that("phint_intersect() works as expected", {
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
  int35 <- phinterval(t3, t5)
  int36 <- phinterval(t3, t6)
  int16 <- phinterval(t1, t6)

  int11 <- phinterval(t1, t1)
  int22 <- phinterval(t2, t2)

  hole <- hole(tzone = "UTC")

  # Self intersection
  expect_equal(phint_intersect(int12, int12), int12)
  expect_equal(phint_intersect(rep(int12, 3), rep(int12, 3)), rep(int12, 3))

  # Intersection with hole
  expect_equal(phint_intersect(int12, hole), hole)
  expect_equal(phint_intersect(hole, int12), hole)
  expect_equal(phint_intersect(rep(int12, 3), rep(hole, 3)), rep(hole, 3))
  expect_equal(phint_intersect(rep(hole, 3), rep(int12, 3)), rep(hole, 3))

  # Intersection with non-overlapping
  expect_equal(phint_intersect(int23, int45), hole)
  expect_equal(phint_intersect(int45, int23), hole)
  expect_equal(
    phint_intersect(phint_squash(c(int12, int56)), int34),
    hole
  )
  expect_equal(
    phint_intersect(
      c(phint_squash(c(int12, int56)), int34),
      c(int34, int12)
    ),
    c(hole, hole)
  )

  # Intersection with overlapping
  expect_equal(phint_intersect(int13, int12), int12)
  expect_equal(phint_intersect(int12, int13), int12)
  expect_equal(phint_intersect(int25, int36), int35)
  expect_equal(phint_intersect(int36, int25), int35)
  expect_equal(
    phint_intersect(phint_squash(c(int12, int45)), int12),
    int12
  )
  expect_equal(
    phint_intersect(phint_squash(c(int13, int45)), phint_squash(c(int12, int45))),
    phint_squash(c(int12, int45))
  )
  expect_equal(phint_intersect(int25, int34), int34)

  expect_equal(
    phint_intersect(int16, phint_squash(c(int12, int34, int56))),
    phint_squash(c(int12, int34, int56))
  )
  expect_equal(
    phint_intersect(phint_squash(c(int12, int34, int56)), int16),
    phint_squash(c(int12, int34, int56))
  )

  # Intersection with instants
  expect_equal(phint_intersect(int22, int22), int22)
  expect_equal(phint_intersect(int12, int22), int22)
  expect_equal(phint_intersect(int22, int12), int22)
  expect_equal(phint_intersect(int13, int22), int22)
  expect_equal(phint_intersect(int22, int13), int22)

  # Intersection with abutting.
  # Intersection is endpoint inclusive, so abutting intervals intersect.
  expect_equal(phint_intersect(int23, int12), int22)
  expect_equal(phint_intersect(int12, int23), int22)
  expect_equal(
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

# phint_unnest -----------------------------------------------------------------

# TODO

# datetime_squash --------------------------------------------------------------

# TODO

# is_valid_tzone ---------------------------------------------------------------

# TODO
