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

  # `order_by`
  expect_error(phinterval(date1, date1, order_by = NA))
  expect_error(phinterval(date1, date1, order_by = "yes"))
})

test_that("phinterval() tzone argument overrides start/end timezones", {
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
  t_utc <- lubridate::with_tz(as.POSIXct("2021-01-03 00:00:00"), "UTC")
  t_est <- lubridate::with_tz(as.POSIXct("2021-01-03 00:00:00"), "EST")
  t_lcl <- lubridate::with_tz(as.POSIXct("2021-01-03 00:00:00"), "")

  phint_rok <- phinterval(t_utc, t_est, tzone = "ROK")
  phint_est <- phinterval(t_est, t_utc, tzone = "EST")
  phint_utc <- phinterval(t_est, t_utc, tzone = "UTC")
  phint_lcl <- phinterval(t_lcl, t_lcl, tzone = "")

  expect_true(all(phint_rok == phint_est))
  expect_true(all(phint_utc == phint_est))
  expect_true(all(phint_lcl == phint_est))

  expect_equal(get_tzone(phint_rok), "ROK")
  expect_equal(get_tzone(phint_est), "EST")
  expect_equal(get_tzone(phint_utc), "UTC")
  expect_equal(get_tzone(phint_lcl), "")

  # First time zone is used by default, unless the time zone is "local", in which
  # case the non-local time zone is used.
  expect_equal(phinterval(t_est, t_utc), phint_est)
  expect_equal(phinterval(t_lcl, t_utc), phint_utc)
  expect_equal(phinterval(t_utc, t_lcl), phint_utc)
})

test_that("phinterval() coerces invalid start/end timezones to UTC", {
  # Skipping because `NA` timezones generate un-handled warnings
  skip_on_cran()

  t_na_zone <- as.POSIXct(10, tz = NA_character_)
  t_utc <- lubridate::with_tz(t_na_zone, "UTC")
  t_lcl <- lubridate::with_tz(t_na_zone, "")

  expect_equal(phinterval(t_na_zone, t_na_zone), phinterval(t_utc, t_utc))
})

test_that("phinterval() handles infinite inputs correctly", {
  time_finite <- as.POSIXct(10, tz = "UTC")
  time_neg_inf <- as.POSIXct(-Inf, tz = "UTC")
  time_pos_inf <- as.POSIXct(Inf, tz = "UTC")
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")

  phint_finite_pos_inf <- phinterval(time_finite, time_pos_inf)
  expect_equal(phint_start(phint_finite_pos_inf), time_finite)
  expect_equal(phint_end(phint_finite_pos_inf), time_pos_inf)

  phint_finite_neg_inf <- phinterval(time_finite, time_neg_inf)
  expect_equal(phint_start(phint_finite_neg_inf), time_neg_inf)
  expect_equal(phint_end(phint_finite_neg_inf), time_finite)

  phint_pos_inf_finite <- phinterval(time_pos_inf, time_finite)
  expect_equal(phint_start(phint_pos_inf_finite), time_finite)
  expect_equal(phint_end(phint_pos_inf_finite), time_pos_inf)

  phint_neg_inf_finite <- phinterval(time_neg_inf, time_finite)
  expect_equal(phint_start(phint_neg_inf_finite), time_neg_inf)
  expect_equal(phint_end(phint_neg_inf_finite), time_finite)

  phint_pos_inf_pos_inf <- phinterval(time_pos_inf, time_pos_inf)
  expect_equal(phint_start(phint_pos_inf_pos_inf), time_pos_inf)
  expect_equal(phint_end(phint_pos_inf_pos_inf), time_pos_inf)

  phint_neg_inf_neg_inf <- phinterval(time_neg_inf, time_neg_inf)
  expect_equal(phint_start(phint_neg_inf_neg_inf), time_neg_inf)
  expect_equal(phint_end(phint_neg_inf_neg_inf), time_neg_inf)

  phint_pos_inf_neg_inf <- phinterval(time_pos_inf, time_neg_inf)
  expect_equal(phint_start(phint_pos_inf_neg_inf), time_neg_inf)
  expect_equal(phint_end(phint_pos_inf_neg_inf), time_pos_inf)

  phint_neg_inf_pos_inf <- phinterval(time_neg_inf, time_pos_inf)
  expect_equal(phint_start(phint_neg_inf_pos_inf), time_neg_inf)
  expect_equal(phint_end(phint_neg_inf_pos_inf), time_pos_inf)

  expect_equal(phint_pos_inf_neg_inf, phint_neg_inf_pos_inf)

  expect_equal(phinterval(NA_POSIXct_, time_pos_inf), na_phint)
  expect_equal(phinterval(time_pos_inf, NA_POSIXct_), na_phint)
  expect_equal(phinterval(NA_POSIXct_, time_neg_inf), na_phint)
  expect_equal(phinterval(time_neg_inf, NA_POSIXct_), na_phint)
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

test_that("phinterval() recycles its inputs", {
  t0 <- as.POSIXct(numeric(), tz = "UTC")
  t1 <- as.POSIXct(0, tz = "UTC")
  t3 <- as.POSIXct(1:3, tz = "UTC")

  expect_equal(phinterval(t1, t3), phinterval(rep(t1, 3), t3))
  expect_equal(phinterval(rep(t1, 3), t3), phinterval(t1, t3))
  expect_equal(phinterval(t0, t1), phinterval(t0, t0))
  expect_equal(phinterval(t1, t0), phinterval(t0, t0))

  expect_error(phinterval(t0, t3))
  expect_error(phinterval(t3, t0))
})

test_that("phinterval() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:04:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:08:00", tz = "UTC")

  phint <- phinterval(c(t1, t3, t6), c(t2, t3, t5))
  expect_equal(phint_start(phint), c(t1, t3, t5))
  expect_equal(phint_end(phint), c(t2, t3, t6))
})

test_that("phinterval() with `by` and `order_by` works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:04:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:08:00", tz = "UTC")

  expect_equal(
    phinterval(c(t1, t3, t5), c(t2, t4, t6), by = 1),
    datetime_squash(c(t1, t3, t5), c(t2, t4, t6))
  )
  expect_equal(
    phinterval(c(t1, t3, t5), c(t2, t4, t6), by = c(2, 2, 1), order_by = FALSE),
    datetime_squash(
      c(t1, t3, t5),
      c(t2, t4, t6),
      by = c(2, 2, 1),
      order_by = FALSE,
      na.rm = TRUE
    )
  )
  expect_equal(
    phinterval(c(t1, t3, t5), c(t2, t4, t6), by = c(2, 2, 1), order_by = TRUE),
    datetime_squash(
      c(t1, t3, t5),
      c(t2, t4, t6),
      by = c(2, 2, 1),
      order_by = TRUE,
      na.rm = TRUE
    )
  )
  expect_equal(
    phinterval(c(t1, t3, t5), c(t2, t4, t6), by = 3:1, order_by = FALSE),
    datetime_squash(
      c(t1, t3, t5),
      c(t2, t4, t6),
      by = 3:1,
      order_by = FALSE,
      na.rm = TRUE
    )
  )
  expect_equal(
    phinterval(c(t1, t3, t5), c(t2, t4, t6), by = 3:1, order_by = TRUE),
    datetime_squash(
      c(t1, t3, t5),
      c(t2, t4, t6),
      by = 3:1,
      order_by = TRUE,
      na.rm = TRUE
    )
  )
})

# casting and coercion ---------------------------------------------------------

test_that("Coercion via c() works as expected", {
  t1 <- as.POSIXct(1, tzone = "UTC")
  t2 <- as.POSIXct(10, tzone = "UTC")

  intvl_utc <- interval(t1, t2, tzone = "UTC")
  intvl_est <- interval(t1, t2, tzone = "EST")
  intvl_lcl <- interval(t1, t2, tzone = "")

  phint_utc <- phinterval(t1, t2, tzone = "UTC")
  phint_est <- phinterval(t1, t2, tzone = "EST")
  phint_lcl <- phinterval(t1, t2, tzone = "")

  # Coercion should take first time zone, unless "local". Note, not touching
  # lubridate:::c.Interval, so `c(<Interval>, <phinterval>)` is an error.
  expect_equal(c(phint_utc, phint_est), c(phint_utc, phint_utc))
  expect_equal(c(phint_lcl, phint_est), c(phint_est, phint_est))
  expect_equal(c(phint_est, phint_lcl), c(phint_est, phint_est))
  expect_equal(c(phint_utc, intvl_est), c(phint_utc, phint_utc))
  expect_equal(c(phint_lcl, intvl_est), c(phint_est, phint_est))

  expect_error(c(phint_utc, 10))
  expect_error(c(phint_utc, t1))
})

test_that("vec_cast() works as expected", {
  t1 <- as.POSIXct(1, tz = "UTC")
  t2 <- as.POSIXct(10, tz = "UTC")

  intvl_utc <- interval(t1, t2, tzone = "UTC")
  intvl_est <- interval(t1, t2, tzone = "EST")
  intvl_lcl <- interval(t1, t2, tzone = "")

  phint_utc <- phinterval(t1, t2, tzone = "UTC")
  phint_est <- phinterval(t1, t2, tzone = "EST")
  phint_lcl <- phinterval(t1, t2, tzone = "")

  # Casting always takes the time zone from `to` (including "local" time)
  expect_equal(vctrs::vec_cast(phint_utc, to = phint_utc), phint_utc)
  expect_equal(vctrs::vec_cast(phint_utc, to = phint_est), phint_est)
  expect_equal(vctrs::vec_cast(phint_utc, to = phint_lcl), phint_lcl)
  expect_equal(vctrs::vec_cast(intvl_utc, to = phint_utc), phint_utc)
  expect_equal(vctrs::vec_cast(intvl_utc, to = phint_est), phint_est)
  expect_equal(vctrs::vec_cast(intvl_utc, to = phint_lcl), phint_lcl)

  expect_error(vctrs::vec_cast(10, to = phint_utc))
  expect_error(vctrs::vec_cast(t1, to = phint_utc))
})

# print ------------------------------------------------------------------------

test_that("print() and pillar_shaft() work as expected", {
  skip_on_cran()

  d1 <- as.Date("2020-01-01")
  d2 <- as.Date("2020-02-17")
  d3 <- as.Date("2020-03-10")
  d4 <- as.Date("2021-09-15")

  t1 <- as.POSIXct(d1, tz = "UTC") + 17.005
  t2 <- as.POSIXct(d2, tz = "UTC") + 450.02456
  t3 <- as.POSIXct(d3, tz = "UTC") + 89.0000
  t4 <- as.POSIXct(d4, tz = "UTC") + 1005.0001
  t5 <- t4 + 13000

  t_neg_inf <- as.POSIXct(-Inf, tz = "UTC")
  t_pos_inf <- as.POSIXct(Inf, tz = "UTC")

  phint <- c(
    phinterval(d1, d2, tzone = "UTC"),                       # {<date> -- <date>}
    phinterval(t1, t2, tzone = "UTC"),                       # {<dttm> -- <dttm>}
    phinterval(c(t1, t3), c(t2, t4), by = 1, tzone = "UTC"), # {span, span}
    phinterval(t_neg_inf, t_pos_inf, tzone = "UTC"),
    phinterval(c(t_neg_inf, t3), c(t1, t_pos_inf), by = 1, tzone = "UTC"),
    hole(tzone = "UTC"),
    NA
  )
  phint_tib <- dplyr::tibble(phint = phint)

  # Respect `digits.secs`, as in `pillar:::pillar_shaft.POSIXt`
  local_options(phinterval.print_max_width = 120, width = 120, digits.secs = 4)
  expect_snapshot(print(phint))
  expect_snapshot(print(phint_tib))

  local_options(digits.secs = 0)

  local_options(phinterval.print_max_width = 90, width = 90)
  expect_snapshot(print(phint))
  expect_snapshot(print(phint_tib))

  local_options(phinterval.print_max_width = 60, width = 60)
  expect_snapshot(print(phint))
  expect_snapshot(print(phint_tib))

  local_options(phinterval.print_max_width = 30, width = 30)
  expect_snapshot(print(phint))
  expect_snapshot(print(phint_tib))
})

test_that("print() and pillar_shaft() emit warning on unrecognized time zone", {
  skip_on_cran()

  phint <- phinterval(as.Date(1), as.Date(2), tzone = "badzone")
  phint_tib <- dplyr::tibble(phint = phint)

  withr::defer(rlang::reset_warning_verbosity("phinterval_warning_unrecognized_tzone"))

  # Warning should only be emitted once per session (i.e. on first `print()`)
  rlang::reset_warning_verbosity("phinterval_warning_unrecognized_tzone")
  expect_snapshot(print(phint), cnd_class = TRUE)
  expect_snapshot(print(phint), cnd_class = TRUE)

  rlang::reset_warning_verbosity("phinterval_warning_unrecognized_tzone")
  expect_snapshot(print(phint_tib), cnd_class = TRUE)
  expect_snapshot(print(phint_tib), cnd_class = TRUE)
})

# arithmetic -------------------------------------------------------------------

test_that("<phinterval> / <Duration> works as expected.", {
  three_days <- phinterval(as.Date("2021-01-01"), as.Date("2021-01-04"))
  ten_seconds <- phinterval(lubridate::origin, lubridate::origin + 10)
  inf_seconds1 <- phinterval(as.POSIXct(-Inf), as.POSIXct(Inf))
  inf_seconds2 <- phinterval(as.POSIXct(0), as.POSIXct(Inf))

  expect_equal(three_days / duration(days = 1), 3)
  expect_equal(ten_seconds / duration(seconds = 1), 10)
  expect_equal(
    phint_squash(c(three_days, ten_seconds)) / duration(seconds = 1),
    (86400 * 3) + 10
  )
  expect_equal(inf_seconds1 / duration(seconds = 1), Inf)
  expect_equal(inf_seconds2 / duration(seconds = 1), Inf)

  expect_equal(three_days %/% duration(days = 2), 1)
  expect_equal(ten_seconds %/% duration(seconds = 3), 3)
  expect_equal(inf_seconds1 %/% duration(seconds = 3), Inf)
  expect_equal(inf_seconds2 %/% duration(seconds = 3), Inf)

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

  phint0 <- phinterval()
  phint1 <- phinterval(t1, t2)
  phint2 <- phinterval(c(t1, t3), c(t2, t4))

  ## <Interval>
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

test_that("as_phinterval() respects timezones", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:30", tz = "UTC")

  int_utc <- interval(t1, t2, tzone = "UTC")
  int_est <- interval(t1, t2, tzone = "EST")

  expect_equal(as_phinterval(int_est), phinterval(t1, t2, tzone = "EST"))
  expect_equal(as_phinterval(int_utc), phinterval(t1, t2, tzone = "UTC"))
  expect_equal(as_phinterval(lubridate::with_tz(t1, "EST")), phinterval(t1, t1, tzone = "EST"))
  expect_equal(as_phinterval(t1), phinterval(t1, t1, tzone = "UTC"))
  expect_equal(attr(as_phinterval(int_utc), "tzone"), "UTC")
  expect_equal(attr(as_phinterval(int_est), "tzone"), "EST")
})

test_that("as_phinterval() coerces invalid timezones to UTC", {
  # Skipping because `NA` timezones generate un-handled warnings
  skip_on_cran()

  t_na_zone <- as.POSIXct(0, tz = NA_character_)
  expect_equal(attr(as_phinterval(t_na_zone), "tzone"), "UTC")
})

test_that("as_phinterval() handles infinite instants correctly", {
  date_finite <- as.Date(1)
  time_finite <- as.POSIXct(10, tz = "UTC")

  date_neg_inf <- as.Date(-Inf)
  date_pos_inf <- as.Date(Inf)
  time_neg_inf <- as.POSIXct(-Inf, tz = "UTC")
  time_pos_inf <- as.POSIXct(Inf, tz = "UTC")

  expect_equal(as_phinterval(date_neg_inf), phinterval(time_neg_inf, time_neg_inf))
  expect_equal(as_phinterval(date_pos_inf), phinterval(time_pos_inf, time_pos_inf))
  expect_equal(as_phinterval(time_neg_inf), phinterval(time_neg_inf, time_neg_inf))
  expect_equal(as_phinterval(time_pos_inf), phinterval(time_pos_inf, time_pos_inf))
})

test_that("as_phinterval() handles infinite <Interval> vectors correctly", {
  # It doesn't seem like `interval()` officially supports infinite endpoints,
  # so this test is fragile.
  skip_on_cran()

  time_finite <- as.POSIXct(10, tz = "UTC")
  time_neg_inf <- as.POSIXct(-Inf, tz = "UTC")
  time_pos_inf <- as.POSIXct(Inf, tz = "UTC")

  intvl_finite_pos_inf <- interval(time_finite, time_pos_inf) # start=finite, span=Inf
  intvl_finite_neg_inf <- interval(time_finite, time_neg_inf) # start=finite, span=-Inf
  intvl_pos_inf_finite <- interval(time_pos_inf, time_finite) # start=Inf, span=-Inf
  intvl_neg_inf_finite <- interval(time_neg_inf, time_finite) # start=-Inf, span=Inf

  intvl_pos_inf_pos_inf <- interval(time_pos_inf, time_pos_inf) # start=Inf, span=NaN
  intvl_pos_inf_neg_inf <- interval(time_pos_inf, time_neg_inf) # start=Inf, span=-Inf
  intvl_neg_inf_pos_inf <- interval(time_neg_inf, time_pos_inf) # start=-Inf, span=Inf
  intvl_neg_inf_neg_inf <- interval(time_neg_inf, time_neg_inf) # start=-Inf, span=NaN

  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")

  expect_equal(as_phinterval(intvl_finite_pos_inf), phinterval(time_finite, time_pos_inf))
  expect_equal(as_phinterval(intvl_finite_neg_inf), phinterval(time_neg_inf, time_finite))
  expect_equal(as_phinterval(intvl_pos_inf_finite), phinterval(time_neg_inf, time_pos_inf))
  expect_equal(as_phinterval(intvl_neg_inf_finite), phinterval(time_neg_inf, time_pos_inf))
  expect_equal(as_phinterval(intvl_pos_inf_pos_inf), na_phint)
  expect_equal(as_phinterval(intvl_pos_inf_neg_inf), phinterval(time_neg_inf, time_pos_inf))
  expect_equal(as_phinterval(intvl_neg_inf_pos_inf), phinterval(time_neg_inf, time_pos_inf))
  expect_equal(as_phinterval(intvl_neg_inf_neg_inf), na_phint)
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

# is_recognized_tzone ----------------------------------------------------------

test_that("is_recognized_tzone() works as expected", {
  # Caution as {tzdb} database might change, although test is using common zones
  skip_on_cran()

  expect_true(is_recognized_tzone("UTC"))
  expect_true(is_recognized_tzone("WET"))
  expect_true(is_recognized_tzone(""))

  expect_false(is_recognized_tzone(NA_character_))
  expect_false(is_recognized_tzone("boop"))
  expect_false(is_recognized_tzone(c("WET", "UTC")))
  expect_false(is_recognized_tzone(10))
})
