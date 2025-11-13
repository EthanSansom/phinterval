# phinterval -------------------------------------------------------------------

test_that("phinterval() returns zero-length vector", {
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

  int0 <- interval()
  int1 <- interval(t1, t2)
  int2 <- interval(c(t1, t3), c(t2, t4))

  phint0 <- phinterval()
  phint1 <- phinterval(list(int1))
  phint2 <- phinterval(list(interval(t1, t2), interval(t3, t4)))

  expect_equal(as_phinterval(int0), phinterval())
  expect_equal(as_phinterval(int1), phint1)
  expect_equal(as_phinterval(int2), phint2)
})

# phint_start ------------------------------------------------------------------

test_that("phint_start() zero-length input results in zero-length output", {
  phint <- phinterval(tzone = "EST")
  start <- phint_start(phint)

  expect_s3_class(start, "POSIXct")
  expect_length(start, 0L)
  expect_equal(lubridate::tz(start), "EST")
})

test_that("phint_start() NA input results in NA output", {
  int1 <- interval(NA, NA)
  int2 <- rep(int1, 3)
  phint <- phinterval(list(int1, int2), tzone = "PST")
  start <- phint_start(phint)

  expect_true(all(is.na(start)))
  expect_s3_class(start, "POSIXct")
  expect_length(start, 2L)
  expect_equal(lubridate::tz(start), "PST")
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

test_that("phint_start() and int_start() are equivilant with Interval input", {
  t1 <- as.POSIXct("2021-01-01 00:00:00")
  t2 <- as.POSIXct("2021-01-01 00:10:00")
  t3 <- as.POSIXct("2021-01-01 00:10:30")
  t4 <- as.POSIXct("2021-01-01 00:10:50")
  t_NA <- lubridate::NA_POSIXct_

  int_empty <- interval()
  int_na <- interval(t_NA, t_NA, tzone = "EST")
  int1 <- interval(c(t3, t1), c(t4, t2), tzone = "ROC")
  int2 <- interval(t2, t4, tzone = "UTC")
  int3 <- interval(c(t_NA, t1, t_NA), c(t_NA, t3, t_NA), tzone = "CET")
  int4 <- interval(t1, t1, tzone = "WET")

  expect_equal(lubridate::int_start(int_empty), phint_start(int_empty))
  expect_equal(lubridate::int_start(int_na), phint_start(int_na))
  expect_equal(lubridate::int_start(int1), phint_start(int1))
  expect_equal(lubridate::int_start(int2), phint_start(int2))
  expect_equal(lubridate::int_start(int3), phint_start(int3))
  expect_equal(lubridate::int_start(int4), phint_start(int4))
})

# phint_starts -----------------------------------------------------------------

test_that("phint_starts() zero-length input results in zero-length list", {
  phint <- phinterval()
  starts <- phint_starts(phint)

  expect_identical(starts, list())
  expect_length(starts, 0L)
})

test_that("phint_starts() NA input results in NA output", {
  int1 <- interval(NA, NA)
  int2 <- rep(int1, 3)
  phint <- phinterval(list(int1, int2), tzone = "PST")
  starts <- phint_starts(phint)

  expect_true(all(map_lgl(starts, is.na)))
  expect_true(all(map_lgl(starts, lubridate::is.POSIXct)))
  expect_length(starts, 2L)
  expect_true(all(map_lgl(starts, \(s) lubridate::tz(s) == "PST")))

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

# phint_squash -----------------------------------------------------------------

# TODO:

# phint_union ------------------------------------------------------------------

# TODO:

# phint_setdiff ----------------------------------------------------------------

# TODO:
# - recycling
# - NA's
# - 0-length inputs
# - error on bad inputs

test_that("phint_setdiff() works as expected", {
  expect_phint_equal <- function(object, expected, ...) {
    expect_equal(
      as_phinterval(object),
      as_phinterval(expected),
      label = rlang::caller_arg(object),
      expected.label = rlang::caller_arg(expected),
      ...
    )
  }

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

  phint12 <- phinterval(int12)
  phint23 <- phinterval(int23)
  phint34 <- phinterval(int34)
  phint45 <- phinterval(int45)
  phint56 <- phinterval(int56)
  phint13 <- phinterval(int13)

  hole <- phinterval(list(interval()))

  # Self difference
  expect_phint_equal(phint_setdiff(int12, int12), hole)
  expect_phint_equal(phint_setdiff(phint12, phint12), hole)
  expect_phint_equal(phint_setdiff(int12, phint12), hole)
  expect_phint_equal(phint_setdiff(phint12, int12), hole)
  expect_phint_equal(phint_setdiff(rep(phint12, 3), rep(phint12, 3)), rep(hole, 3))

  # Difference with hole
  expect_phint_equal(phint_setdiff(phint12, hole), phint12)
  expect_phint_equal(phint_setdiff(hole, phint12), hole)
  expect_phint_equal(phint_setdiff(int12, hole), phint12)
  expect_phint_equal(phint_setdiff(hole, int12), hole)
  expect_phint_equal(phint_setdiff(rep(phint12, 3), rep(hole, 3)), rep(phint12, 3))
  expect_phint_equal(phint_setdiff(rep(hole, 3), rep(phint12)), rep(hole, 3))

  # Difference with non-Intersection
  expect_phint_equal(phint_setdiff(int23, int45), phint23)
  expect_phint_equal(phint_setdiff(int45, int23), phint45)
  expect_phint_equal(phint_setdiff(phint45, phint23), phint45)
  expect_phint_equal(phint_setdiff(phint23, phint45), phint23)
  expect_phint_equal(
    phint_setdiff(phinterval(c(int12, int56)), int34),
    phinterval(c(int12, int56))
  )
  expect_phint_equal(
    phint_setdiff(int34, phinterval(c(int12, int56))),
    phint34
  )
  expect_phint_equal(
    phint_setdiff(
      c(phinterval(c(int12, int56)), phint34),
      c(int34, int12)
    ),
    c(phinterval(c(int12, int56)), phint34)
  )

  # Difference with intersection
  expect_phint_equal(phint_setdiff(int13, int12), phint23)
  expect_phint_equal(phint_setdiff(int12, int13), hole)
  expect_phint_equal(phint_setdiff(phint13, phint12), phint23)
  expect_phint_equal(phint_setdiff(phint12, phint13), hole)
  expect_phint_equal(
    phint_setdiff(phint_squash(c(phint12, phint45)), phint12),
    phint45
  )
  expect_phint_equal(
    phint_setdiff(phint12, phint_squash(c(phint12, phint45))),
    hole
  )
  expect_phint_equal(
    phint_setdiff(phint_squash(c(phint13, phint45)), phint12),
    phint_squash(c(phint23, phint45))
  )
  expect_phint_equal(
    phint_setdiff(phint12, phint_squash(c(phint13, phint45))),
    hole
  )
  expect_phint_equal(
    phint_setdiff(int25, int34),
    phint_squash(c(int23, int45))
  )
  expect_phint_equal(phint_setdiff(int25, int36), phint23)
  expect_phint_equal(phint_setdiff(int36, int25), phint56)

  # Difference with instants
  expect_phint_equal(phint_setdiff(int12, int22), int12)
  expect_phint_equal(phint_setdiff(int22, int12), hole)
  expect_phint_equal(phint_setdiff(int12, int12), hole)
  expect_phint_equal(phint_setdiff(int13, int22), int13)
  expect_phint_equal(phint_setdiff(int22, int13), hole)
})

# phint_intersect --------------------------------------------------------------

# TODO:
# - recycling
# - NA's
# - 0-length inputs
# - error on bad inputs

test_that("phint_intersect() works as expected", {
  expect_phint_equal <- function(object, expected, ...) {
    expect_equal(
      as_phinterval(object),
      as_phinterval(expected),
      label = rlang::caller_arg(object),
      expected.label = rlang::caller_arg(expected),
      ...
    )
  }

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

  int11 <- interval(t1, t1)
  int22 <- interval(t2, t2)

  phint12 <- phinterval(int12)
  phint23 <- phinterval(int23)
  phint34 <- phinterval(int34)
  phint45 <- phinterval(int45)
  phint56 <- phinterval(int56)
  phint13 <- phinterval(int13)
  phint35 <- phinterval(int35)
  phint16 <- phinterval(c(int13, int36))

  hole <- phinterval(list(interval()))

  # Self intersection
  expect_phint_equal(phint_intersect(int12, int12), phint12)
  expect_phint_equal(phint_intersect(phint12, phint12), phint12)
  expect_phint_equal(phint_intersect(int12, phint12), phint12)
  expect_phint_equal(phint_intersect(phint12, int12), phint12)
  expect_phint_equal(phint_intersect(rep(phint12, 3), rep(phint12, 3)), rep(phint12, 3))

  # Intersect with hole
  expect_phint_equal(phint_intersect(phint12, hole), hole)
  expect_phint_equal(phint_intersect(hole, phint12), hole)
  expect_phint_equal(phint_intersect(int12, hole), hole)
  expect_phint_equal(phint_intersect(hole, int12), hole)
  expect_phint_equal(phint_intersect(rep(phint12, 3), rep(hole, 3)), rep(hole, 3))
  expect_phint_equal(phint_intersect(rep(hole, 3), rep(phint12)), rep(hole, 3))

  # Intersect with Non-Overlapping
  expect_phint_equal(phint_intersect(int23, int45), hole)
  expect_phint_equal(phint_intersect(int45, int23), hole)
  expect_phint_equal(phint_intersect(phint45, phint23), hole)
  expect_phint_equal(phint_intersect(phint23, phint45), hole)
  expect_phint_equal(
    phint_intersect(phinterval(c(int12, int56)), int34),
    hole
  )
  expect_phint_equal(
    phint_intersect(int34, phinterval(c(int12, int56))),
    hole
  )
  expect_phint_equal(
    phint_intersect(
      c(phinterval(c(int12, int56)), phint34),
      c(int34, int12)
    ),
    c(hole, hole)
  )

  # Intersection with overlapping
  expect_phint_equal(phint_intersect(int13, int12), phint12)
  expect_phint_equal(phint_intersect(int12, int13), phint12)
  expect_phint_equal(phint_intersect(phint13, phint12), phint12)
  expect_phint_equal(phint_intersect(phint12, phint13), phint12)
  expect_phint_equal(phint_intersect(int25, int36), phint35)
  expect_phint_equal(phint_intersect(int36, int25), phint35)

  expect_phint_equal(
    phint_intersect(phint_squash(c(phint12, phint45)), phint12),
    phint12
  )
  expect_phint_equal(
    phint_intersect(phint12, phint_squash(c(phint12, phint45))),
    phint12
  )
  expect_phint_equal(
    phint_intersect(phint_squash(c(phint13, phint45)), phint_squash(c(phint12, phint45))),
    phint_squash(c(phint12, phint45))
  )
  expect_phint_equal(phint_intersect(int25, int34), phint34)
  expect_phint_equal(phint_intersect(int25, int36), phint35)
  expect_phint_equal(phint_intersect(int36, int25), phint35)

  expect_phint_equal(
    phint_intersect(phint16, phint_squash(c(int12, int34, int56))),
    phint_squash(c(int12, int34, int56))
  )
  expect_phint_equal(
    phint_intersect(phint_squash(c(int12, int34, int56)), phint16),
    phint_squash(c(int12, int34, int56))
  )
  expect_phint_equal(
    phint_intersect(phint16, phint_squash(c(int12, int34, int56))),
    phint_squash(c(int12, int34, int56))
  )

  # Intersection with instant
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
# - works as expected (can be taken from `_diff` and `_intersect`)
# - recycling
# - NA's
# - 0-length inputs
# - error on bad inputs

# phint_within -----------------------------------------------------------------

# TODO: phint_within() isn't implemented yet
if (FALSE) {

test_that("phint_within() zero-length input results in zero-length output", {
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

  # self within self
  expect_true(phint_within(int12, as_phinterval(int12)))
  expect_true(phint_within(as_phinterval(int12), int12))
  expect_true(phint_within(as_phinterval(int12), as_phinterval(int12)))
  expect_true(phint_within(phinterval(c(int12, int45)), phinterval(c(int12, int45))))

  expect_equal(
    phint_within(rep(as_phinterval(int12), 2), rep(int12, 2)),
    c(TRUE, TRUE)
  )
  expect_equal(
    phint_within(rep(int12, 2), rep(as_phinterval(int12), 2)),
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

  # Within a portion of the phinterval
  expect_true(phint_within(int12, phinterval(c(int12, int45))))
  expect_true(phint_within(as_phinterval(int12), phinterval(c(int12, int45))))

  expect_equal(
    phint_within(rep(int12, 3), phinterval(c(int12, int45))),
    c(TRUE, TRUE, TRUE)
  )
  expect_equal(
    phint_within(as_phinterval(int12), rep(phinterval(c(int12, int45)), 2)),
    c(TRUE, TRUE)
  )

  # Partial intersection
  expect_false(phint_within(as_phinterval(int36), as_phinterval(int25)))
  expect_false(phint_within(int36, as_phinterval(int25)))
  expect_false(phint_within(as_phinterval(int36), int25))
  expect_false(phint_within(phinterval(c(int24, int56)), int25))
  expect_false(phint_within(int25, phinterval(c(int24, int56))))

  # No intersection
  expect_false(phint_within(as_phinterval(int12), as_phinterval(int56)))
  expect_false(phint_within(int12, as_phinterval(int56)))
  expect_false(phint_within(as_phinterval(int12), int56))
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
