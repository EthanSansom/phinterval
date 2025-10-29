# TODO:
# - test error constructors using snapshots

# phinterval -------------------------------------------------------------------

test_that("phinterval() returns zero-length vector", {

  phint <- phinterval()
  expect_s3_class(phint, "phinterval")
  expect_length(phint, 0L)

  phint <- phinterval(list())
  expect_s3_class(phint, "phinterval")
  expect_length(phint, 0L)

  phint <- phinterval(NULL)
  expect_s3_class(phint, "phinterval")
  expect_length(phint, 0L)

})

test_that("zero-length phinterval() respects `tzone`", {
  phint <- phinterval(tzone = "EST")
  expect_equal(attr(phint, "tzone"), "EST")
})

test_that("NA or empty intervals result in NA output", {

  int_NA <- interval(NA_POSIXct_, NA_POSIXct_)
  int_empty <- interval()
  int1 <- interval(as.Date("2020-01-01"), as.Date("2022-01-01"))
  phint1 <- phinterval(list(int_NA))
  phint2 <- phinterval(list(int_empty))
  phint3 <- phinterval(list(int_NA, int_empty))
  phint4 <- phinterval(list(c(int1, int_NA)))

  expect_true(all(is.na(phint1)))
  expect_true(all(is.na(phint2)))
  expect_true(all(is.na(phint3)))
  expect_true(all(is.na(phint4)))
  expect_length(phint1, 1L)
  expect_length(phint2, 1L)
  expect_length(phint3, 2L)
  expect_length(phint4, 1L)

})

test_that("phinterval errors on invalid inputs", {

  int <- interval(as.Date("2021-01-01"), as.Date("2021-02-01"), tzone = "UTC")

  expect_error(
    phinterval(list(int, 10)),
    class = "phinterval_error_wrong_list_of"
  )
  expect_error(
    phinterval(10),
    class = "phinterval_error_wrong_list_of"
  )
  expect_error(
    phinterval(as.Date("2020-01-01"), tzone = 10),
    class = "phinterval_error_wrong_list_of"
  )

  expect_error(
    phinterval(list(int), tzone = 1),
    class = "phinterval_error_wrong_class"
  )
  expect_error(
    phinterval(tzone = 1),
    class = "phinterval_error_wrong_class"
  )
  expect_error(
    phinterval(list(int), tzone = c("UTC", "EST")),
    class = "phinterval_error_wrong_class"
  )
  expect_error(
    phinterval(tzone = c("UTC", "EST")),
    class = "phinterval_error_wrong_class"
  )

})

test_that("phinterval `tzone` argument overrides `intervals` timezone", {

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

test_that("phinterval merges overlapping and adjacent intervals", {

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

test_that("phinterval standardizes input intervals", {
  int <- interval(as.Date("2020-01-01"), as.Date("2021-01-01"))
  expect_equal(
    phinterval(list(int)),
    phinterval(list(lubridate::int_flip(int)))
  )
})

test_that("phinterval works as expected", {

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

test_that("as_phinterval works as expected", {

  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:30", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:20", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:20:30", tz = "UTC")

  int0 <- interval()
  int1 <- interval(t1, t2)
  int2 <- interval(c(t1, t3), c(t2, t4))

  as_phint1 <- as_phinterval(int1)
  as_phint2 <- as_phinterval(int2)
  phint0 <- phinterval()
  phint1 <- phinterval(list(int1))
  phint2 <- phinterval(list(interval(t1, t2), interval(t3, t4)))

  expect_equal(as_phinterval(int0), phinterval())
  expect_equal(as_phint1, phint1)
  expect_equal(as_phint2, phint2)
  expect_equal(as_phinterval(phint0), phint0)
  expect_equal(as_phinterval(phint1), phint1)
  expect_equal(as_phinterval(phint2), phint2)

  as_phint1 <- as_phinterval(int1, tzone = "EST")
  as_phint2 <- as_phinterval(int2, tzone = "EST")
  phint1 <- phinterval(list(int1), tzone = "EST")
  phint2 <- phinterval(list(interval(t1, t2), interval(t3, t4)), tzone = "EST")

  expect_equal(as_phinterval(int0, tzone = "EST"), phinterval(tzone = "EST"))
  expect_equal(as_phint1, phint1)
  expect_equal(as_phint2, phint2)

})

# is_phinterval ----------------------------------------------------------------

test_that("is_phinterval works as expected", {

  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:30", tz = "UTC")
  int1 <- interval(t1, t2)

  expect_true(is_phinterval(phinterval()))
  expect_true(is_phinterval(phinterval(list(int1))))
  expect_true(is_phinterval(na_phinterval()))
  expect_false(is_phinterval(t1))
  expect_false(is_phinterval(int1))
  expect_false(is_phinterval("a"))
  expect_false(is_phinterval(logical()))

})

# phint_start ------------------------------------------------------------------

test_that("phint_start zero-length input results in zero-length output", {
  phint <- phinterval(tzone = "EST")
  start <- phint_start(phint)

  expect_s3_class(start, "POSIXct")
  expect_length(start, 0L)
  expect_equal(lubridate::tz(start), "EST")
})

test_that("phint_start NA input results in NA output", {

  int1 <- interval(NA_POSIXct_, NA_POSIXct_)
  int2 <- rep(int1, 3)
  phint <- phinterval(list(int1, int2), tzone = "PST")
  start <- phint_start(phint)

  expect_true(all(is.na(start)))
  expect_s3_class(start, "POSIXct")
  expect_length(start, 2L)
  expect_equal(lubridate::tz(start), "PST")

})

test_that("phint_start starts are correct", {

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

test_that("phint_start and int_start are equivilant with Interval input", {

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

test_that("phint_starts zero-length input results in zero-length list", {
  phint <- phinterval()
  starts <- phint_starts(phint)

  expect_identical(starts, list())
  expect_length(starts, 0L)
})

test_that("phint_starts NA input results in NA output", {

  int1 <- interval(NA_POSIXct_, NA_POSIXct_)
  int2 <- rep(int1, 3)
  phint <- phinterval(list(int1, int2), tzone = "PST")
  starts <- phint_starts(phint)

  expect_true(all(map_lgl(starts, is.na)))
  expect_true(all(map_lgl(starts, lubridate::is.POSIXct)))
  expect_length(starts, 2L)
  expect_true(all(map_lgl(starts, \(s) lubridate::tz(s) == "PST")))

})

test_that("phint_starts starts are correct", {

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

# phint_bound ------------------------------------------------------------------

test_that("phint_bound zero-length input results in zero-length output", {

  t1 <- as.POSIXct("2021-01-01 00:00:00")
  t2 <- as.POSIXct("2021-01-01 00:10:00")

  phint <- phinterval(tzone = "EST")
  bounded <- phint_bound(phint, left = t1, right = t2)

  expect_s3_class(bounded, "phinterval")
  expect_length(bounded, 0L)
  expect_equal(attr(bounded, "tzone"), "EST")

})

test_that("phint_bound NA input results in NA output", {

  t1 <- as.POSIXct("2022-02-01 10:10:10", tzone = "UTC")
  t2 <- as.POSIXct("2023-05-10 00:17:30", tzone = "UTC")

  phint <- na_phinterval(2L, tzone = "UTC")
  bounded <- phint_bound(phint, left = t1, right = t2)

  expect_equal(is.na(bounded), c(TRUE, TRUE))
  expect_s3_class(bounded, "phinterval")
  expect_length(bounded, 2L)
  expect_equal(attr(bounded, "tzone"), "UTC")

})

test_that("phint_bound recycles `left` and `right` to length of `phint`", {

  expect_phint_equal <- function(object, expected, ...) {
    expect_equal(
      standardize_phinterval(object),
      standardize_phinterval(expected),
      ...
    )
  }

  left <- as.POSIXct("2021-01-01 00:00:00")
  right <- as.POSIXct("2021-01-02 00:00:00")
  left_within <- left + 100
  right_within <- right - 100

  contains1 <- as_phinterval(interval(left - 100, right + 100))
  contains2 <- rep(contains1, 2)
  contains3 <- rep(contains1, 3)
  contains4 <- rep(contains1, 4)

  expect_phint_equal(
    phint_bound(contains2, left = left, right = right),
    as_phinterval(interval(c(left, left), c(right, right)))
  )
  expect_phint_equal(
    phint_bound(contains2, right = right),
    as_phinterval(interval(phint_start(contains2), c(right, right)))
  )
  expect_phint_equal(
    phint_bound(contains2, left = left),
    as_phinterval(interval(c(left, left), phint_end(contains2)))
  )
  expect_phint_equal(
    phint_bound(contains4, left = left, right = c(right, right_within)),
    as_phinterval(interval(
      rep(left, 4),
      c(right, right_within, right, right_within)
    ))
  )
  expect_phint_equal(
    phint_bound(contains4, left = c(left, left_within), right = right),
    as_phinterval(interval(
      c(left, left_within, left, left_within),
      rep(right, 4)
    ))
  )
  expect_phint_equal(
    phint_bound(contains4, right = c(right, right_within)),
    as_phinterval(interval(
      phint_start(contains4),
      c(right, right_within, right, right_within)
    ))
  )
  expect_phint_equal(
    phint_bound(contains4, left = c(left, left_within)),
    as_phinterval(interval(
      c(left, left_within, left, left_within),
      phint_end(contains4)
    ))
  )
  expect_error(
    phint_bound(contains1, left = c(left, left), right = c(right, right)),
    class = "phinterval_error_incompatible_length"
  )
  expect_error(
    phint_bound(contains3, left = c(left, left), right = c(right, right)),
    class = "phinterval_error_incompatible_length"
  )
  expect_error(
    phint_bound(contains1, right = c(right, right)),
    class = "phinterval_error_incompatible_length"
  )
  expect_error(
    phint_bound(contains3, left = c(left, left)),
    class = "phinterval_error_incompatible_length"
  )

})

test_that("phint_bound works as expected", {

  expect_phint_equal <- function(object, expected, ...) {
    expect_equal(
      standardize_phinterval(object),
      standardize_phinterval(expected),
      ...
    )
  }

  left <- as.POSIXct("2021-01-01 00:00:00")
  right <- as.POSIXct("2021-01-02 00:00:00")
  left_within <- left + 100
  left_outside <- left - 100
  right_within <- right - 100
  right_outside <- right + 100
  lisland_start <- left_outside - 100
  lisland_end <- left_outside - 10
  risland_start <- right_outside + 10
  risland_end <- right_outside + 100

  center_hole <- interval(left + 1000, right - 1000)
  left_island <- interval(lisland_start, lisland_end)
  right_island <- interval(risland_start, risland_end)

  within <- as_phinterval(interval(left_within, right_within))
  within_hole <- phint_diff(within, center_hole)
  contains <- as_phinterval(interval(left_outside, right_outside))
  bounded_left <- as_phinterval(interval(left_outside, left_within))
  bounded_right <- as_phinterval(interval(right_within, right_outside))
  island_left <- phint_union(bounded_left, left_island)
  island_right <- phint_union(bounded_right, right_island)

  # Within Bounds
  expect_phint_equal(phint_bound(within, left = left, right = right), within)
  expect_phint_equal(phint_bound(within, left = left), within)
  expect_phint_equal(phint_bound(within, right = right), within)
  expect_phint_equal(
    phint_bound(within_hole, left = left, right = right),
    within_hole
  )
  expect_phint_equal(phint_bound(within_hole, left = left), within_hole)
  expect_phint_equal(phint_bound(within_hole, right = right), within_hole)
  # Contains Bounds
  expect_phint_equal(
    phint_bound(contains, left = left, right = right),
    as_phinterval(interval(left, right))
  )
  expect_phint_equal(
    phint_bound(contains, left = left),
    as_phinterval(interval(left, right_outside))
  )
  expect_phint_equal(
    phint_bound(contains, right = right),
    as_phinterval(interval(left_outside, right))
  )
  # Outside of Left Bound
  expect_phint_equal(
    phint_bound(bounded_left, left = left, right = right),
    as_phinterval(interval(left, left_within))
  )
  expect_phint_equal(
    phint_bound(bounded_left, left = left),
    as_phinterval(interval(left, left_within))
  )
  expect_phint_equal(
    phint_bound(bounded_left, right = right),
    bounded_left
  )
  # Island Outside of Left Bound is cut off
  expect_phint_equal(
    phint_bound(island_left, left = left, right = right),
    as_phinterval(interval(left, left_within))
  )
  expect_phint_equal(
    phint_bound(island_left, left = left),
    as_phinterval(interval(left, left_within))
  )
  expect_phint_equal(
    phint_bound(island_left, right = right),
    island_left
  )
  # Outside of Right Bound
  expect_phint_equal(
    phint_bound(bounded_right, left = left, right = right),
    as_phinterval(interval(right_within, right))
  )
  expect_phint_equal(
    phint_bound(bounded_right, left = left),
    bounded_right
  )
  expect_phint_equal(
    phint_bound(bounded_right, right = right),
    as_phinterval(interval(right_within, right))
  )
  # Island Outside of Right Bound is cut off
  expect_phint_equal(
    phint_bound(island_right, left = left, right = right),
    as_phinterval(interval(right_within, right))
  )
  expect_phint_equal(
    phint_bound(island_right, left = left),
    island_right
  )
  expect_phint_equal(
    phint_bound(island_right, right = right),
    as_phinterval(interval(right_within, right))
  )

})

test_that("phint_bound works as expected", {

  expect_phint_equal <- function(object, expected, ...) {
    expect_equal(
      standardize_phinterval(object),
      standardize_phinterval(expected),
      ...
    )
  }

  left <- as.POSIXct("2021-01-01 00:00:00")
  right <- as.POSIXct("2021-01-02 00:00:00")
  left_within <- left + 100
  left_outside <- left - 100
  right_within <- right - 100
  right_outside <- right + 100
  lisland_start <- left_outside - 100
  lisland_end <- left_outside - 10
  risland_start <- right_outside + 10
  risland_end <- right_outside + 100

  center_hole <- interval(left + 1000, right - 1000)
  left_island <- interval(lisland_start, lisland_end)
  right_island <- interval(risland_start, risland_end)

  within <- as_phinterval(interval(left_within, right_within))
  within_hole <- phint_diff(within, center_hole)
  contains <- as_phinterval(interval(left_outside, right_outside))
  bounded_left <- as_phinterval(interval(left_outside, left_within))
  bounded_right <- as_phinterval(interval(right_within, right_outside))
  island_left <- phint_union(bounded_left, left_island)
  island_right <- phint_union(bounded_right, right_island)

  # Within Bounds
  expect_phint_equal(phint_bound(within, left = left, right = right), within)
  expect_phint_equal(phint_bound(within, left = left), within)
  expect_phint_equal(phint_bound(within, right = right), within)
  expect_phint_equal(
    phint_bound(within_hole, left = left, right = right),
    within_hole
  )
  expect_phint_equal(phint_bound(within_hole, left = left), within_hole)
  expect_phint_equal(phint_bound(within_hole, right = right), within_hole)
  # Contains Bounds
  expect_phint_equal(
    phint_bound(contains, left = left, right = right),
    as_phinterval(interval(left, right))
  )
  expect_phint_equal(
    phint_bound(contains, left = left),
    as_phinterval(interval(left, right_outside))
  )
  expect_phint_equal(
    phint_bound(contains, right = right),
    as_phinterval(interval(left_outside, right))
  )
  # Outside of Left Bound
  expect_phint_equal(
    phint_bound(bounded_left, left = left, right = right),
    as_phinterval(interval(left, left_within))
  )
  expect_phint_equal(
    phint_bound(bounded_left, left = left),
    as_phinterval(interval(left, left_within))
  )
  expect_phint_equal(
    phint_bound(bounded_left, right = right),
    bounded_left
  )
  # Island Outside of Left Bound
  expect_phint_equal(
    phint_bound(island_left, left = left, right = right),
    as_phinterval(interval(left, left_within))
  )
  expect_phint_equal(
    phint_bound(island_left, left = left),
    as_phinterval(interval(left, left_within))
  )
  expect_phint_equal(
    phint_bound(island_left, right = right),
    island_left
  )
  # Outside of Right Bound
  expect_phint_equal(
    phint_bound(bounded_right, left = left, right = right),
    as_phinterval(interval(right_within, right))
  )
  expect_phint_equal(
    phint_bound(bounded_right, left = left),
    bounded_right
  )
  expect_phint_equal(
    phint_bound(bounded_right, right = right),
    as_phinterval(interval(right_within, right))
  )
  # Island Outside of Right Bound
  expect_phint_equal(
    phint_bound(island_right, left = left, right = right),
    as_phinterval(interval(right_within, right))
  )
  expect_phint_equal(
    phint_bound(island_right, left = left),
    island_right
  )
  expect_phint_equal(
    phint_bound(island_right, right = right),
    as_phinterval(interval(right_within, right))
  )

})

# phint_diff -------------------------------------------------------------------

# TODO:
# - recycling
# - NA's
# - 0-length inputs
# - error on bad inputs

test_that("phint_diff works as expected", {

  expect_phint_equal <- function(object, expected, ...) {
    expect_equal(
      standardize_phinterval(object),
      standardize_phinterval(expected),
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

  phint12 <- phinterval(int12)
  phint23 <- phinterval(int23)
  phint34 <- phinterval(int34)
  phint45 <- phinterval(int45)
  phint56 <- phinterval(int56)
  phint13 <- phinterval(int13)

  hole <- new_phinterval(
    reference_time = origin_posixct(),
    range_starts = list(numeric()),
    range_ends = list(numeric()),
    tzone = "UTC"
  )

  # Self Difference
  expect_phint_equal(phint_diff(int12, int12), hole)
  expect_phint_equal(phint_diff(phint12, phint12), hole)
  expect_phint_equal(phint_diff(int12, phint12), hole)
  expect_phint_equal(phint_diff(phint12, int12), hole)
  expect_phint_equal(phint_diff(rep(phint12, 3), rep(phint12, 3)), rep(hole, 3))

  # Difference with Hole
  expect_phint_equal(phint_diff(phint12, hole), phint12)
  expect_phint_equal(phint_diff(hole, phint12), hole)
  expect_phint_equal(phint_diff(int12, hole), phint12)
  expect_phint_equal(phint_diff(hole, int12), hole)
  expect_phint_equal(phint_diff(rep(phint12, 3), rep(hole, 3)), rep(phint12, 3))
  expect_phint_equal(phint_diff(rep(hole, 3), rep(phint12)), rep(hole, 3))

  # Difference with Non-Intersection
  expect_phint_equal(phint_diff(int23, int45), phint23)
  expect_phint_equal(phint_diff(int45, int23), phint45)
  expect_phint_equal(phint_diff(phint45, phint23), phint45)
  expect_phint_equal(phint_diff(phint23, phint45), phint23)
  expect_phint_equal(
    phint_diff(phinterval(c(int12, int56)), int34),
    phinterval(c(int12, int56))
  )
  expect_phint_equal(
    phint_diff(int34, phinterval(c(int12, int56))),
    phint34
  )
  expect_phint_equal(
    phint_diff(
      c(phinterval(c(int12, int56)), phint34),
      c(int34, int12)
    ),
    c(phinterval(c(int12, int56)), phint34)
  )

  # Difference with Intersection
  expect_phint_equal(phint_diff(int13, int12), phint23)
  expect_phint_equal(phint_diff(int12, int13), hole)
  expect_phint_equal(phint_diff(phint13, phint12), phint23)
  expect_phint_equal(phint_diff(phint12, phint13), hole)
  expect_phint_equal(
    phint_diff(phint_squash(c(phint12, phint45)), phint12),
    phint45
  )
  expect_phint_equal(
    phint_diff(phint12, phint_squash(c(phint12, phint45))),
    hole
  )
  expect_phint_equal(
    phint_diff(phint_squash(c(phint13, phint45)), phint12),
    phint_squash(c(phint23, phint45))
  )
  expect_phint_equal(
    phint_diff(phint12, phint_squash(c(phint13, phint45))),
    hole
  )
  expect_phint_equal(
    phint_diff(int25, int34),
    phint_squash(c(int23, int45))
  )
  expect_phint_equal(phint_diff(int25, int36), phint23)
  expect_phint_equal(phint_diff(int36, int25), phint56)

  # All together
  phint_x <- phinterval(list(
    # Self Difference
    int12,
    c(int12, int34),
    # Difference with Hole
    interval(), # Subbed with a hole later
    int12,
    # Difference with Non-Intersection
    c(int12, int56),
    int34,
    # Difference with Intersection
    int13,
    c(int13, int45)
  ))
  phint_y <- phinterval(list(
    # Self Difference
    int12,
    c(int12, int34),
    # Difference with Hole
    int12,
    interval(), # Subbed with a hole later
    # Difference with Non-Intersection
    int34,
    c(int12, int56),
    # Difference with Intersection
    int12,
    int12
  ))
  phint_x[is.na(phint_x)] <- hole
  phint_y[is.na(phint_y)] <- hole

  expect_phint_equal(
    phint_diff(phint_x, phint_y),
    c(
      # Self Difference
      hole,
      hole,
      # Difference with Hole
      hole,
      phint12,
      # Difference with Non-Intersection
      phint_squash(c(phint12, phint56)),
      phint34,
      # Difference with Intersection
      phint23,
      phint_squash(c(phint23, phint45))
    )
  )

})

# phint_intersect --------------------------------------------------------------

# TODO:
# - recycling
# - NA's
# - 0-length inputs
# - error on bad inputs

test_that("phint_intersect works as expected", {

  expect_phint_equal <- function(object, expected, ...) {
    expect_equal(
      standardize_phinterval(object),
      standardize_phinterval(expected),
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

  phint12 <- phinterval(int12)
  phint23 <- phinterval(int23)
  phint34 <- phinterval(int34)
  phint45 <- phinterval(int45)
  phint56 <- phinterval(int56)
  phint13 <- phinterval(int13)
  phint35 <- phinterval(int35)
  phint16 <- phinterval(c(int13, int36))

  hole <- new_phinterval(
    reference_time = origin_posixct(),
    range_starts = list(numeric()),
    range_ends = list(numeric()),
    tzone = "UTC"
  )

  # Self Intersection
  expect_phint_equal(phint_intersect(int12, int12), phint12)
  expect_phint_equal(phint_intersect(phint12, phint12), phint12)
  expect_phint_equal(phint_intersect(int12, phint12), phint12)
  expect_phint_equal(phint_intersect(phint12, int12), phint12)
  expect_phint_equal(phint_intersect(rep(phint12, 3), rep(phint12, 3)), rep(phint12, 3))

  # Intersect with Hole
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

  # Intersection with Overlapping
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
    phint_intersect(phint_squash(c(int12, int34, int45, int56)), int25),
    phint_squash(c(int34, int45))
  )
  expect_phint_equal(
    phint_intersect(int25, phint_squash(c(int12, int34, int45, int56))),
    phint_squash(c(int34, int45))
  )
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

})

# phint_overlaps ---------------------------------------------------------------

# TODO:
# - works as expected (can be taken from `_diff` and `_intersect`)
# - recycling
# - NA's
# - 0-length inputs
# - error on bad inputs

# %within% ---------------------------------------------------------------------

test_that("`%within%` zero-length input results in zero-length output", {

  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  int <- interval(t1, t2)
  phint <- as_phinterval(int)

  expect_identical(empty_posixct() %within% phint, logical())
  expect_identical(t1 %within% phinterval(), logical())
  expect_identical(empty_posixct() %within% phinterval(), logical())

  expect_error(phinterval() %within% t1)
  expect_error(phinterval() %within% empty_posixct())

  expect_identical(int %within% phinterval(), logical())
  expect_identical(phint %within% interval(), logical())
  expect_identical(phinterval() %within% interval(), logical())
  expect_identical(interval() %within% phinterval(), logical())

})

test_that("`%within%` NA input results in NA output", {

  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t_na <- na_posixct(tzone = "UTC")

  int <- interval(t1, t2, tzone = "UTC")
  int_na <- interval(t_na, t_na, tzone = "UTC")

  phint <- as_phinterval(int)
  ph_na <- na_phinterval(tzone = "UTC")

  expect_true(is.na(t1 %within% ph_na))
  expect_true(is.na(t_na %within% phint))
  expect_true(is.na(t_na %within% ph_na))
  expect_equal(is.na(t1 %within% c(phint, ph_na)), c(FALSE, TRUE))
  expect_equal(is.na(t_na %within% c(phint, ph_na)), c(TRUE, TRUE))
  expect_equal(is.na(t_na %within% c(ph_na, ph_na)), c(TRUE, TRUE))

  expect_true(is.na(phint %within% int_na))
  expect_true(is.na(int %within% ph_na))
  expect_true(is.na(ph_na %within% int_na))
  expect_true(is.na(int_na %within% ph_na))
  expect_equal(is.na(int %within% c(ph_na, phint)), c(TRUE, FALSE))
  expect_equal(is.na(int_na %within% c(phint, ph_na)), c(TRUE, TRUE))
  expect_equal(is.na(int_na %within% c(ph_na, ph_na)), c(TRUE, TRUE))

  expect_true(is.na(ph_na %within% phint))
  expect_true(is.na(phint %within% ph_na))
  expect_true(is.na(ph_na %within% ph_na))
  expect_equal(is.na(phint %within% c(ph_na, phint)), c(TRUE, FALSE))
  expect_equal(is.na(ph_na %within% c(phint, phint)), c(TRUE, TRUE))
  expect_equal(is.na(ph_na %within% c(phint, ph_na)), c(TRUE, TRUE))

})

test_that("`%within%` recycles LHS and RHS inputs correctly", {

  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:12:12", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:17:18", tz = "UTC")
  int <- interval(t1, t2, tzone = "UTC")
  int_out <- interval(t3, t4)
  phint <- as_phinterval(int)
  phint_out <- as_phinterval(int_out)

  expect_length(t1 %within% phinterval(), 0L)
  expect_length(c(t1, t2) %within% phinterval(), 0L)
  expect_length(empty_posixct() %within% phint, 0L)
  expect_length(empty_posixct() %within% c(phint, phint), 0L)

  expect_length(rep(t1, 3) %within% phint, 3L)
  expect_length(t1 %within% rep(phint, 4), 4L)
  expect_length(rep(t1, 3) %within% rep(phint, 6), 6L)
  expect_error(
    rep(t1, 4) %within% rep(phint, 3),
    class = "phinterval_error_incompatible_length"
  )

  expect_equal(t3 %within% c(phint, phint), c(FALSE, FALSE))
  expect_equal(c(t1, t3) %within% phint, c(TRUE, FALSE))
  expect_equal(c(t1, t3) %within% rep(phint, 4), c(TRUE, FALSE, TRUE, FALSE))

  expect_length(interval() %within% phint, 0L)
  expect_length(interval() %within% c(phint, phint), 0L)
  expect_length(int %within% phinterval(), 0L)
  expect_length(c(int, int) %within% phinterval(), 0L)

  expect_length(rep(int, 3) %within% phint, 3L)
  expect_length(int %within% rep(phint, 4), 4L)
  expect_length(rep(int, 3) %within% rep(phint, 6), 6L)
  expect_error(
    rep(int, 4) %within% rep(phint, 3),
    class = "phinterval_error_incompatible_length"
  )

  expect_equal(int %within% c(phint_out, phint), c(FALSE, TRUE))
  expect_equal(c(int, int_out) %within% phint, c(TRUE, FALSE))
  expect_equal(c(int, int_out) %within% rep(phint, 4), c(TRUE, FALSE, TRUE, FALSE))

  expect_length(phinterval() %within% int, 0L)
  expect_length(phinterval() %within% c(int, int), 0L)
  expect_length(phint %within% interval(), 0L)
  expect_length(c(phint, phint) %within% interval(), 0L)

  expect_length(rep(phint, 3) %within% int, 3L)
  expect_length(phint %within% rep(int, 4), 4L)
  expect_length(rep(phint, 3) %within% rep(int, 6), 6L)
  expect_error(
    rep(phint, 4) %within% rep(int, 3),
    class = "phinterval_error_incompatible_length"
  )

  expect_equal(phint %within% c(int_out, int), c(FALSE, TRUE))
  expect_equal(c(phint, phint_out) %within% int, c(TRUE, FALSE))
  expect_equal(c(phint, phint_out) %within% rep(int, 4), c(TRUE, FALSE, TRUE, FALSE))

  expect_length(phinterval() %within% phint, 0L)
  expect_length(phinterval() %within% c(phint, phint), 0L)
  expect_length(phint %within% phinterval(), 0L)
  expect_length(c(phint, phint) %within% phinterval(), 0L)

  expect_length(rep(phint, 3) %within% phint, 3L)
  expect_length(phint %within% rep(phint, 4), 4L)
  expect_length(rep(phint, 3) %within% rep(phint, 6), 6L)
  expect_error(
    rep(phint, 4) %within% rep(phint, 3),
    class = "phinterval_error_incompatible_length"
  )

  expect_equal(phint %within% c(phint_out, phint), c(FALSE, TRUE))
  expect_equal(c(phint, phint_out) %within% phint, c(TRUE, FALSE))
  expect_equal(c(phint, phint_out) %within% rep(phint, 4), c(TRUE, FALSE, TRUE, FALSE))

})

test_that("`(ph)interval` %within% `(ph)interval` works as expected", {

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

  # `self %within% self`
  expect_true(int12 %within% as_phinterval(int12))
  expect_true(as_phinterval(int12) %within% int12)
  expect_true(as_phinterval(int12) %within% as_phinterval(int12))
  expect_true(phinterval(c(int12, int45)) %within% phinterval(c(int12, int45)))

  expect_equal(
    rep(as_phinterval(int12), 2) %within% rep(int12, 2),
    c(TRUE, TRUE)
  )
  expect_equal(
    rep(int12, 2) %within% rep(as_phinterval(int12), 2),
    c(TRUE, TRUE)
  )
  expect_equal(
    phinterval(c(int12, int45)) %within% rep(phinterval(c(int12, int45)), 2),
    c(TRUE, TRUE)
  )
  expect_equal(
    rep(phinterval(c(int12, int45)), 2) %within% phinterval(c(int12, int45)),
    c(TRUE, TRUE)
  )

  # Within a portion of the phinterval
  expect_true(int12 %within% phinterval(c(int12, int45)))
  expect_true(as_phinterval(int12) %within% phinterval(c(int12, int45)))

  expect_equal(
    rep(int12, 3) %within% phinterval(c(int12, int45)),
    c(TRUE, TRUE, TRUE)
  )
  expect_equal(
    as_phinterval(int12) %within% rep(phinterval(c(int12, int45)), 2),
    c(TRUE, TRUE)
  )

  # Partial intersection
  expect_false(as_phinterval(int36) %within% as_phinterval(int25))
  expect_false(int36 %within% as_phinterval(int25))
  expect_false(as_phinterval(int36) %within% int25)
  expect_false(phinterval(c(int24, int56)) %within% int25)
  expect_false(int25 %within% phinterval(c(int24, int56)))

  # No intersection
  expect_false(as_phinterval(int12) %within% as_phinterval(int56))
  expect_false(int12 %within% as_phinterval(int56))
  expect_false(as_phinterval(int12) %within% int56)
  expect_false(phinterval(c(int12, int34)) %within% int56)
  expect_false(int56 %within% phinterval(c(int12, int34)))

  # All together
  phint1 <- phinterval(list(
    # Self intersection
    int12,
    c(int12, int34),
    # Within a portion
    int24,
    # Partial intersection
    c(int24, int56),
    int36,
    int24,
    # No intersection
    int12,
    int56
  ))
  phint2 <- phinterval(list(
    # Self intersection
    int12,
    c(int12, int34),
    # Within a portion
    c(int24, int56),
    # Partial intersection
    int36,
    c(int24, int56),
    int36,
    # No intersection
    int56,
    int12
  ))
  expect_equal(
    phint1 %within% phint2,
    c(rep(TRUE, 3), rep(FALSE, 5))
  )

})

test_that("datetime %within% `phinterval` works as expected", {

  t1 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-07 00:02:00", tz = "UTC")

  time_in <- as.POSIXct("2021-01-03 00:00:00", tz = "UTC")
  time_out <- as.POSIXct("2021-06-13 00:00:45", tz = "UTC")
  date_in <- as.Date("2021-01-03")
  date_out <-as.Date("2021-06-13")

  phint <- phinterval(interval(t1, t2))

  expect_true(date_in %within% phint)
  expect_true(time_in %within% phint)
  expect_false(date_out %within% phint)
  expect_false(time_out %within% phint)
  expect_true(t1 %within% phint)
  expect_true(t2 %within% phint)

})

# This is a modified test from `lubridate` as of 2024/03/09. In particular, I
# test whether the `lubridate` test results are unaffected by the implementation
# of the `phinterval` `%within%` method and whether `as_phinterval(int) %within% ...`
# is equivalent to `int %within% ...`.
#
# See: https://github.com/tidyverse/lubridate/blob/main/tests/testthat/test-intervals.R
test_that("%within% implementation matches lubridate", {

  time1 <- as.POSIXct("2001-01-01", tz = "UTC")
  time2 <- as.POSIXct("2003-01-01", tz = "UTC")
  time3 <- as.POSIXct("2001-06-01", tz = "UTC")
  time4 <- as.POSIXct("2002-06-01", tz = "UTC")
  time5 <- as.POSIXct("2003-01-01", tz = "UTC")
  time6 <- as.POSIXct("2004-01-01", tz = "UTC")
  time7 <- as.POSIXct("2003-01-02", tz = "UTC")

  # `interval` `%within%` methods shouldn't be affected

  base <- interval(time1, time2)
  ins <- interval(time3, time4)
  bord <- interval(time5, time6)
  olap <- interval(time4, time6)
  outs <- interval(time7, time6)

  nbase <- interval(time2, time1)
  nins <- interval(time4, time3)
  nbord <- interval(time6, time5)
  nolap <- interval(time6, time4)
  nouts <- interval(time6, time7)

  expect_true(ins %within% base)
  expect_false(outs %within% base)
  expect_false(bord %within% base)
  expect_false(olap %within% base)

  expect_true(nins %within% nbase)
  expect_false(nouts %within% nbase)
  expect_false(nbord %within% nbase)
  expect_false(nolap %within% nbase)

  expect_true(ins %within% nbase)
  expect_false(outs %within% nbase)
  expect_false(bord %within% nbase)
  expect_false(olap %within% nbase)

  expect_true(nins %within% base)
  expect_false(nouts %within% base)
  expect_false(nbord %within% base)
  expect_false(nolap %within% base)

  expect_true(time3 %within% base)
  expect_false(time6 %within% base)
  expect_error(base %within% time3)

  expect_equal(
    c(time1, time2, time3, time4, time5, time6) %within% base,
    c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_equal(
    c(time1, time2, time3, time4, time5, time6) %within% list(base, olap),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )

  # `phinterval` `%within%` methods

  pbase <- as_phinterval(base)
  pins <- as_phinterval(ins)
  pbord <- as_phinterval(bord)
  polap <- as_phinterval(olap)
  pouts <- as_phinterval(outs)

  pnbase <- as_phinterval(nbase)
  pnins <- as_phinterval(nins)
  pnbord <- as_phinterval(nbord)
  pnolap <- as_phinterval(nolap)
  pnouts <- as_phinterval(nouts)

  ## `datetime %within% phinterval`

  expect_true(time3 %within% pbase)
  expect_false(time6 %within% pbase)
  expect_error(pbase %within% time3)

  expect_equal(
    c(time1, time2, time3, time4, time5, time6) %within% pbase,
    c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  )

  ## `interval %within% phinterval`

  expect_true(ins %within% pbase)
  expect_false(outs %within% pbase)
  expect_false(bord %within% pbase)
  expect_false(olap %within% pbase)

  expect_true(nins %within% pnbase)
  expect_false(nouts %within% pnbase)
  expect_false(nbord %within% pnbase)
  expect_false(nolap %within% pnbase)

  expect_true(ins %within% pnbase)
  expect_false(outs %within% pnbase)
  expect_false(bord %within% pnbase)
  expect_false(olap %within% pnbase)

  expect_true(nins %within% pbase)
  expect_false(nouts %within% pbase)
  expect_false(nbord %within% pbase)
  expect_false(nolap %within% pbase)

  ## `phinterval %within% interval`

  expect_true(pins %within% base)
  expect_false(pouts %within% base)
  expect_false(pbord %within% base)
  expect_false(polap %within% base)

  expect_true(pnins %within% nbase)
  expect_false(pnouts %within% nbase)
  expect_false(pnbord %within% nbase)
  expect_false(pnolap %within% nbase)

  expect_true(pins %within% nbase)
  expect_false(pouts %within% nbase)
  expect_false(pbord %within% nbase)
  expect_false(polap %within% nbase)

  expect_true(nins %within% base)
  expect_false(nouts %within% base)
  expect_false(nbord %within% base)
  expect_false(nolap %within% base)

  ## `phinterval %within% phinterval`

  expect_true(pins %within% pbase)
  expect_false(pouts %within% pbase)
  expect_false(pbord %within% pbase)
  expect_false(polap %within% pbase)

  expect_true(pnins %within% pnbase)
  expect_false(pnouts %within% pnbase)
  expect_false(pnbord %within% pnbase)
  expect_false(pnolap %within% pnbase)

  expect_true(pins %within% pnbase)
  expect_false(pouts %within% pnbase)
  expect_false(pbord %within% pnbase)
  expect_false(polap %within% pnbase)

  expect_true(nins %within% pbase)
  expect_false(nouts %within% pbase)
  expect_false(nbord %within% pbase)
  expect_false(nolap %within% pbase)

})


# is_holey ---------------------------------------------------------------------

test_that("holey phintervals correctly identified", {

  starts1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 120, 240))
  ends1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 4000))
  int1 <- lubridate::interval(starts1, ends1)

  starts2 <- lubridate::ymd(20211011) + lubridate::dminutes(c(0, 10))
  ends2 <- lubridate::ymd(20211011) + lubridate::dminutes(c(5, 20))
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
  ends <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 4000))
  int <- lubridate::interval(starts, ends)

  expect_identical(
    is_holey(int),
    c(FALSE, FALSE)
  )
})

test_that("NA inputs result in NA outputs", {

  ## `interval` input
  starts <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, NA_real_))
  ends <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 4000))
  int <- lubridate::interval(starts, ends)

  expect_identical(
    is_holey(int),
    c(FALSE, NA)
  )

  ## `phinterval` input
  starts1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 120, 240))
  ends1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 4000))
  int1 <- lubridate::interval(starts1, ends1)

  starts2 <- lubridate::ymd(20211011) + lubridate::dminutes(c(NA_real_, 10))
  ends2 <- lubridate::ymd(20211011) + lubridate::dminutes(c(5, 20))
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
  ends1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 4000))
  int1 <- lubridate::interval(starts1, ends1)

  starts2 <- lubridate::ymd(20211011) + lubridate::dminutes(c(0, 10))
  ends2 <- lubridate::ymd(20211011) + lubridate::dminutes(c(5, 20))
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
  ends <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 4000))
  int <- lubridate::interval(starts, ends)

  phint <- as_phinterval(int, tzone = "UTC")
  expect_identical(
    n_spans(int),
    rep(1L, length(int))
  )
})

test_that("NA inputs result in NA outputs", {
  starts1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 120, 240, 400))
  ends1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(6, 180, 360, 500))
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
  ends1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 4000))
  int1 <- lubridate::interval(starts1, ends1)

  starts2 <- lubridate::ymd(20211011) + lubridate::dminutes(c(0, 10))
  ends2 <- lubridate::ymd(20211011) + lubridate::dminutes(c(5, 20))
  int2 <- lubridate::interval(starts2, ends2)

  phint <- phinterval(intervals = list(int1, int2), tzone = "UTC")
  expect_identical(
    phint_to_spans(phint),
    list(int1, int2)
  )
})

test_that("spans are ordered by start date", {
  starts1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 120, 240))
  ends1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 4000))
  int1 <- lubridate::interval(starts1, ends1)

  starts2 <- lubridate::ymd(20211011) + lubridate::dminutes(c(0, 10))
  ends2 <- lubridate::ymd(20211011) + lubridate::dminutes(c(5, 20))
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
  ends1 <- NA_POSIXct_
  starts2 <- lubridate::ymd(20111111) + lubridate::dseconds(c(0, 60))
  ends2 <- lubridate::ymd(20111111) + lubridate::dseconds(c(30, 120))

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
  ends1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 4000))
  int1 <- lubridate::interval(starts1, ends1)

  starts2 <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 10))
  ends2 <- lubridate::ymd(20000101) + lubridate::dseconds(c(5, 20))
  int2 <- lubridate::interval(starts2, ends2)

  starts3 <- lubridate::ymd(20000101) + lubridate::dseconds(1000)
  ends3 <- lubridate::ymd(20000101) + lubridate::dseconds(5000)
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
  ends1 <- date + lubridate::dseconds(c(60, 180, 4000))
  int1 <- lubridate::interval(starts1, ends1)

  starts2 <- date + lubridate::dseconds(c(0, 10, 5000))
  ends2 <- date + lubridate::dseconds(c(5, 20, 6000))
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
    na_phinterval(tzone = "UTC")
  )
})

test_that("all NA input always results in NA output", {
  na_int <- lubridate::interval(NA_POSIXct_, NA_POSIXct_)
  phint <- phinterval(intervals = list(na_int, na_int), tzone = "UTC")

  ## na.rm = TRUE
  expect_identical(
    phint_squash(phint, na.rm = TRUE),
    na_phinterval(tzone = "UTC")
  )

  ## na.rm = FALSE
  expect_identical(
    phint_squash(phint, na.rm = FALSE),
    na_phinterval(tzone = "UTC")
  )
})

test_that("empty input results in empty phinterval output", {
  ## na.rm = TRUE
  expect_identical(
    phint_squash(phinterval(), na.rm = TRUE, empty = "empty"),
    phinterval()
  )

  ## na.rm = FALSE
  expect_identical(
    phint_squash(phinterval(), na.rm = FALSE, empty = "empty"),
    phinterval()
  )
})

# phint_squash -------------------------------------------------------------------

test_that("overlaps in the Interval are flattened", {
  starts1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 120, 240))
  ends1 <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 4000))
  int1 <- lubridate::interval(starts1, ends1, tzone = "UTC")

  starts2 <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 10))
  ends2 <- lubridate::ymd(20000101) + lubridate::dseconds(c(5, 20))
  int2 <- lubridate::interval(starts2, ends2, tzone = "UTC")

  starts3 <- lubridate::ymd(20000101) + lubridate::dseconds(1000)
  ends3 <- lubridate::ymd(20000101) + lubridate::dseconds(5000)
  int3 <- lubridate::interval(starts3, ends3, tzone = "UTC")

  int <- c(int1, int2, int3)

  flat_starts <- lubridate::ymd(20000101) + lubridate::dseconds(c(0, 120, 240))
  flat_ends <- lubridate::ymd(20000101) + lubridate::dseconds(c(60, 180, 5000))
  flat_int <- lubridate::interval(flat_starts, flat_ends)

  flat_phint <- phinterval(intervals = list(flat_int), tzone = "UTC")

  expect_identical(
    standardize_phinterval(phint_squash(int)),
    standardize_phinterval(flat_phint)
  )
})

test_that("NA inputs are removed when `na.rm = TRUE`, result in NA when `FALSE`", {
  date <- lubridate::ymd(20000101)

  starts1 <- date + lubridate::dseconds(c(0, 120, 240))
  ends1 <- date + lubridate::dseconds(c(60, 180, 4000))
  int1 <- lubridate::interval(starts1, ends1, tzone = "UTC")

  starts2 <- date + lubridate::dseconds(c(0, 10, 5000))
  ends2 <- date + lubridate::dseconds(c(5, 20, 6000))
  int2 <- lubridate::interval(starts2, ends2, tzone = "UTC")

  int3 <- lubridate::interval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")

  int <- c(int1, int2, int3)

  flat_starts <- date + lubridate::dseconds(c(0, 120, 240, 5000))
  flat_ends <- date + lubridate::dseconds(c(60, 180, 4000, 6000))
  flat_int <- lubridate::interval(flat_starts, flat_ends)

  flat_phint <- phinterval(intervals = list(flat_int), tzone = "UTC")

  ## na.rm = TRUE
  expect_identical(
    standardize_phinterval(phint_squash(int, na.rm = TRUE)),
    standardize_phinterval(flat_phint)
  )
  expect_identical(
    standardize_phinterval(phint_squash(as_phinterval(int), na.rm = TRUE)),
    standardize_phinterval(flat_phint)
  )

  ## na.rm = FALSE
  expect_identical(
    standardize_phinterval(phint_squash(int, na.rm = FALSE)),
    na_phinterval(tzone = "UTC")
  )
  expect_identical(
    standardize_phinterval(phint_squash(as_phinterval(int), na.rm = FALSE)),
    na_phinterval(tzone = "UTC")
  )
})

test_that("all NA input always results in NA output", {
  na_int <- lubridate::interval(NA_POSIXct_, NA_POSIXct_)

  ## na.rm = TRUE
  expect_identical(
    phint_squash(na_int, na.rm = TRUE, empty = "empty"),
    na_phinterval(tzone = "UTC")
  )

  ## na.rm = FALSE
  expect_identical(
    phint_squash(na_int, na.rm = FALSE, empty = "empty"),
    na_phinterval(tzone = "UTC")
  )
})

test_that("empty input results in empty phinterval output", {
  ## na.rm = TRUE
  expect_identical(
    phint_squash(lubridate::interval(), na.rm = TRUE, empty = "empty"),
    phinterval()
  )

  ## na.rm = FALSE
  expect_identical(
    phint_squash(lubridate::interval(), na.rm = FALSE, empty = "empty"),
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

test_that("instants within ranges are overlaps iff `inclusive = TRUE`", {
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

  expect_true(phint_overlaps(phint1, phint2, inclusive = TRUE))
  expect_false(phint_overlaps(phint1, phint2, inclusive = FALSE))
})

test_that("aligned spans are not overlaps if `inclusive = FALSE`", {
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

  expect_false(phint_overlaps(phint1, phint2, inclusive = FALSE))
})

test_that("aligned spans are overlaps if `inclusive = TRUE`", {
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
    phint_overlaps(phint1, phint2, inclusive = TRUE),
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
  ends1 <- NA_POSIXct_
  int1 <- lubridate::interval(starts1, ends1, tzone = "UTC")

  starts2 <- origin + lubridate::dseconds(c(10, 60, 240, 50))
  ends2 <- origin + lubridate::dseconds(c(30, 120, 300, 55))
  int2 <- lubridate::interval(starts2, ends2, tzone = "UTC")

  starts3 <- origin + lubridate::dseconds(c(15.60))
  ends3 <- origin + lubridate::dseconds(c(27.99))
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
    standardize_phinterval(na_phinterval()),
    na_phinterval()
  )
})

test_that("empty input results in empty output", {
  expect_identical(
    standardize_phinterval(phinterval()),
    phinterval()
  )
})
