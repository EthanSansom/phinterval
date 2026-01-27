# unary dispatch ---------------------------------------------------------------

test_that("Unary functions work on <Interval> and <phinterval> inputs", {
  t1 <- as.POSIXct("2021-01-01", tz = "UTC")
  t2 <- as.POSIXct("2021-01-02", tz = "UTC")

  intvl <- interval(t1, t2)
  phint <- as_phinterval(intvl)

  expect_equal(phint_complement(phint), phint_complement(intvl))
  expect_equal(phint_sift(phint), phint_sift(intvl))
  expect_equal(phint_invert(phint), phint_invert(intvl))
})

# unary functions --------------------------------------------------------------

test_that("Unary functions empty input results in empty output", {
  expect_all_true(map_lgl(
    list(
      phint_complement(phinterval()),
      phint_sift(phinterval()),
      phint_invert(phinterval())
    ),
    identical,
    phinterval()
  ))
})

test_that("Unary functions NA input results in NA output", {
  t1 <- as.POSIXct("2021-01-01", tz = "UTC")
  t2 <- as.POSIXct("2021-01-02", tz = "UTC")

  intvl <- interval(c(t1, NA, t1, NA), c(t2, t2, NA, NA))
  phint <- as_phinterval(intvl)

  expect_all_true(
    map_lgl(
      list(
        is.na(phint_complement(intvl)),
        is.na(phint_complement(phint)),
        is.na(phint_sift(intvl)),
        is.na(phint_sift(phint)),
        is.na(phint_invert(intvl)),
        is.na(phint_invert(phint))
      ),
      identical,
      c(FALSE, TRUE, TRUE, TRUE)
    )
  )
})

test_that("Unary functions error on invalid inputs", {
  expect_error(phint_complement(as.POSIXct(0)))
  expect_error(phint_complement(10))
  expect_error(phint_sift(as.POSIXct(0)))
  expect_error(phint_sift(10))
  expect_error(phint_invert(as.POSIXct(0)))
  expect_error(phint_invert(interval(NA, NA), hole_to = "span"))
})

# phint_sift -------------------------------------------------------------------

test_that("phint_sift() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:30", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:10:50", tz = "UTC")

  int12 <- phinterval(t1, t2)
  int34 <- phinterval(t3, t4)
  hole <- hole(tzone = "UTC")

  # No instants
  expect_equal(phint_sift(int12), as_phinterval(int12))
  expect_equal(
    phint_sift(c(int12, int34)),
    c(int12, int34)
  )

  # With instants
  expect_equal(phint_sift(interval(t1, t1)), hole)
  expect_equal(
    phint_sift(phinterval(c(t1, t4), c(t1, t4), by = 1)),
    hole
  )
  expect_equal(
    phint_sift(phinterval(c(t1, t2, t3), c(t1, t2, t4), by = 1)),
    int34
  )
})

# phint_complement -------------------------------------------------------------

test_that("phint_complement() works as expected", {
  t1 <- as.POSIXct(0, tz = "UTC")
  t2 <- as.POSIXct(100, tz = "UTC")
  t3 <- as.POSIXct(250, tz = "UTC")
  t4 <- as.POSIXct(700, tz = "UTC")
  t_neg_inf <- as.POSIXct(-Inf, tz = "UTC")
  t_pos_inf <- as.POSIXct(Inf, tz = "UTC")

  int12 <- phinterval(t1, t2)
  int23 <- phinterval(t2, t3)
  int34 <- phinterval(t3, t4)
  hole <- hole(tzone = "UTC")

  # Finite endpoints
  expect_equal(
    phint_complement(int23),
    phinterval(c(t_neg_inf, t3), c(t2, t_pos_inf), by = 1)
  )
  expect_equal(
    phint_complement(phint_squash(c(int12, int34))),
    phinterval(c(t_neg_inf, t2, t4), c(t1, t3, t_pos_inf), by = 1)
  )

  # One infinite endpoint
  expect_equal(
    phint_complement(phinterval(t_neg_inf, t1)),
    phinterval(t1, t_pos_inf)
  )
  expect_equal(
    phint_complement(phinterval(t1, t_pos_inf)),
    phinterval(t_neg_inf, t1)
  )
  expect_equal(
    phint_complement(phinterval(c(t_neg_inf, t2), c(t1, t3), by = 1)),
    phinterval(c(t1, t3), c(t2, t_pos_inf), by = 1)
  )
  expect_equal(
    phint_complement(phinterval(c(t1, t3), c(t2, t_pos_inf), by = 1)),
    phinterval(c(t_neg_inf, t2), c(t1, t3), by = 1)
  )

  # Two infinite endpoints
  expect_equal(phint_complement(phinterval(t_neg_inf, t_pos_inf)), hole)
  expect_equal(
    phint_complement(phinterval(c(t_neg_inf, t3), c(t2, t_pos_inf), by = 1)),
    int23
  )

  # Hole and Instant
  expect_equal(phint_complement(hole), phinterval(t_neg_inf, t_pos_inf))
  expect_equal(phint_complement(phinterval(t1, t1)), phinterval(t_neg_inf, t_pos_inf))
})

# phint_invert -----------------------------------------------------------------

test_that("phint_invert() works as expected", {
  t1 <- as.POSIXct(0, tz = "UTC")
  t2 <- as.POSIXct(100, tz = "UTC")
  t3 <- as.POSIXct(250, tz = "UTC")
  t4 <- as.POSIXct(700, tz = "UTC")
  t5 <- as.POSIXct(1000, tz = "UTC")
  t6 <- as.POSIXct(1300, tz = "UTC")
  t_neg_inf <- as.POSIXct(-Inf, tz = "UTC")
  t_pos_inf <- as.POSIXct(Inf, tz = "UTC")

  int11 <- phinterval(t1, t1)
  int12 <- phinterval(t1, t2)
  int13 <- phinterval(t1, t3)
  int23 <- phinterval(t2, t3)
  int34 <- phinterval(t3, t4)
  int44 <- phinterval(t4, t4)
  int45 <- phinterval(t4, t5)
  int56 <- phinterval(t5, t6)
  hole <- hole(tzone = "UTC")

  # Single spans
  expect_equal(phint_invert(c(int23, int34, int11)), rep(hole, 3))
  expect_equal(phint_invert(phinterval(t_neg_inf, t_pos_inf)), hole)

  # Holey intervals
  expect_equal(phint_invert(phint_squash(c(int12, int34))), int23)
  expect_equal(phint_invert(phint_squash(c(int11, int34))), int13)
  expect_equal(phint_invert(phint_squash(c(int23, int44))), int34)
  expect_equal(
    phint_invert(phint_squash(c(int12, int34, int56))),
    phint_squash(c(int23, int45))
  )

  # Holes
  expect_equal(phint_invert(hole), hole)
  expect_equal(phint_invert(hole, hole_to = "hole"), hole)
  expect_equal(phint_invert(hole, hole_to = "na"), phinterval(NA_POSIXct_, NA_POSIXct_))
  expect_equal(phint_invert(hole, hole_to = "inf"), phinterval(t_neg_inf, t_pos_inf))
})
