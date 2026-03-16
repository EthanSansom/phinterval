# unary dispatch ---------------------------------------------------------------

test_that("Unary functions work on <Interval> and <phinterval> inputs", {
  t1 <- as.POSIXct("2021-01-01", tz = "UTC")
  t2 <- as.POSIXct("2021-01-02", tz = "UTC")

  intvl <- interval(t1, t2)
  phint <- as_phinterval(intvl)

  expect_equal(phint_complement(phint), phint_complement(intvl))
  expect_equal(phint_discard_instants(phint), phint_discard_instants(intvl))
  expect_equal(phint_invert(phint), phint_invert(intvl))
  expect_equal(phint_cumunion(phint), phint_cumunion(intvl))
  expect_equal(phint_cumintersect(phint), phint_cumintersect(intvl))
  expect_equal(phint_sift(phint, min_length = 1), phint_sift(intvl, min_length = 1))
})

# unary functions --------------------------------------------------------------

test_that("Unary functions empty input results in empty output", {
  expect_all_true(map_lgl(
    list(
      phint_complement(phinterval()),
      phint_discard_instants(phinterval()),
      phint_invert(phinterval()),
      phint_cumunion(phinterval()),
      phint_cumintersect(phinterval()),
      phint_sift(phinterval(), min_length = 1)
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

  # phint_cumunion(), phint_cumintersect() are exceptions
  expect_all_true(
    map_lgl(
      list(
        is.na(phint_complement(intvl)),
        is.na(phint_complement(phint)),
        is.na(phint_discard_instants(intvl)),
        is.na(phint_discard_instants(phint)),
        is.na(phint_invert(intvl)),
        is.na(phint_invert(phint)),
        is.na(phint_sift(intvl, min_length = 1)),
        is.na(phint_sift(phint, min_length = 1))
      ),
      identical,
      c(FALSE, TRUE, TRUE, TRUE)
    )
  )
})

test_that("Unary functions error on invalid inputs", {
  intvl <- interval(NA, NA)

  expect_error(phint_complement(as.POSIXct(0)))
  expect_error(phint_complement(10))
  expect_error(phint_discard_instants(as.POSIXct(0)))
  expect_error(phint_discard_instants(10))
  expect_error(phint_invert(as.POSIXct(0)))
  expect_error(phint_invert(intvl, hole_to = "span"))
  expect_error(phint_cumunion(as.POSIXct(0)))
  expect_error(phint_cumunion(intvl, na_propagate = "no"))
  expect_error(phint_cumintersect(as.POSIXct(0)))
  expect_error(phint_cumintersect(intvl, na_propagate = "no"))
  expect_error(phint_cumintersect(intvl, bounds = TRUE))
  expect_error(phint_sift(10, min_length = 1))
  expect_error(phint_sift(intvl, min_length = 1, action = "drop"))
  expect_error(phint_sift(intvl, min_length = "A"))
  expect_error(phint_sift(intvl, min_length = 1, max_length = TRUE))
  expect_error(phint_sift(intvl, min_length = 1, max_length = 1:2))
  expect_error(phint_sift(intvl, min_length = 1:3, max_length = 1))
  expect_error(phint_sift(rep(intvl, 2), min_length = 1:3, max_length = 1:3))
  expect_error(phint_sift(rep(intvl, 2), min_length = 1:3, max_length = 1:3))
})

# phint_discard_instants -------------------------------------------------------------------

test_that("phint_discard_instants() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:30", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:10:50", tz = "UTC")

  int12 <- phinterval(t1, t2)
  int34 <- phinterval(t3, t4)
  hole <- hole(tzone = "UTC")

  # No instants
  expect_equal(phint_discard_instants(int12), as_phinterval(int12))
  expect_equal(
    phint_discard_instants(c(int12, int34)),
    c(int12, int34)
  )

  # With instants
  expect_equal(phint_discard_instants(interval(t1, t1)), hole)
  expect_equal(
    phint_discard_instants(phinterval(c(t1, t4), c(t1, t4), by = 1)),
    hole
  )
  expect_equal(
    phint_discard_instants(phinterval(c(t1, t2, t3), c(t1, t2, t4), by = 1)),
    int34
  )
})

# phint_sift -------------------------------------------------------------------

test_that("phint_sift() works as expected", {
  int_10min <- phinterval(
    as.POSIXct("2021-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  )
  int_30sec <- phinterval(as.POSIXct(0, tz = "UTC"), as.POSIXct(30, tz = "UTC"))
  int_20sec <- phinterval(as.POSIXct(120, tz = "UTC"), as.POSIXct(140, tz = "UTC"))
  int_0sec <- phinterval(as.POSIXct(200, tz = "UTC"), as.POSIXct(200, tz = "UTC"))
  hole <- hole(tzone = "UTC")
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")

  # Error when both min_length and max_length are NULL
  expect_error(phint_sift(int_10min))

  dseconds <- lubridate::dseconds
  dminutes <- lubridate::dminutes

  # min_length only (discard spans shorter than min)
  expect_equal(phint_sift(int_10min, min_length = dminutes(5)), int_10min)
  expect_equal(phint_sift(int_30sec, min_length = dminutes(5)), hole)
  expect_equal(
    phint_sift(c(int_10min, int_30sec, int_20sec), min_length = dminutes(1)),
    c(int_10min, hole, hole)
  )

  # max_length only (discard spans longer than max)
  expect_equal(phint_sift(int_10min, max_length = dminutes(5)), hole)
  expect_equal(phint_sift(int_30sec, max_length = dminutes(5)), int_30sec)
  expect_equal(
    phint_sift(c(int_10min, int_30sec, int_20sec), max_length = dminutes(1)),
    c(hole, int_30sec, int_20sec)
  )

  # min_length and max_length (keep spans inside [min, max])
  expect_equal(
    phint_sift(c(int_10min, int_30sec, int_20sec), min_length = dseconds(25), max_length = dminutes(1), action = "keep"),
    c(hole, int_30sec, hole)
  )

  # action = "discard": discard spans within [min, max]
  expect_equal(
    phint_sift(c(int_10min, int_30sec, int_20sec), min_length = dseconds(25), max_length = dminutes(1), action = "discard"),
    c(int_10min, hole, int_20sec)
  )
  expect_equal(
    phint_sift(c(int_10min, int_30sec, int_20sec), min_length = dminutes(1), action = "discard"),
    c(hole, int_30sec, int_20sec)
  )

  # Holey phinterval: spans are filtered independently
  phint_10min_20sec <- phint_squash(c(int_10min, int_20sec))
  expect_equal(
    phint_sift(phint_10min_20sec, min_length = dminutes(1)),
    int_10min
  )
  expect_equal(
    phint_sift(phint_10min_20sec, max_length = dminutes(1)),
    int_20sec
  )

  # Instants are treated as zero-duration spans
  expect_equal(phint_sift(int_0sec, min_length = dseconds(1)), hole)
  expect_equal(phint_sift(int_0sec, max_length = dseconds(1)), int_0sec)
  expect_equal(phint_sift(int_0sec, max_length = dseconds(0)), int_0sec)

  # NA and hole inputs are preserved
  expect_equal(phint_sift(hole, min_length = dminutes(1)), hole)
  expect_equal(phint_sift(na_phint, min_length = dminutes(1)), na_phint)

  # Vectorized min_length and max_length
  expect_equal(
    phint_sift(c(int_10min, int_30sec), min_length = c(dminutes(5), dseconds(15))),
    c(int_10min, int_30sec)
  )
  expect_equal(
    phint_sift(c(int_10min, int_30sec), min_length = c(dminutes(15), dseconds(15))),
    c(hole, int_30sec)
  )

  # Disjoint spans: sieve removes individual spans independently
  phint_3spans <- phint_squash(c(int_20sec, int_30sec, int_10min))
  phint_2spans <- phint_squash(c(int_20sec, int_30sec))

  expect_equal(
    phint_sift(phint_3spans, min_length = 0),
    phint_3spans
  )
  expect_equal(
    phint_sift(phint_3spans, min_length = dseconds(25)),
    phint_squash(c(int_30sec, int_10min))
  )
  expect_equal(
    phint_sift(phint_3spans, max_length = dseconds(25)),
    int_20sec
  )
  expect_equal(
    phint_sift(phint_3spans, min_length = dseconds(25), max_length = dminutes(1)),
    int_30sec
  )
  expect_equal(
    phint_sift(phint_3spans, min_length = dseconds(25)),
    phint_squash(c(int_30sec, int_10min))
  )
  expect_equal(
    phint_sift(phint_3spans, max_length = dseconds(25)),
    int_20sec
  )

  # Removing all spans from a disjoint element results in a hole
  expect_equal(
    phint_sift(phint_2spans, min_length = dminutes(1)),
    hole
  )
  expect_equal(
    phint_sift(
      interval(as.POSIXct(200, tz = "UTC"), as.POSIXct(200, tz = "UTC")),
      min_length = dminutes(1)
    ),
    hole
  )
})

test_that("phint_sift() works with numeric or <Duration> bounds", {
  withr::local_seed(123)

  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:30", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:10:50", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  int_30sec <- phinterval(t2, t3)
  int_20sec <- phinterval(t3 + 10, t4 + 10)
  int_0sec <- phinterval(t1 - 10, t1 - 10)

  dseconds <- lubridate::dseconds
  dminutes <- lubridate::dminutes

  # numeric input is equivalent to deseconds() input
  bounds <- expand.grid(seq(0, 50, 5), seq(0, 50, 5))
  min_lengths <- bounds[[1]]
  max_lengths <- bounds[[2]]
  n <- length(min_lengths)

  ints_30sec <- rep(int_30sec, n)
  ints_20sec <- rep(int_20sec, n)
  ints_0sec <- rep(int_0sec, n)

  phints <- phint_squash_by(
    c(ints_30sec, ints_20sec, ints_0sec),
    by = sample(rep(seq(n), 3), n * 3, replace = FALSE)
  )$phint

  # action = "keep"
  expect_equal(
    phint_sift(ints_30sec, min_lengths, max_lengths, action = "keep"),
    phint_sift(ints_30sec, duration(min_lengths), duration(max_lengths), action = "keep")
  )
  expect_equal(
    phint_sift(ints_20sec, min_lengths, max_lengths, action = "keep"),
    phint_sift(ints_20sec, duration(min_lengths), duration(max_lengths), action = "keep")
  )
  expect_equal(
    phint_sift(ints_0sec, min_lengths, max_lengths, action = "keep"),
    phint_sift(ints_0sec, duration(min_lengths), duration(max_lengths), action = "keep")
  )
  expect_equal(
    phint_sift(phints, min_lengths, max_lengths, action = "keep"),
    phint_sift(phints, duration(min_lengths), duration(max_lengths), action = "keep")
  )

  # action = "discard"
  expect_equal(
    phint_sift(ints_30sec, min_lengths, max_lengths, action = "discard"),
    phint_sift(ints_30sec, duration(min_lengths), duration(max_lengths), action = "discard")
  )
  expect_equal(
    phint_sift(ints_20sec, min_lengths, max_lengths, action = "discard"),
    phint_sift(ints_20sec, duration(min_lengths), duration(max_lengths), action = "discard")
  )
  expect_equal(
    phint_sift(ints_0sec, min_lengths, max_lengths, action = "discard"),
    phint_sift(ints_0sec, duration(min_lengths), duration(max_lengths), action = "discard")
  )
  expect_equal(
    phint_sift(phints, min_lengths, max_lengths, action = "discard"),
    phint_sift(phints, duration(min_lengths), duration(max_lengths), action = "discard")
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

test_that("phint_invert() handles instants correctly", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:04:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:08:00", tz = "UTC")
  t7 <- as.POSIXct("2021-01-01 00:09:00", tz = "UTC")
  t8 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t9 <- as.POSIXct("2021-01-01 00:11:00", tz = "UTC")
  hole <- hole(tzone = "UTC")

  phint11 <- phinterval(t1, t1)
  phint55 <- phinterval(t5, t5)
  phint66 <- phinterval(t6, t6)
  phint99 <- phinterval(t9, t9)

  phint34 <- phinterval(t3, t4)
  phint78 <- phinterval(t7, t8)

  # <instant(s)>
  expect_all_true(
    c(
      phint_invert(phint11) == hole,
      phint_invert(phint_squash(c(phint11, phint55))) == phinterval(t1, t5),
      phint_invert(phint_squash(c(phint11, phint55, phint99))) == phinterval(t1, t9),
      phint_invert(phint_squash(c(phint11, phint55, phint66, phint99))) == phinterval(t1, t9)
    )
  )

  # <instant> x <span>
  expect_all_true(
    c(
      phint_invert(phint_squash(c(phint11, phint34))) == phinterval(t1, t3),
      phint_invert(phint_squash(c(phint34, phint55))) == phinterval(t4, t5),

      # <span>, <instant(s)>, <span>
      phint_invert(phint_squash(c(phint34, phint55, phint78))) == phinterval(t4, t7),
      phint_invert(phint_squash(c(phint34, phint55, phint78))) == phinterval(t4, t7),
      phint_invert(phint_squash(c(phint34, phint55, phint66, phint78))) == phinterval(t4, t7),

      # <instant>, <span>, <instant>
      phint_invert(phint_squash(c(phint11, phint34, phint99))) == phint_squash(c(phinterval(t1, t3), phinterval(t4, t9))),
      phint_invert(phint_squash(c(phint11, phint34, phint78, phint99))) == phinterval(c(t1, t4, t8), c(t3, t7, t9), by = 1)
    )
  )
})

# phint_cumunion ---------------------------------------------------------------

test_that("phint_cumunion() works as expected", {
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
  hole  <- hole(tzone = "UTC")

  # Length-1 input
  expect_equal(phint_cumunion(int12), int12)
  expect_equal(phint_cumunion(hole), hole)

  # Non-overlapping intervals accumulate into growing unions
  phint <- c(int12, int23, int34)
  expect_equal(
    phint_cumunion(phint),
    c(int12, phinterval(t1, t3), phinterval(t1, t4))
  )

  # Overlapping intervals: later duplicates are absorbed
  phint <- c(int25, int23, int36)
  expect_equal(
    phint_cumunion(phint),
    c(int25, int25, phinterval(t2, t6))
  )

  # Holes in input are treated as empty (na_propagate = FALSE default)
  phint <- c(int12, hole, int34)
  expect_equal(
    phint_cumunion(phint),
    c(int12, int12, phint_squash(c(int12, int34)))
  )
})

test_that("phint_cumunion() respects na_propagate argument", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  int12 <- phinterval(t1, t2)
  int23 <- phinterval(t2, t3)
  int34 <- phinterval(t3, t4)
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")
  hole  <- hole(tzone = "UTC")

  # na_propagate = FALSE (default): NA treated as hole, does not infect result
  expect_equal(
    phint_cumunion(c(int12, na_phint, int34)),
    c(int12, int12, phint_squash(c(int12, int34)))
  )
  expect_equal(
    phint_cumunion(c(na_phint, int12, int23)),
    c(hole, int12, phinterval(t1, t3))
  )

  # na_propagate = TRUE: NA infects all subsequent elements
  expect_equal(
    phint_cumunion(c(int12, na_phint, int34), na_propagate = TRUE),
    c(int12, na_phint, na_phint)
  )
  expect_equal(
    phint_cumunion(c(na_phint, int12, int23), na_propagate = TRUE),
    c(na_phint, na_phint, na_phint)
  )

  # na_propagate = TRUE: elements before the NA are unaffected
  expect_equal(
    phint_cumunion(c(int12, int23, na_phint), na_propagate = TRUE),
    c(int12, phinterval(t1, t3), na_phint)
  )

  # All NA input
  expect_equal(
    phint_cumunion(c(na_phint, na_phint), na_propagate = TRUE),
    c(na_phint, na_phint)
  )
  expect_equal(
    phint_cumunion(c(na_phint, na_phint), na_propagate = FALSE),
    c(hole, hole)
  )
})

# phint_cumintersect -----------------------------------------------------------

test_that("phint_cumintersect() works as expected", {
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
  hole  <- hole(tzone = "UTC")

  # Length-1 input
  expect_equal(phint_cumintersect(int12), int12)
  expect_equal(phint_cumintersect(hole), hole)

  # Non-overlapping intervals: intersection collapses to hole
  phint <- c(int12, int34)
  expect_equal(
    phint_cumintersect(phint),
    c(int12, hole)
  )

  # Overlapping intervals: intersection narrows
  phint <- c(int25, int36)
  expect_equal(
    phint_cumintersect(phint),
    c(int25, phinterval(t3, t5))
  )

  # Three overlapping intervals
  phint <- c(phinterval(t1, t5), phinterval(t2, t6), int34)
  expect_equal(
    phint_cumintersect(phint),
    c(phinterval(t1, t5), phinterval(t2, t5), int34)
  )

  # Once intersection becomes hole, remains hole
  phint <- c(int12, int34, int25)
  expect_equal(
    phint_cumintersect(phint),
    c(int12, hole, hole)
  )

  # Holes in input treated as empty (na_propagate = FALSE default)
  phint <- c(int25, hole, int36)
  expect_equal(
    phint_cumintersect(phint),
    c(int25, hole, hole)
  )
})

test_that("phint_cumintersect() respects na_propagate argument", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:04:00", tz = "UTC")
  int12 <- phinterval(t1, t2)
  int25 <- phinterval(t2, t5)
  int34 <- phinterval(t3, t4)
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")
  hole  <- hole(tzone = "UTC")

  # na_propagate = FALSE (default): NA treated as hole, does not infect result
  expect_equal(
    phint_cumintersect(c(int25, na_phint, int34)),
    c(int25, hole, hole)
  )
  expect_equal(
    phint_cumintersect(c(na_phint, int25, int34)),
    c(hole, hole, hole)
  )

  # na_propagate = TRUE: NA infects all subsequent elements
  expect_equal(
    phint_cumintersect(c(int25, na_phint, int34), na_propagate = TRUE),
    c(int25, na_phint, na_phint)
  )
  expect_equal(
    phint_cumintersect(c(na_phint, int25, int34), na_propagate = TRUE),
    c(na_phint, na_phint, na_phint)
  )

  # na_propagate = TRUE: elements before the NA are unaffected
  expect_equal(
    phint_cumintersect(c(int25, int34, na_phint), na_propagate = TRUE),
    c(int25, int34, na_phint)
  )

  # All NA input
  expect_equal(
    phint_cumintersect(c(na_phint, na_phint), na_propagate = TRUE),
    c(na_phint, na_phint)
  )
  expect_equal(
    phint_cumintersect(c(na_phint, na_phint), na_propagate = FALSE),
    c(hole, hole)
  )
})

test_that("phint_cumintersect() respects bounds argument", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:04:00", tz = "UTC")
  int12 <- phinterval(t1, t2)
  int23 <- phinterval(t2, t3)
  int34 <- phinterval(t3, t4)
  int25 <- phinterval(t2, t5)
  int22 <- phinterval(t2, t2)
  hole  <- hole(tzone = "UTC")

  # Abutting intervals: bounds = "[]" produces an instant, bounds = "()" produces a hole
  expect_equal(
    phint_cumintersect(c(int12, int23)),
    c(int12, int22)
  )
  expect_equal(
    phint_cumintersect(c(int12, int23), bounds = "()"),
    c(int12, hole)
  )

  # Chain of abutting intervals: bounds = "[]" collapses to instants, "()" to holes
  expect_equal(
    phint_cumintersect(c(int12, int23, int34)),
    c(int12, int22, hole)
  )
  expect_equal(
    phint_cumintersect(c(int12, int23, int34), bounds = "()"),
    c(int12, hole, hole)
  )

  # Instant inputs: bounds = "[]" preserves instant, bounds = "()" produces hole
  expect_equal(
    phint_cumintersect(c(int25, int22)),
    c(int25, int22)
  )
  expect_equal(
    phint_cumintersect(c(int25, int22), bounds = "()"),
    c(int25, hole)
  )

  # Once hole, always hole regardless of bounds
  expect_equal(
    phint_cumintersect(c(int12, int34, int23)),
    c(int12, hole, hole)
  )
  expect_equal(
    phint_cumintersect(c(int12, int34, int23), bounds = "()"),
    c(int12, hole, hole)
  )

  # Non-abutting overlapping intervals are unaffected by bounds
  expect_equal(
    phint_cumintersect(c(int12, int34, int25)),
    phint_cumintersect(c(int12, int34, int25), bounds = "()")
  )
})
