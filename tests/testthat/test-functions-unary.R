# unary dispatch ---------------------------------------------------------------

test_that("Unary functions work on <Interval> and <phinterval> inputs", {
  t1 <- as.POSIXct("2021-01-01", tz = "UTC")
  t2 <- as.POSIXct("2021-01-02", tz = "UTC")

  intvl <- interval(t1, t2)
  phint <- as_phinterval(intvl)

  expect_equal(phint_complement(phint), phint_complement(intvl))
  expect_equal(phint_sift(phint), phint_sift(intvl))
  expect_equal(phint_invert(phint), phint_invert(intvl))
  expect_equal(phint_cumunion(phint), phint_cumunion(intvl))
  expect_equal(phint_cumintersect(phint), phint_cumintersect(intvl))
})

# unary functions --------------------------------------------------------------

test_that("Unary functions empty input results in empty output", {
  expect_all_true(map_lgl(
    list(
      phint_complement(phinterval()),
      phint_sift(phinterval()),
      phint_invert(phinterval()),
      phint_cumunion(phinterval()),
      phint_cumintersect(phinterval())
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
  intvl <- interval(NA, NA)

  expect_error(phint_complement(as.POSIXct(0)))
  expect_error(phint_complement(10))
  expect_error(phint_sift(as.POSIXct(0)))
  expect_error(phint_sift(10))
  expect_error(phint_invert(as.POSIXct(0)))
  expect_error(phint_invert(intvl, hole_to = "span"))
  expect_error(phint_cumunion(as.POSIXct(0)))
  expect_error(phint_cumunion(intvl, na_propagate = "no"))
  expect_error(phint_cumintersect(as.POSIXct(0)))
  expect_error(phint_cumintersect(intvl, na_propagate = "no"))
  expect_error(phint_cumintersect(intvl, bounds = TRUE))
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
