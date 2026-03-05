# binary dispatch --------------------------------------------------------------

test_that("Binary functions work on <Interval> x <phinterval> combinations", {
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
  res_symmetric_setdiff <- list(
    phint_phint = phint_symmetric_setdiff(phint13, phint24),
    phint_intvl = phint_symmetric_setdiff(phint13, intvl24),
    intvl_phint = phint_symmetric_setdiff(intvl13, phint24),
    intvl_intvl = phint_symmetric_setdiff(intvl13, intvl24)
  )

  expect_all_true(map_lgl(res_union[-1], ~ .x == res_union[[1]]))
  expect_all_true(map_lgl(res_intersect[-1], ~ .x == res_intersect[[1]]))
  expect_all_true(map_lgl(res_setdiff[-1], ~ .x == res_setdiff[[1]]))
  expect_all_true(map_lgl(res_symmetric_setdiff[-1], ~ .x == res_symmetric_setdiff[[1]]))

  expect_error(phint_union(phint13, t1))
  expect_error(phint_union(t1, phint13))
  expect_error(phint_intersect(phint13, t1))
  expect_error(phint_intersect(t1, phint13))
  expect_error(phint_setdiff(phint13, t1))
  expect_error(phint_setdiff(t1, phint13))
  expect_error(phint_symmetric_setdiff(phint13, t1))
  expect_error(phint_symmetric_setdiff(t1, phint13))

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

# binary functions -------------------------------------------------------------

test_that("Binary functions empty input results in empty output", {
  expect_all_true(
    map_lgl(
      list(
        phint_union(phinterval(), phinterval()),
        phint_intersect(phinterval(), phinterval()),
        phint_setdiff(phinterval(), phinterval()),
        phint_symmetric_setdiff(phinterval(), phinterval())
      ),
      identical,
      phinterval()
    )
  )

  expect_all_true(
    map_lgl(
      list(
        phint_within(phinterval(), phinterval()),
        phint_within(lubridate::POSIXct(), phinterval()),
        phint_overlaps(phinterval(), phinterval())
      ),
      identical,
      logical()
    )
  )
})

test_that("Binary functions NA input results in NA output", {
  t1 <- as.POSIXct("2021-01-01", tz = "UTC")
  t2 <- as.POSIXct("2021-01-02", tz = "UTC")

  point <- c(t1, NA, NA, NA, NA)
  intvl <- interval(c(t1, t1, NA, t1, NA), c(t2, t2, t2, NA, NA))
  phint <- phinterval(c(t1, NA, t1, NA, NA), c(t2, NA, t2, NA, NA))

  expect_all_true(
    map_lgl(
      list(
        is.na(phint_union(intvl, phint)),
        is.na(phint_union(phint, intvl)),
        is.na(phint_intersect(intvl, phint)),
        is.na(phint_intersect(phint, intvl)),
        is.na(phint_setdiff(intvl, phint)),
        is.na(phint_setdiff(phint, intvl)),
        is.na(phint_symmetric_setdiff(intvl, phint)),
        is.na(phint_symmetric_setdiff(phint, intvl)),

        is.na(phint_within(intvl, phint)),
        is.na(phint_within(phint, intvl)),
        is.na(phint_within(point, phint)),
        is.na(phint_within(point, intvl)),

        is.na(phint_overlaps(intvl, phint)),
        is.na(phint_overlaps(phint, intvl))
      ),
      identical,
      c(FALSE, TRUE, TRUE, TRUE, TRUE)
    )
  )
})

test_that("Binary functions recycle inputs", {
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

  expect_equal(phint_symmetric_setdiff(phint1, phint3), phint_symmetric_setdiff(rep(phint1, 3), phint3))
  expect_equal(phint_symmetric_setdiff(phint3, phint1), phint_symmetric_setdiff(phint3, rep(phint1, 3)))
  expect_equal(phint_symmetric_setdiff(phint0, phint1), phint0)
  expect_equal(phint_symmetric_setdiff(phint1, phint0), phint0)

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

test_that("Binary functions error on invalid inputs", {
  int <- interval(as.Date("2021-01-01"), as.Date("2021-02-01"))
  phint <- as_phinterval(int)

  expect_error(phint_union(int, 10))
  expect_error(phint_union("A", phint))
  expect_error(phint_union(phint))

  expect_error(phint_intersect(int, 10))
  expect_error(phint_intersect("A", phint))
  expect_error(phint_intersect(phint))
  expect_error(phint_intersect(phint, phint, bounds = "none"))

  expect_error(phint_setdiff(int, 10))
  expect_error(phint_setdiff("A", phint))
  expect_error(phint_setdiff(phint))

  expect_error(phint_within(int, 10))
  expect_error(phint_within("A", phint))
  expect_error(phint_within(phint))
  expect_error(phint_within(phint, phint, bounds = "none"))

  expect_error(phint_overlaps(int, 10))
  expect_error(phint_overlaps("A", phint))
  expect_error(phint_overlaps(phint))
  expect_error(phint_overlaps(phint, phint, bounds = "none"))
})

# phint_union ------------------------------------------------------------------

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
  expect_equal(phint_union(rep(int12, 3), rep(hole, 3)), rep(int12, 3))
  expect_equal(phint_union(rep(hole, 3), rep(int12, 3)), rep(int12, 3))

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

test_that("phint_setdiff() handles instants and identical spans correctly", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:04:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:08:00", tz = "UTC")
  t7 <- as.POSIXct("2021-01-01 00:08:30", tz = "UTC")
  t8 <- as.POSIXct("2021-01-01 00:09:00", tz = "UTC")
  hole  <- hole(tzone = "UTC")

  phint14 <- phinterval(t1, t4)
  phint23 <- phinterval(t2, t3)
  phint67 <- phinterval(t6, t7)
  phint58 <- phinterval(t5, t8)

  phint11 <- phinterval(t1, t1)
  phint22 <- phinterval(t2, t2)
  phint44 <- phinterval(t4, t4)
  phint55 <- phinterval(t5, t5)
  phint66 <- phinterval(t6, t6)
  phint88 <- phinterval(t8, t8)

  phint14_58 <- phint_union(phint14, phint58)
  phint14_88 <- phint_union(phint14, phint88)
  phint11_58 <- phint_union(phint11, phint58)

  ## <span> x <instant>
  expect_all_true(
    c(
      phint_setdiff(phint14, phint22) == phint14,
      phint_setdiff(phint14, phint11) == phint14,
      phint_setdiff(phint14, phint44) == phint14,

      phint_setdiff(phint22, phint14) == hole,
      phint_setdiff(phint11, phint14) == phint11,
      phint_setdiff(phint44, phint14) == phint44
    )
  )

  # <span> x <set-of-spans>
  expect_all_true(
    c(
      phint_setdiff(phint14, phint14_58) == hole(),
      phint_setdiff(phint58, phint14_58) == hole(),
      phint_setdiff(phint23, phint14_58) == hole(),
      phint_setdiff(phint67, phint14_58) == hole(),

      phint_setdiff(phint14_58, phint14) == phint58,
      phint_setdiff(phint14_58, phint58) == phint14,
      phint_setdiff(phint14_58, phint23) == phinterval(c(t1, t3, t5), c(t2, t4, t8), by = 1),
      phint_setdiff(phint14_58, phint67) == phinterval(c(t1, t5, t7), c(t4, t6, t8), by = 1)
    )
  )

  # <span> x <set-of-span-and-instant>
  expect_all_true(
    c(
      phint_setdiff(phint14, phint14_88) == hole(),
      phint_setdiff(phint23, phint14_88) == hole(),
      phint_setdiff(phint58, phint11_58) == hole(),
      phint_setdiff(phint67, phint11_58) == hole(),

      phint_setdiff(phint14_88, phint14) == phint88,
      phint_setdiff(phint14_88, phint23) == phinterval(c(t1, t3, t8), c(t2, t4, t8), by = 1),
      phint_setdiff(phint11_58, phint58) == phint11,
      phint_setdiff(phint11_58, phint67) == phinterval(c(t1, t5, t7), c(t1, t6, t8), by = 1)
    )
  )

  # <instant> x <set-of-spans>
  expect_all_true(
    c(
      phint_setdiff(phint14_58, phint22) == phint14_58,
      phint_setdiff(phint14_58, phint11) == phint14_58,
      phint_setdiff(phint14_58, phint44) == phint14_58,
      phint_setdiff(phint14_58, phint55) == phint14_58,
      phint_setdiff(phint14_58, phint88) == phint14_58,

      phint_setdiff(phint22, phint14_58) == hole,
      phint_setdiff(phint11, phint14_58) == phint11,
      phint_setdiff(phint44, phint14_58) == phint44,
      phint_setdiff(phint55, phint14_58) == phint55,
      phint_setdiff(phint88, phint14_58) == phint88
    )
  )

  # <instant> x <set-of-span-and-instant>
  expect_all_true(
    c(
      phint_setdiff(phint14_88, phint22) == phint14_88,
      phint_setdiff(phint14_88, phint11) == phint14_88,
      phint_setdiff(phint14_88, phint44) == phint14_88,
      phint_setdiff(phint14_88, phint88) == phint14_88,
      phint_setdiff(phint11_58, phint66) == phint11_58,
      phint_setdiff(phint11_58, phint11) == phint11_58,
      phint_setdiff(phint11_58, phint55) == phint11_58,
      phint_setdiff(phint11_58, phint88) == phint11_58,

      phint_setdiff(phint22, phint14_88) == hole,
      phint_setdiff(phint11, phint14_88) == phint11,
      phint_setdiff(phint44, phint14_88) == phint44,
      phint_setdiff(phint88, phint14_88) == phint88,
      phint_setdiff(phint66, phint11_58) == hole,
      phint_setdiff(phint11, phint11_58) == phint11,
      phint_setdiff(phint55, phint11_58) == phint55,
      phint_setdiff(phint88, phint11_58) == phint88,

      phint_setdiff(phint44, phint11_58) == phint44,
      phint_setdiff(phint55, phint14_88) == phint55
    )
  )
})

# phint_symmetric_setdiff ------------------------------------------------------

# Symmetric set-difference assumes exclusive `()` intervals
test_that("phint_symmetric_setdiff() works as expected", {
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

  # Self symmetric difference is a hole
  expect_equal(phint_symmetric_setdiff(int12, int12), hole)
  expect_equal(phint_symmetric_setdiff(rep(int12, 3), rep(int12, 3)), rep(hole, 3))

  # Symmetric difference with hole is identity
  expect_equal(phint_symmetric_setdiff(int12, hole), int12)
  expect_equal(phint_symmetric_setdiff(hole, int12), int12)
  expect_equal(phint_symmetric_setdiff(rep(int12, 3), rep(hole, 3)), rep(int12, 3))
  expect_equal(phint_symmetric_setdiff(rep(hole, 3), rep(int12, 3)), rep(int12, 3))

  # Symmetry: f(x, y) == f(y, x)
  expect_equal(phint_symmetric_setdiff(int12, int23), phint_symmetric_setdiff(int23, int12))
  expect_equal(phint_symmetric_setdiff(int25, int36), phint_symmetric_setdiff(int36, int25))

  # Non-overlapping: result is union
  expect_equal(phint_symmetric_setdiff(int12, int34), phint_squash(c(int12, int34)))
  expect_equal(phint_symmetric_setdiff(int12, int56), phint_squash(c(int12, int56)))
  expect_equal(
    phint_symmetric_setdiff(phint_squash(c(int12, int56)), int34),
    phint_squash(c(int12, int34, int56))
  )

  # Overlapping: result is union minus intersection
  expect_equal(phint_symmetric_setdiff(int13, int23), int12)
  expect_equal(phint_symmetric_setdiff(int25, int36), phint_squash(c(int23, int56)))
  expect_equal(phint_symmetric_setdiff(int13, int25), phint_squash(c(int12, phinterval(t3, t5))))

  # One contains the other
  expect_equal(phint_symmetric_setdiff(int25, int34), phint_squash(c(int23, int45)))
  expect_equal(phint_symmetric_setdiff(int34, int25), phint_squash(c(int23, int45)))

  # Holey phinterval inputs
  expect_equal(
    phint_symmetric_setdiff(phint_squash(c(int12, int34)), int13),
    phint_squash(c(int23, int34))
  )
  expect_equal(
    phint_symmetric_setdiff(phint_squash(c(int12, int45)), phint_squash(c(int23, int56))),
    phint_squash(c(int13, phinterval(t4, t6)))
  )

  # Instants
  expect_equal(phint_symmetric_setdiff(int12, int22), int12)
  expect_equal(phint_symmetric_setdiff(int22, int12), int12)
  expect_equal(phint_symmetric_setdiff(int22, int22), int22)
  expect_equal(phint_symmetric_setdiff(int13, int22), int13)

  # Vectorized
  expect_equal(
    phint_symmetric_setdiff(c(int12, int25), c(int12, int36)),
    c(hole, phint_squash(c(int23, int56)))
  )
})

test_that("phint_symmetric_setdiff() handles instants and identical spans correctly", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:04:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:08:00", tz = "UTC")
  hole <- hole(tzone = "UTC")

  phint14 <- phinterval(t1, t4)
  phint36 <- phinterval(t3, t6)
  phint23 <- phinterval(t2, t3)

  phint11 <- phinterval(t1, t1)
  phint22 <- phinterval(t2, t2)
  phint44 <- phinterval(t4, t4)
  phint66 <- phinterval(t6, t6)

  phint14_66 <- phint_union(phint14, phint66)

  # <span> x <instant>: instants do not affect the symmetric difference
  expect_all_true(
    c(
      phint_symmetric_setdiff(phint14, phint22) == phint14,
      phint_symmetric_setdiff(phint14, phint11) == phint14,
      phint_symmetric_setdiff(phint14, phint44) == phint14,
      phint_symmetric_setdiff(phint22, phint14) == phint14,
      phint_symmetric_setdiff(phint11, phint14) == phint14,
      phint_symmetric_setdiff(phint44, phint14) == phint14
    )
  )

  # <instant> x <instant>
  expect_all_true(
    c(
      phint_symmetric_setdiff(phint22, phint22) == phint22,
      phint_symmetric_setdiff(phint22, phint44) == phint_squash(c(phint22, phint44))
    )
  )

  # <span> x <set-of-spans>
  expect_all_true(
    c(
      phint_symmetric_setdiff(phint14, phint36) == phinterval(c(t1, t4), c(t3, t6), by = 1),
      phint_symmetric_setdiff(phint14, phint14_66) == phint66,
      phint_symmetric_setdiff(phint14_66, phint14) == phint66,
      phint_symmetric_setdiff(phint14_66, phint36) == phinterval(c(t1, t4), c(t3, t6), by = 1)
    )
  )

  # <set-of-spans> x <instant>: instants do not affect the symmetric difference
  expect_all_true(
    c(
      phint_symmetric_setdiff(phint14_66, phint22) == phint14_66,
      phint_symmetric_setdiff(phint14_66, phint44) == phint14_66,
      phint_symmetric_setdiff(phint14_66, phint66) == phint14_66,
      phint_symmetric_setdiff(phint22, phint14_66) == phint14_66,
      phint_symmetric_setdiff(phint44, phint14_66) == phint14_66,
      phint_symmetric_setdiff(phint66, phint14_66) == phint14_66
    )
  )
})

test_that("phint_symmetric_setdiff() respects bounds argument", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  hole <- hole(tzone = "UTC")

  phint12 <- phinterval(t1, t2)
  phint23 <- phinterval(t2, t3)
  phint34 <- phinterval(t3, t4)
  phint22 <- phinterval(t2, t2)
  phint44 <- phinterval(t4, t4)

  phint22_44 <- phint_union(phint22, phint44)
  phint22_34 <- phint_union(phint22, phint34)

  # Default bounds = "()" is consistent with phint_setdiff instant behavior
  expect_equal(phint_symmetric_setdiff(phint22, phint22), phint22)
  expect_equal(phint_symmetric_setdiff(phint22, phint22, bounds = "()"), phint22)

  # bounds = "[]": instant self-symmetric-difference is a hole
  expect_equal(phint_symmetric_setdiff(phint22, phint22, bounds = "[]"), hole)
  expect_equal(phint_symmetric_setdiff(phint22_44, phint22_44, bounds = "[]"), hole)
  expect_equal(phint_symmetric_setdiff(phint22_34, phint22_34, bounds = "[]"), hole)
  expect_equal(phint_symmetric_setdiff(phint22, phint22_44, bounds = "[]"), phint44)
  expect_equal(phint_symmetric_setdiff(phint22_44, phint22, bounds = "[]"), phint22)
  expect_equal(phint_symmetric_setdiff(phint22_34, phint22, bounds = "[]"), phint34)
  expect_equal(phint_symmetric_setdiff(phint22, phint22_34, bounds = "[]"), phint34)

  # Adjacent intervals: bounds = "()" and bounds = "[]" both result in a merge
  expect_equal(
    phint_symmetric_setdiff(phint12, phint23, bounds = "()"),
    phinterval(t1, t3)
  )
  expect_equal(
    phint_symmetric_setdiff(phint12, phint23, bounds = "[]"),
    phinterval(t1, t3)
  )

  # Non-adjacent intervals are unaffected by bounds
  expect_equal(
    phint_symmetric_setdiff(phint12, phint34, bounds = "()"),
    phint_squash(c(phint12, phint34))
  )
  expect_equal(
    phint_symmetric_setdiff(phint12, phint34, bounds = "[]"),
    phint_squash(c(phint12, phint34))
  )
})

# phint_intersect --------------------------------------------------------------

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

  phint12_45 <- phint_squash(c(int12, int45))
  phint13_45 <- phint_squash(c(int13, int45))

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
    phint_intersect(phint12_45, int12),
    int12
  )
  expect_equal(
    phint_intersect(phint13_45, phint12_45),
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

  expect_equal(
    phint_intersect(
      c(int13, phint_squash(c(int12, int34, int56)), phint12_45),
      c(int12, int16, phint13_45)
    ),
    phint_intersect(
      c(int13, phint_squash(c(int12, int34, int56)), phint12_45),
      c(int12, int16, phint13_45),
      bounds = "()"
    )
  )

  # Intersection with instants
  expect_all_true(
    map_lgl(
      list(
        phint_intersect(int22, int22),
        phint_intersect(int12, int22),
        phint_intersect(int22, int12),
        phint_intersect(int13, int22),
        phint_intersect(int22, int13)
      ),
      identical,
      int22
    )
  )
  expect_all_true(
    map_lgl(
      list(
        phint_intersect(int22, int22, bounds = "()"),
        phint_intersect(int12, int22, bounds = "()"),
        phint_intersect(int22, int12, bounds = "()"),
        phint_intersect(int13, int22, bounds = "()"),
        phint_intersect(int22, int13, bounds = "()")
      ),
      identical,
      hole
    )
  )

  # Intersection with abutting.
  expect_equal(phint_intersect(int23, int12), int22)
  expect_equal(phint_intersect(int12, int23), int22)
  expect_equal(
    phint_intersect(int12, phint_complement(int12)),
    phint_squash(c(int11, int22))
  )
  expect_equal(phint_intersect(int23, int12, bounds = "()"), hole)
  expect_equal(phint_intersect(int12, int23, bounds = "()"), hole)
  expect_equal(
    phint_intersect(int12, phint_complement(int12), bounds = "()"),
    hole
  )
})

# phint_overlaps ---------------------------------------------------------------

test_that("phint_overlaps() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:02:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:03:00", tz = "UTC")

  int12 <- phinterval(t1, t2)
  int23 <- phinterval(t2, t3)
  int24 <- phinterval(t2, t4)
  int34 <- phinterval(t3, t4)
  int45 <- phinterval(t4, t5)
  int56 <- phinterval(t5, t6)
  int14 <- phinterval(t1, t4)
  int25 <- phinterval(t2, t5)
  int36 <- phinterval(t3, t6)
  int22 <- phinterval(t2, t2)

  phint12_45 <- phint_squash(c(int12, int45))
  hole <- hole(tzone = "UTC")

  # Self overlap
  expect_true(phint_overlaps(int12, int12))
  expect_true(phint_overlaps(int22, int22))
  expect_true(phint_overlaps(phint12_45, phint12_45))
  expect_false(phint_overlaps(hole, hole))

  expect_true(phint_overlaps(int12, int12, bounds = "()"))
  expect_true(phint_overlaps(phint12_45, phint12_45, bounds = "()"))
  expect_false(phint_overlaps(int22, int22, bounds = "()"))
  expect_false(phint_overlaps(hole, hole, bounds = "()"))

  # Partial overlap
  expect_all_true(c(
    phint_overlaps(int12, int14),
    phint_overlaps(int14, int12),
    phint_overlaps(int25, int36),
    phint_overlaps(int36, int25),
    phint_overlaps(int12, int14, bounds = "()"),
    phint_overlaps(int14, int12, bounds = "()"),
    phint_overlaps(int25, int36, bounds = "()"),
    phint_overlaps(int36, int25, bounds = "()")
  ))

  # Non-overlapping
  expect_all_false(c(
    phint_overlaps(int12, int34),
    phint_overlaps(int12, int56),
    phint_overlaps(phint_squash(c(int12, int34)), int56),
    phint_overlaps(int56, phint_squash(c(int12, int34))),
    phint_overlaps(int12, int34, bounds = "()"),
    phint_overlaps(int12, int56, bounds = "()"),
    phint_overlaps(phint_squash(c(int12, int34)), int56, bounds = "()"),
    phint_overlaps(int56, phint_squash(c(int12, int34)), bounds = "()")
  ))

  # Abutting
  expect_all_true(c(
    phint_overlaps(int12, int23),
    phint_overlaps(int23, int12),
    phint_overlaps(int22, int22),
    phint_overlaps(phint12_45, int24)
  ))
  expect_all_false(c(
    phint_overlaps(int12, int23, bounds = "()"),
    phint_overlaps(int23, int12, bounds = "()"),
    phint_overlaps(int22, int22, bounds = "()"),
    phint_overlaps(phint12_45, int24, bounds = "()")
  ))

  # Hole
  expect_all_false(c(
    phint_overlaps(hole, int22),
    phint_overlaps(int23, hole),
    phint_overlaps(hole, int22, bounds = "()"),
    phint_overlaps(int23, hole, bounds = "()")
  ))
})

# phint_within -----------------------------------------------------------------

test_that("phint_within(<phintish>, <phintish>) works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:00:45", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:00:55", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:02:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:03:00", tz = "UTC")

  int11 <- phinterval(t1, t1)
  int12 <- phinterval(t1, t2)
  int22 <- phinterval(t2, t2)
  int23 <- phinterval(t2, t3)
  int34 <- phinterval(t3, t4)
  int45 <- phinterval(t4, t5)
  int56 <- phinterval(t5, t6)
  int14 <- phinterval(t1, t4)
  int24 <- phinterval(t2, t4)
  int25 <- phinterval(t2, t5)
  int36 <- phinterval(t3, t6)

  phint12_45 <- phint_squash(c(int12, int45))
  hole <- hole(tzone = "UTC")

  # Self within self
  expect_true(phint_within(int12, int12))
  expect_true(phint_within(int22, int22))
  expect_true(phint_within(phint12_45, phint12_45))
  expect_false(phint_within(int22, int22, bounds = "()"))

  # Endpoint within interval
  expect_all_true(c(
    phint_within(int22, int12),
    phint_within(int11, int12),
    phint_within(int22, phint12_45),
    phint_within(int11, phint12_45)
  ))
  expect_all_false(c(
    phint_within(int22, int12, bounds = "()"),
    phint_within(int11, int12, bounds = "()"),
    phint_within(int22, phint12_45, bounds = "()"),
    phint_within(int11, phint12_45, bounds = "()")
  ))

  expect_equal(phint_within(rep(int12, 2), rep(int12, 2)), c(TRUE, TRUE))
  expect_equal(phint_within(phint12_45, rep(phint12_45, 2)), c(TRUE, TRUE))
  expect_equal(phint_within(rep(phint12_45, 2), phint12_45), c(TRUE, TRUE))

  # Within a portion of a larger phinterval
  expect_true(phint_within(int12, phint12_45))
  expect_true(phint_within(int22, int12))
  expect_true(phint_within(int22, phint12_45))

  expect_all_true(c(
    phint_within(int12, phint12_45),
    phint_within(int22, int12),
    phint_within(int22, phint12_45)
  ))
  expect_all_false(c(
    phint_within(int11, phint12_45, bounds = "()"),
    phint_within(int22, int12, bounds = "()"),
    phint_within(int22, phint12_45, bounds = "()")
  ))

  expect_equal(phint_within(rep(int12, 3), phint12_45), c(TRUE, TRUE, TRUE))
  expect_equal(phint_within(int12, rep(phint12_45, 2)), c(TRUE, TRUE))

  # Partial intersection (not fully within)
  expect_false(phint_within(int36, int25))
  expect_false(phint_within(phint_squash(c(int24, int56)), int25))
  expect_false(phint_within(int25, phint_squash(c(int24, int56))))

  # No intersection
  expect_all_false(c(
    phint_within(int12, int56),
    phint_within(phint_squash(c(int12, int34)), int56),
    phint_within(int22, int56),
    phint_within(int22, phint_squash(c(int34, int56))),
    phint_within(int56, phint_squash(c(int12, int34))),
    phint_within(int12, int56, bounds = "()"),
    phint_within(phint_squash(c(int12, int34)), int56, bounds = "()"),
    phint_within(int22, int56, bounds = "()"),
    phint_within(int22, phint_squash(c(int34, int56)), bounds = "()"),
    phint_within(int56, phint_squash(c(int12, int34)), bounds = "()")
  ))

  # Hole
  expect_all_false(
    c(
      phint_within(int12, hole),
      phint_within(hole, int12),
      phint_within(phint_squash(c(int12, int34)), hole),
      phint_within(hole, phint_squash(c(int12, int34))),
      phint_within(hole, hole),
      phint_within(int12, hole, bounds = "()"),
      phint_within(hole, int12, bounds = "()")
    )
  )
})

test_that("phint_within(<datetime>, <phintish>) works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:01:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-07 00:02:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-08 00:01:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-09 00:02:00", tz = "UTC")

  time_in <- as.POSIXct("2021-01-03 00:00:00", tz = "UTC")
  time_out <- as.POSIXct("2021-06-13 00:00:45", tz = "UTC")
  date_in <- as.Date("2021-01-03")
  date_out <- as.Date("2021-06-13")

  intvl1 <- interval(t1, t2)
  phint1 <- phinterval(t1, t2)
  phint2 <- phinterval(c(t1, t3), c(t2, t4), by = 1)
  hole <- hole(tzone = "UTC")

  # In the middle
  expect_all_true(c(
    phint_within(date_in, intvl1),
    phint_within(time_in, intvl1),
    phint_within(date_in, phint1),
    phint_within(time_in, phint1),
    phint_within(date_in, phint2),
    phint_within(time_in, phint2),
    phint_within(date_in, intvl1, bounds = "()"),
    phint_within(time_in, intvl1, bounds = "()"),
    phint_within(date_in, phint1, bounds = "()"),
    phint_within(time_in, phint1, bounds = "()"),
    phint_within(date_in, phint2, bounds = "()"),
    phint_within(time_in, phint2, bounds = "()")
  ))

  # Outside
  expect_all_false(c(
    phint_within(date_out, intvl1),
    phint_within(time_out, intvl1),
    phint_within(date_out, phint1),
    phint_within(time_out, phint1),
    phint_within(date_out, phint2),
    phint_within(time_out, phint2),
    phint_within(date_out, intvl1, bounds = "()"),
    phint_within(time_out, intvl1, bounds = "()"),
    phint_within(date_out, phint1, bounds = "()"),
    phint_within(time_out, phint1, bounds = "()"),
    phint_within(date_out, phint2, bounds = "()"),
    phint_within(time_out, phint2, bounds = "()")
  ))

  # On the boundary
  expect_all_true(c(
    phint_within(t1, phint1),
    phint_within(t1, phint2),
    phint_within(t2, phint1),
    phint_within(t2, phint2)
  ))
  expect_all_false(c(
    phint_within(t1, phint1, bounds = "()"),
    phint_within(t1, phint2, bounds = "()"),
    phint_within(t2, phint1, bounds = "()"),
    phint_within(t2, phint2, bounds = "()")
  ))

  # Instant
  expect_true(phint_within(time_in, as_phinterval(time_in)))
  expect_true(phint_within(date_in, as_phinterval(date_in)))
  expect_false(phint_within(time_in, as_phinterval(time_out)))
  expect_false(phint_within(date_out, as_phinterval(date_in)))

  expect_all_false(c(
    phint_within(time_in, as_phinterval(time_in), bounds = "()"),
    phint_within(date_in, as_phinterval(date_in), bounds = "()"),
    phint_within(time_in, as_phinterval(time_out), bounds = "()"),
    phint_within(date_out, as_phinterval(date_in), bounds = "()")
  ))

  # Hole
  expect_all_false(c(
    phint_within(time_in, hole),
    phint_within(date_in, hole),
    phint_within(time_in, hole, bounds = "()"),
    phint_within(date_in, hole, bounds = "()")
  ))
})
