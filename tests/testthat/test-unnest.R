# phint_unnest -----------------------------------------------------------------

test_that("phint_unnest() respects hole_to argument", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:05:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:15:00", tz = "UTC")

  hole <- hole(tzone = "UTC")
  int12 <- phinterval(t1, t2)
  int34 <- phinterval(t3, t4)
  phint <- c(int12, hole, int34)
  phint_holes <- c(hole, hole, hole)

  expect_equal(
    phint_unnest(phint, hole_to = "drop"),
    tibble::tibble(
      key = c(1, 3),
      start = c(t1, t3),
      end = c(t2, t4)
    )
  )
  expect_equal(
    phint_unnest(c(hole, phint, hole), hole_to = "drop"),
    tibble::tibble(
      key = c(2, 4),
      start = c(t1, t3),
      end = c(t2, t4)
    )
  )
  expect_equal(
    phint_unnest(phint, hole_to = "na"),
    tibble::tibble(
      key = c(1, 2, 3),
      start = c(t1, NA_POSIXct_, t3),
      end = c(t2, NA_POSIXct_, t4)
    )
  )

  expect_equal(
    phint_unnest(phint_holes, hole_to = "drop"),
    tibble::tibble(
      key = integer(),
      start = as.POSIXct(numeric(), tz = "UTC"),
      end = as.POSIXct(numeric(), tz = "UTC")
    )
  )
  expect_equal(
    phint_unnest(phint_holes, hole_to = "na"),
    tibble::tibble(
      key = c(1, 2, 3),
      start = c(NA_POSIXct_, NA_POSIXct_, NA_POSIXct_),
      end = c(NA_POSIXct_, NA_POSIXct_, NA_POSIXct_)
    )
  )
})

test_that("phint_unnest() respects keep_size argument", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:05:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:15:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:25:00", tz = "UTC")

  int12 <- phinterval(t1, t2)
  phint <- phinterval(c(t1, t3, t5), c(t2, t4, t6), by = c(1, 1, 2))

  expect_equal(
    phint_unnest(phint, keep_size = FALSE),
    tibble::tibble(
      key = c(1, 1, 2),
      start = c(t1, t3, t5),
      end = c(t2, t4, t6)
    )
  )
  expect_equal(
    phint_unnest(phint, keep_size = TRUE),
    tibble::tibble(
      key = c(1, 1, 2),
      start = c(t1, t3, t5),
      end = c(t2, t4, t6),
      size = c(2, 2, 1)
    )
  )
  expect_equal(
    phint_unnest(int12, keep_size = TRUE),
    tibble::tibble(
      key = 1,
      start = t1,
      end = t2,
      size = 1
    )
  )
})

test_that("phint_unnest() works with <Interval> or <phinterval> inputs", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:05:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:15:00", tz = "UTC")

  intvl <- interval(c(t1, t3), c(t2, t4))
  phint <- phinterval(c(t1, t3), c(t2, t4))

  expect_equal(phint_unnest(intvl), phint_unnest(phint))
  expect_equal(
    phint_unnest(intvl, keep_size = TRUE),
    phint_unnest(phint, keep_size = TRUE)
  )
  expect_equal(
    phint_unnest(intvl, hole_to = "na"),
    phint_unnest(phint, hole_to = "na")
  )
})

test_that("phint_unnest() errors on invalid inputs", {
  expect_error(phint_unnest(as.Date("2020-01-01")))
  expect_error(phint_unnest(phinterval(), hole_to = ""))
  expect_error(phint_unnest(phinterval(), hole_to = 10))
  expect_error(phint_unnest(phinterval(), keep_size = NA))
  expect_error(phint_unnest(phinterval(), keep_size = "yes"))
})

test_that("phint_unnest() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:05:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:15:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:25:00", tz = "UTC")

  int11 <- phinterval(t1, t1)
  int12 <- phinterval(t1, t2)
  int34 <- phinterval(t3, t4)
  int56 <- phinterval(t5, t6)

  phint12_34 <- phinterval(c(t1, t3), c(t2, t4), by = 1)
  phint12_34_56 <- phinterval(c(t1, t3, t5), c(t2, t4, t6), by = 1)

  hole <- hole(tzone = "UTC")
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")

  # Empty
  expect_equal(
    phint_unnest(phinterval(tzone = "UTC")),
    tibble::tibble(
      key = integer(),
      start = as.POSIXct(numeric(), tz = "UTC"),
      end = as.POSIXct(numeric(), tz = "UTC")
    )
  )

  # Scalar
  expect_equal(
    phint_unnest(int12),
    tibble::tibble(
      key = 1,
      start = t1,
      end = t2
    )
  )
  expect_equal(
    phint_unnest(c(int12, int34, int56)),
    tibble::tibble(
      key = c(1, 2, 3),
      start = c(t1, t3, t5),
      end = c(t2, t4, t6)
    )
  )

  # Multi-span
  expect_equal(
    phint_unnest(phint12_34),
    tibble::tibble(
      key = c(1, 1),
      start = c(t1, t3),
      end = c(t2, t4)
    )
  )
  expect_equal(
    phint_unnest(phint12_34_56),
    tibble::tibble(
      key = c(1, 1, 1),
      start = c(t1, t3, t5),
      end = c(t2, t4, t6)
    )
  )

  # Scalar and Multi-span
  expect_equal(
    phint_unnest(c(int12, phint12_34, int56)),
    tibble::tibble(
      key = c(1, 2, 2, 3),
      start = c(t1, t1, t3, t5),
      end = c(t2, t2, t4, t6)
    )
  )
  expect_equal(
    phint_unnest(c(phint12_34, int56, phint12_34_56)),
    tibble::tibble(
      key = c(1, 1, 2, 3, 3, 3),
      start = c(t1, t3, t5, t1, t3, t5),
      end = c(t2, t4, t6, t2, t4, t6)
    )
  )

  # Holes
  expect_equal(
    phint_unnest(c(int12, hole, int34), hole_to = "drop"),
    tibble::tibble(
      key = c(1, 3),
      start = c(t1, t3),
      end = c(t2, t4)
    )
  )
  expect_equal(
    phint_unnest(c(hole, phint12_34, hole), hole_to = "drop"),
    tibble::tibble(
      key = c(2, 2),
      start = c(t1, t3),
      end = c(t2, t4)
    )
  )
  expect_equal(
    phint_unnest(c(int12, hole, int34), hole_to = "na"),
    tibble::tibble(
      key = c(1, 2, 3),
      start = c(t1, NA_POSIXct_, t3),
      end = c(t2, NA_POSIXct_, t4)
    )
  )
  expect_equal(
    phint_unnest(c(int12, hole, int34), hole_to = "na", keep_size = TRUE),
    tibble::tibble(
      key = c(1, 2, 3),
      start = c(t1, NA_POSIXct_, t3),
      end = c(t2, NA_POSIXct_, t4),
      size = c(1L, 0L, 1L)
    )
  )
  expect_equal(
    phint_unnest(c(phint12_34, hole, int56), hole_to = "na"),
    tibble::tibble(
      key = c(1, 1, 2, 3),
      start = c(t1, t3, NA_POSIXct_, t5),
      end = c(t2, t4, NA_POSIXct_, t6)
    )
  )
  expect_equal(
    phint_unnest(c(phint12_34, hole, int56), hole_to = "na", keep_size = TRUE),
    tibble::tibble(
      key = c(1, 1, 2, 3),
      start = c(t1, t3, NA_POSIXct_, t5),
      end = c(t2, t4, NA_POSIXct_, t6),
      size = c(2L, 2L, 0L, 1L)
    )
  )

  # NA's
  expect_equal(
    phint_unnest(c(int12, na_phint, int34)),
    tibble::tibble(
      key = c(1, 2, 3),
      start = c(t1, NA_POSIXct_, t3),
      end = c(t2, NA_POSIXct_, t4)
    )
  )
  expect_equal(
    phint_unnest(c(int12, na_phint, int34), keep_size = TRUE),
    tibble::tibble(
      key = c(1, 2, 3),
      start = c(t1, NA_POSIXct_, t3),
      end = c(t2, NA_POSIXct_, t4),
      size = c(1L, NA_integer_, 1L)
    )
  )

  # Instants
  expect_equal(
    phint_unnest(int11),
    tibble::tibble(
      key = 1,
      start = t1,
      end = t1
    )
  )
  expect_equal(
    phint_unnest(c(int11, int12)),
    tibble::tibble(
      key = c(1, 2),
      start = c(t1, t1),
      end = c(t1, t2)
    )
  )
})

test_that("phint_unnest() handles infinite bounds", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:05:00", tz = "UTC")
  t_neg_inf <- as.POSIXct(-Inf, tz = "UTC")
  t_pos_inf <- as.POSIXct(Inf, tz = "UTC")

  int_inf <- phinterval(t_neg_inf, t_pos_inf)
  int_neg_inf <- phinterval(t_neg_inf, t2)
  int_pos_inf <- phinterval(t1, t_pos_inf)
  phint_inf <- phinterval(c(t_neg_inf, t2), c(t1, t_pos_inf), by = 1)

  expect_equal(
    phint_unnest(int_inf),
    tibble::tibble(
      key = 1,
      start = t_neg_inf,
      end = t_pos_inf
    )
  )
  expect_equal(
    phint_unnest(c(int_neg_inf, int_pos_inf)),
    tibble::tibble(
      key = c(1, 2),
      start = c(t_neg_inf, t1),
      end = c(t2, t_pos_inf)
    )
  )
  expect_equal(
    phint_unnest(phint_inf),
    tibble::tibble(
      key = c(1, 1),
      start = c(t_neg_inf, t2),
      end = c(t1, t_pos_inf)
    )
  )
})

test_that("phint_unnest() preserves time zone", {
  t1_utc <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2_utc <- as.POSIXct("2021-01-01 00:05:00", tz = "UTC")

  t1_est <- as.POSIXct("2021-01-01 00:00:00", tz = "America/New_York")
  t2_est <- as.POSIXct("2021-01-01 00:05:00", tz = "America/New_York")

  phint_utc <- phinterval(t1_utc, t2_utc)
  phint_est <- phinterval(t1_est, t2_est)

  result_utc <- phint_unnest(phint_utc)
  result_est <- phint_unnest(phint_est)

  expect_equal(attr(result_utc$start, "tzone"), "UTC")
  expect_equal(attr(result_utc$end, "tzone"), "UTC")
  expect_equal(attr(result_est$start, "tzone"), "America/New_York")
  expect_equal(attr(result_est$end, "tzone"), "America/New_York")
})

test_that("phint_unnest() works with large multi-span elements", {
  starts <- as.POSIXct(seq(10, 900, by = 10), tz = "UTC")
  ends <- starts + 5

  phint_large <- phinterval(starts, ends, by = 1)
  result <- phint_unnest(phint_large)

  expect_equal(nrow(result), 90)
  expect_equal(result$key, rep(1, 90))
  expect_equal(result$start, starts)
  expect_equal(result$end, ends)

  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:05:00", tz = "UTC")

  expect_equal(
    phint_unnest(c(phint_large, phinterval(t1, t2))),
    rbind(result, tibble::tibble(key = 2, start = t1, end = t2))
  )
})

test_that("phint_unnest() with keep_size shows correct size for all rows", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:05:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:15:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t_neg_inf <- as.POSIXct(-Inf, tz = "UTC")
  t_pos_inf <- as.POSIXct(Inf, tz = "UTC")
  t_na <- as.POSIXct(NA, tz = "UTC")

  int12 <- phinterval(t1, t2)
  phint34_55 <- phinterval(c(t3, t5), c(t4, t5), by = 1)
  hole <- hole(tzone = "UTC")
  phint_inf1 <- phinterval(t_neg_inf, t_pos_inf)
  phint_inf2 <- phinterval(c(t_neg_inf, t2), c(t1, t_pos_inf), by = 1L)
  phint_na <- phinterval(t_na, t_na)

  expect_equal(
    phint_unnest(
      c(int12, phint34_55, hole, phint_inf1, phint_inf2, phint_na),
      keep_size = TRUE,
      hole_to = "na"
    ),
    tibble::tibble(
      key = c(1, 2, 2, 3, 4, 5, 5, 6),
      start = c(t1, t3, t5, t_na, t_neg_inf, t_neg_inf, t2, t_na),
      end = c(t2, t4, t5, t_na, t_pos_inf, t1, t_pos_inf, t_na),
      size = c(1L, 2L, 2L, 0L, 1L, 2L, 2L, NA_integer_)
    )
  )
  expect_equal(
    phint_unnest(
      c(int12, phint34_55, hole, phint_inf1, phint_inf2, phint_na),
      keep_size = TRUE,
      hole_to = "drop"
    ),
    tibble::tibble(
      key = c(1, 2, 2, 4, 5, 5, 6),
      start = c(t1, t3, t5, t_neg_inf, t_neg_inf, t2, t_na),
      end = c(t2, t4, t5, t_pos_inf, t1, t_pos_inf, t_na),
      size = c(1L, 2L, 2L, 1L, 2L, 2L, NA_integer_)
    )
  )
})

test_that("phint_unnest() works with `key` as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:05:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:15:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:25:00", tz = "UTC")

  int12 <- phinterval(t1, t2)
  int34 <- phinterval(t3, t4)
  int56 <- phinterval(t5, t6)

  phint12_34 <- phinterval(c(t1, t3), c(t2, t4), by = 1)
  phint12_34_56 <- phinterval(c(t1, t3, t5), c(t2, t4, t6), by = 1)

  hole <- hole(tzone = "UTC")
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")

  # key with scalar phintervals
  expect_equal(
    phint_unnest(c(int12, int34, int56), key = c("A", "B", "C")),
    tibble::tibble(
      key = c("A", "B", "C"),
      start = c(t1, t3, t5),
      end = c(t2, t4, t6)
    )
  )

  # key with multi-span phintervals
  expect_equal(
    phint_unnest(c(int12, phint12_34, int56), key = c("A", "B", "C")),
    tibble::tibble(
      key = c("A", "B", "B", "C"),
      start = c(t1, t1, t3, t5),
      end = c(t2, t2, t4, t6)
    )
  )
  expect_equal(
    phint_unnest(c(phint12_34, int56, phint12_34_56), key = 1:3),
    tibble::tibble(
      key = c(1, 1, 2, 3, 3, 3),
      start = c(t1, t3, t5, t1, t3, t5),
      end = c(t2, t4, t6, t2, t4, t6)
    )
  )

  # key with holes (drop)
  expect_equal(
    phint_unnest(c(int12, hole, int34), key = c("A", "B", "C"), hole_to = "drop"),
    tibble::tibble(
      key = c("A", "C"),
      start = c(t1, t3),
      end = c(t2, t4)
    )
  )
  expect_equal(
    phint_unnest(c(hole, phint12_34, hole), key = c("A", "B", "C"), hole_to = "drop"),
    tibble::tibble(
      key = c("B", "B"),
      start = c(t1, t3),
      end = c(t2, t4)
    )
  )

  # key with holes (na)
  expect_equal(
    phint_unnest(c(int12, hole, int34), key = c("A", "B", "C"), hole_to = "na"),
    tibble::tibble(
      key = c("A", "B", "C"),
      start = c(t1, NA_POSIXct_, t3),
      end = c(t2, NA_POSIXct_, t4)
    )
  )
  expect_equal(
    phint_unnest(c(phint12_34, hole, int56), key = c("A", "B", "C"), hole_to = "na"),
    tibble::tibble(
      key = c("A", "A", "B", "C"),
      start = c(t1, t3, NA_POSIXct_, t5),
      end = c(t2, t4, NA_POSIXct_, t6)
    )
  )

  # key with keep_size = TRUE
  expect_equal(
    phint_unnest(c(int12, phint12_34, int56), key = c("A", "B", "C"), keep_size = TRUE),
    tibble::tibble(
      key = c("A", "B", "B", "C"),
      start = c(t1, t1, t3, t5),
      end = c(t2, t2, t4, t6),
      size = c(1L, 2L, 2L, 1L)
    )
  )

  # key with factor
  expect_equal(
    phint_unnest(c(int12, int34, int56), key = factor(c("A", "B", "A"))),
    tibble::tibble(
      key = factor(c("A", "B", "A"), levels = c("A", "B")),
      start = c(t1, t3, t5),
      end = c(t2, t4, t6)
    )
  )

  # key with data.frame
  key <- data.frame(x = c("A", "B"), y = 1:2)
  set_rownames <- function(x, nms) {
    rownames(x) <- nms
    x
  }
  expect_equal(
    phint_unnest(c(int12, phint12_34), key = key),
    tibble::tibble(
      key = set_rownames(key[c(1, 2, 2), ], 1:3),
      start = c(t1, t1, t3),
      end = c(t2, t2, t4)
    )
  )

  # key = NULL returns default behavior (row indices)
  expect_equal(
    phint_unnest(c(int12, phint12_34), key = NULL),
    phint_unnest(c(int12, phint12_34))
  )

  # key with empty phinterval
  expect_equal(
    phint_unnest(phinterval(tzone = "UTC"), key = character()),
    tibble::tibble(
      key = character(),
      start = as.POSIXct(numeric(), tz = "UTC"),
      end = as.POSIXct(numeric(), tz = "UTC")
    )
  )

  # key errors when length doesn't match
  expect_error(phint_unnest(c(int12, int34), key = c("A", "B", "C")))
})
