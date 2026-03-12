# phint_squash -----------------------------------------------------------------

test_that("phint_squash() respects empty_to argument", {
  phint <- phinterval(tzone = "UTC")
  intvl <- interval(tzone = "UTC")

  empty <- phinterval(tzone = "UTC")
  na <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")
  hole <- hole(tzone = "UTC")

  expect_equal(phint_squash(phint), hole)
  expect_equal(phint_squash(phint, empty_to = "hole"), hole)
  expect_equal(phint_squash(phint, empty_to = "na"), na)

  expect_equal(phint_squash(intvl, empty_to = "hole"), hole)
  expect_equal(phint_squash(intvl, empty_to = "na"), na)
})

test_that("phint_squash() respects na_rm argument", {
  d1 <- as.Date("2021-01-01")
  d2 <- as.Date("2021-01-02")
  d3 <- as.Date("2021-01-04")
  d4 <- as.Date("2021-01-05")

  na_int <- interval(NA, NA, tzone = "UTC")
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")

  int12 <- phinterval(d1, d2)
  int34 <- phinterval(d3, d4)
  phint <- phint_squash(c(int12, int34))

  expect_equal(phint_squash(na_int, na_rm = TRUE), na_phint)
  expect_equal(phint_squash(c(na_int, na_int), na_rm = TRUE), na_phint)
  expect_equal(phint_squash(na_int, na_rm = FALSE), na_phint)
  expect_equal(phint_squash(na_phint, na_rm = TRUE), na_phint)
  expect_equal(phint_squash(c(na_phint, na_phint), na_rm = TRUE), na_phint)
  expect_equal(phint_squash(na_phint, na_rm = FALSE), na_phint)

  expect_equal(phint_squash(c(int12, int34, na_int), na_rm = TRUE), phint)
  expect_equal(phint_squash(c(int12, int34, na_int), na_rm = FALSE), na_phint)
})

test_that("phint_squash() works with <Interval> or <phinterval> inputs", {
  starts <- as.POSIXct(seq(0, 100, by = 10), tz = "UTC")
  ends <- starts + seq(-25, 25, by = 5)

  expect_equal(
    phint_squash(interval(starts, ends)),
    phint_squash(phinterval(starts, ends))
  )
})

test_that("phint_squash() errors on invalid inputs", {
  expect_error(phint_squash(phinterval(), na_rm = NA))
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
  t_neg_inf <- as.POSIXct(-Inf, tz = "UTC")
  t_pos_inf <- as.POSIXct(Inf, tz = "UTC")

  int12 <- phinterval(t1, t2)
  int13 <- phinterval(t1, t3)
  int15 <- phinterval(t1, t5)
  int16 <- phinterval(t1, t6)
  int22 <- phinterval(t2, t2)
  int23 <- phinterval(t2, t3)
  int24 <- phinterval(t2, t4)
  int34 <- phinterval(t3, t4)
  int45 <- phinterval(t4, t5)
  int14 <- phinterval(t1, t4)
  int35 <- phinterval(t3, t5)
  int36 <- phinterval(t3, t6)
  int46 <- phinterval(t4, t6)
  int56 <- phinterval(t5, t6)

  phint12_46 <- phinterval(c(t1, t4), c(t2, t6), by = 1)
  phint23_45 <- phinterval(c(t2, t4), c(t3, t5), by = 1)
  phint12_34 <- phinterval(c(t1, t3), c(t2, t4), by = 1)
  phint12_34_56 <- phinterval(c(t1, t3, t5), c(t2, t4, t6), by = 1)

  hole <- hole(tzone = "UTC")

  # Squash scalar
  expect_equal(phint_squash(int22), int22)
  expect_equal(phint_squash(int12), int12)
  expect_equal(phint_squash(hole), hole)
  expect_equal(phint_squash(phint12_46), phint12_46)
  expect_equal(phint_squash(phint12_34_56), phint12_34_56)

  # Squash abutting
  expect_equal(phint_squash(c(int12, int23)), int13)
  expect_equal(phint_squash(c(int46, int14)), int16)
  expect_equal(
    phint_squash(c(phint12_46, int23)),
    phinterval(c(t1, t4), c(t3, t6), by = 1)
  )
  expect_equal(
    phint_squash(c(phint12_34, int23)),
    int14
  )
  expect_equal(
    phint_squash(c(phint12_34_56, int45)),
    phinterval(c(t1, t3), c(t2, t6), by = 1)
  )
  expect_equal(
    phint_squash(c(phint12_46, int24)),
    phinterval(t1, t6)
  )
  expect_equal(
    phint_squash(c(int45, phint12_34)),
    phinterval(c(t1, t3), c(t2, t5), by = 1)
  )

  # Squash overlapping
  expect_equal(phint_squash(c(int12, int14)), int14)
  expect_equal(phint_squash(c(int12, int14)), int14)
  expect_equal(phint_squash(c(int12, int22)), int12)
  expect_equal(phint_squash(c(int14, int35)), int15)
  expect_equal(phint_squash(c(int46, int35)), int36)
  expect_equal(
    phint_squash(c(phint12_46, int14)),
    int16
  )
  expect_equal(
    phint_squash(c(phint12_46, phint23_45)),
    phinterval(c(t1, t4), c(t3, t6), by = 1)
  )
  expect_equal(
    phint_squash(c(phint12_34, phinterval(t2, t5))),
    int15
  )
  expect_equal(
    phint_squash(c(phint12_46, int35)),
    phinterval(c(t1, t3), c(t2, t6), by = 1)
  )

  # Squash non-overlapping
  expect_equal(
    phint_squash(c(int12, int34, int45)),
    phinterval(c(t1, t3, t4), c(t2, t4, t5), by = 1)
  )
  expect_equal(
    phint_squash(c(hole, int12, int34)),
    phinterval(c(t1, t3), c(t2, t4), by = 1)
  )
  expect_equal(
    phint_squash(c(phint12_34, int56)),
    phint12_34_56
  )
  expect_equal(
    phint_squash(c(hole, phint12_46)),
    phint12_46
  )

  # Squash infinite
  expect_equal(
    phint_squash(phinterval(t_neg_inf, t_pos_inf)),
    phinterval(t_neg_inf, t_pos_inf)
  )
  expect_equal(
    phint_squash(phinterval(c(t_neg_inf, t1), c(t_pos_inf, t2))),
    phinterval(t_neg_inf, t_pos_inf)
  )
  expect_equal(
    phint_squash(c(phint12_46, phinterval(t_neg_inf, t4))),
    phinterval(t_neg_inf, t6)
  )
  expect_equal(
    phint_squash(c(phint12_46, phinterval(t5, t_pos_inf))),
    phinterval(c(t1, t4), c(t2, t_pos_inf), by = 1)
  )
  expect_equal(
    phint_squash(c(
      phinterval(c(t_neg_inf, t3), c(t5, t_pos_inf), by = 1),
      int35
    )),
    phinterval(t_neg_inf, t_pos_inf)
  )
})

# phint_squash_by --------------------------------------------------------------

test_that("phint_squash_by() works with <Interval> or <phinterval> inputs", {
  starts <- as.POSIXct(seq(0, 100, by = 10), tz = "UTC")
  ends <- starts + seq(-25, 25, by = 5)
  by <- seq_along(starts) %% 3

  expect_equal(
    phint_squash_by(interval(starts, ends), by = by),
    phint_squash_by(phinterval(starts, ends), by = by)
  )
})

test_that("phint_squash_by() errors on invalid inputs", {
  expect_error(phint_squash_by(phinterval(), by = 1L, na_rm = NA))
  expect_error(phint_squash_by(phinterval(), by = 1L, empty_to = ""))
  expect_error(phint_squash_by(phinterval(), by = 1L, empty_to = 10))
  expect_error(phint_squash_by(as.Date("2020-01-01"), by = 1L))

  # Recycling
  phint <- as_phinterval(as.POSIXct("2021-01-01 00:00:00", tz = "UTC"))
  expect_error(phint_squash_by(phint, by = logical()))
  expect_error(phint_squash_by(phint, by = 1:2))
  expect_error(phint_squash_by(rep(phint, 3), by = 1:2))
})

test_that("phint_squash_by() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:05:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:15:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:25:00", tz = "UTC")
  t_neg_inf <- as.POSIXct(-Inf, tz = "UTC")
  t_pos_inf <- as.POSIXct(Inf, tz = "UTC")
  hole <- hole(tzone = "UTC")

  phint <- phinterval(
    c(t1, t2, t3, t5, t5, t_neg_inf, NA_POSIXct_),
    c(t2, t3, t4, t5, t6, t_pos_inf, NA_POSIXct_)
  )
  phint <- c(phint, hole)

  # `by` with groups of size 1
  expect_equal(
    phint_squash_by(phint, by = 1:8),
    tibble::tibble(by = 1:8, phint = phint)
  )
  expect_equal(
    phint_squash_by(phint, by = 8:1, order_by = TRUE),
    tibble::tibble(by = 1:8, phint = rev(phint))
  )
  expect_equal(
    phint_squash_by(phint, by = 8:1, order_by = FALSE),
    tibble::tibble(by = 8:1, phint = phint)
  )

  # order_by = FALSE
  expect_equal(
    phint_squash_by(phint, by = c(8, 8, 2, 2, 2, 3, 3, 3), order_by = FALSE)$phint,
    c(
      phint_squash(phint[1:2]),
      phint_squash(phint[3:5]),
      phint_squash(phint[6:8])
    )
  )
  expect_equal(
    phint_squash_by(phint, by = c(8, 8, 2, 2, 2, 3, 3, 3), order_by = FALSE, na_rm = FALSE)$phint,
    c(
      phint_squash(phint[1:2], na_rm = FALSE),
      phint_squash(phint[3:5], na_rm = FALSE),
      phint_squash(phint[6:8], na_rm = FALSE)
    )
  )
  expect_equal(
    phint_squash_by(phint, by = c(1, 2, 3, 2, 3, 3, 4, 1), order_by = FALSE)$phint,
    c(
      phint_squash(phint[c(1, 8)]),
      phint_squash(phint[c(2, 4)]),
      phint_squash(phint[c(3, 5:6)]),
      phint_squash(phint[7])
    )
  )

  # order_by = TRUE
  expect_equal(
    phint_squash_by(phint, by = c(8, 8, 2, 2, 2, 3, 3, 3), order_by = TRUE)$phint,
    c(
      phint_squash(phint[3:5]), # by = 2
      phint_squash(phint[6:8]), # by = 3
      phint_squash(phint[1:2])  # by = 8
    )
  )
  expect_equal(
    phint_squash_by(phint, by = c(1, 4, 3, 4, 3, 3, 2, 1), order_by = TRUE)$phint,
    c(
      phint_squash(phint[c(1, 8)]),   # by = 1
      phint_squash(phint[7]),         # by = 2
      phint_squash(phint[c(3, 5:6)]), # by = 3
      phint_squash(phint[c(2, 4)])    # by = 4
    )
  )

  # phint_squash() and phint_squash_by(by = 1) are equivalent
  expect_equal(phint_squash_by(phint[1:4], by = 1)$phint, phint_squash(phint[1:4]))
  expect_equal(phint_squash_by(phint, by = 1)$phint, phint_squash(phint))
  expect_equal(
    phint_squash_by(phint, by = 1, na_rm = FALSE)$phint,
    phint_squash(phint, na_rm = FALSE)
  )

  # `by` works with a data.frame
  by <- data.frame(
    x = c("E", "E", "D", "D", "C", "C", "B", "A"),
    y = c(1, 1, 1, 1, 2, 2, 3, 3)
  )
  expect_equal(
    phint_squash_by(phint, by = by, order_by = FALSE),
    tibble::tibble(
      by = vec_unique(by),
      phint = c(
        phint_squash(phint[1:2]), # E1
        phint_squash(phint[3:4]), # D1
        phint_squash(phint[5:6]), # C2
        phint_squash(phint[7]),   # B3
        phint_squash(phint[8])    # A3
      )
    )
  )
  expect_equal(
    phint_squash_by(phint, by = by, order_by = TRUE),
    tibble::tibble(
      by = vec_sort(vec_unique(by)),
      phint = c(
        phint_squash(phint[8]),   # A3
        phint_squash(phint[7]),   # B3
        phint_squash(phint[5:6]), # C2
        phint_squash(phint[3:4]), # D1
        phint_squash(phint[1:2])  # E1
      )
    )
  )
})

test_that("phint_squash_by() with handles empty inputs correctly", {
  empty <- phinterval(tzone = "UTC")

  # Empty input with size-0 by
  expect_equal(
    phint_squash_by(empty, by = character(), empty_to = "hole"),
    tibble::tibble(
      by = NA_character_,
      phint = hole(tzone = "UTC")
    )
  )
  expect_equal(
    phint_squash_by(empty, by = integer(), empty_to = "na"),
    tibble::tibble(
      by = NA_integer_,
      phint = phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")
    )
  )
  expect_equal(
    phint_squash_by(
      empty,
      by = data.frame(x = numeric(), y = logical()),
      empty_to = "hole"
    ),
    tibble::tibble(
      by = data.frame(x = NA_real_, y = NA),
      phint = hole(tzone = "UTC")
    )
  )

  # Empty input with size-1 by
  expect_equal(
    phint_squash_by(empty, by = "A", empty_to = "hole"),
    tibble::tibble(
      by = "A",
      phint = hole(tzone = "UTC")
    )
  )
  expect_equal(
    phint_squash_by(empty, by = 1L, empty_to = "na"),
    tibble::tibble(
      by = 1L,
      phint = phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")
    )
  )
  expect_equal(
    phint_squash_by(empty, by = data.frame(x = 1.0, y = TRUE), empty_to = "na"),
    tibble::tibble(
      by = data.frame(x = 1.0, y = TRUE),
      phint = phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")
    )
  )
})

test_that("phint_squash_by() with `order_by = TRUE` matches dplyr::group_by() order", {
  skip_on_cran() # Skipping as {dplyr} is just a Suggest

  `%>%` <- dplyr::`%>%`

  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:05:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:15:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:25:00", tz = "UTC")
  t_neg_inf <- as.POSIXct(-Inf, tz = "UTC")
  t_pos_inf <- as.POSIXct(Inf, tz = "UTC")
  hole <- hole(tzone = "UTC")

  phint <- phinterval(
    c(t1, t2, t3, t5, t5, t_neg_inf, NA_POSIXct_),
    c(t2, t3, t4, t5, t6, t_pos_inf, NA_POSIXct_)
  )
  phint <- c(phint, hole)

  phint_tib <- dplyr::tibble(
    phint = phint,
    by = c(1, 4, 3, 4, 3, 3, 2, 1),
    x = c("E", "E", "D", "D", "C", "C", "B", "A"),
    y = c(1, 1, 1, 1, 2, 2, 3, 3)
  )

  expect_equal(
    phint_squash_by(
      phint,
      by = phint_tib$by,
      order_by = TRUE
    ) %>% dplyr::pull(phint),
    phint_tib %>%
      dplyr::group_by(by) %>%
      dplyr::summarise(phint = phint_squash(phint), .groups = "drop") %>%
      dplyr::pull(phint)
  )
  expect_equal(
    phint_squash_by(
      phint,
      by = dplyr::select(phint_tib, x, y),
      order_by = TRUE
    ) %>% dplyr::pull(phint),
    phint_tib %>%
      dplyr::group_by(x, y) %>%
      dplyr::summarise(phint = phint_squash(phint), .groups = "drop") %>%
      dplyr::pull(phint)
  )
})

# datetime_squash --------------------------------------------------------------

test_that("datetime_squash() respects empty_to argument", {
  starts <- as.POSIXct(numeric(), tz = "UTC")
  ends <- as.POSIXct(numeric(), tz = "UTC")

  empty <- phinterval(tzone = "UTC")
  na <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")
  hole <- hole(tzone = "UTC")

  expect_equal(datetime_squash(starts, ends), hole)
  expect_equal(datetime_squash(starts, ends, empty_to = "hole"), hole)
  expect_equal(datetime_squash(starts, ends, empty_to = "na"), na)
})

test_that("datetime_squash() errors on invalid inputs", {
  t0 <- as.POSIXct(numeric(), tz = "UTC")
  t1 <- as.POSIXct(1, tz = "UTC")
  t3 <- as.POSIXct(2:4, tz = "UTC")

  expect_error(datetime_squash(t1, 10))
  expect_error(datetime_squash("A", t1))
  expect_error(datetime_squash(t1, t1, na_rm = NA))
  expect_error(datetime_squash(t0, t0, empty_to = "span"))

  # Recycling
  expect_error(datetime_squash(t3, t0))
  expect_error(datetime_squash(rep(t1, 2), t3))
})

test_that("datetime_squash() recycles inputs", {
  t0 <- as.POSIXct(numeric(), tz = "UTC")
  t1 <- as.POSIXct(1, tz = "UTC")
  t3 <- as.POSIXct(2:4, tz = "UTC")

  expect_equal(datetime_squash(t1, t3), datetime_squash(rep(t1, 3), t3))
  expect_equal(datetime_squash(t3, t1), datetime_squash(t3, rep(t1, 3)))
  expect_equal(datetime_squash(t0, t1), datetime_squash(t0, t0))
  expect_equal(datetime_squash(t1, t0), datetime_squash(t0, t0))
})

test_that("datetime_squash() standardizes starts/ends", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:05:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:15:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:25:00", tz = "UTC")
  t_na <- as.POSIXct(NA, tz = "UTC")
  t_neg_inf <- as.POSIXct(-Inf, tz = "UTC")
  t_pos_inf <- as.POSIXct(Inf, tz = "UTC")

  expect_equal(datetime_squash(t2, t1), phinterval(t1, t2))
  expect_equal(datetime_squash(t1, t_na), phinterval(t_na, t_na))
  expect_equal(datetime_squash(t_na, t1), phinterval(t_na, t_na))

  expect_equal(
    datetime_squash(t_pos_inf, t_neg_inf),
    phinterval(t_neg_inf, t_pos_inf)
  )
  expect_equal(datetime_squash(t_pos_inf, t1), phinterval(t1, t_pos_inf))
  expect_equal(datetime_squash(t1, t_neg_inf), phinterval(t_neg_inf, t1))

  expect_equal(
    datetime_squash(c(t1, t4, t6, t6, t2), c(t2, t3, t5, t6, t4)),
    datetime_squash(c(t1, t3, t5, t6, t4), c(t2, t4, t6, t6, t2))
  )
})

test_that("datetime_squash() respects na_rm argument", {
  d1 <- as.Date("2021-01-01")
  d2 <- as.Date("2021-01-02")
  d3 <- as.Date("2021-01-04")
  d4 <- as.Date("2021-01-05")
  d_na <- as.Date(NA)

  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")
  phint <- phinterval(c(d1, d3), c(d2, d4), by = 1)

  expect_equal(datetime_squash(d_na, d_na, na_rm = TRUE), na_phint)
  expect_equal(datetime_squash(rep(d_na, 2), rep(d_na, 2), na_rm = TRUE), na_phint)
  expect_equal(datetime_squash(d_na, d1, na_rm = TRUE), na_phint)
  expect_equal(datetime_squash(d1, d_na, na_rm = TRUE), na_phint)

  expect_equal(datetime_squash(d_na, d_na, na_rm = FALSE), na_phint)
  expect_equal(datetime_squash(d1, d_na, na_rm = FALSE), na_phint)

  expect_equal(datetime_squash(c(d1, d3, d_na), c(d2, d4, d_na), na_rm = TRUE), phint)
  expect_equal(datetime_squash(c(d1, d3, d_na), c(d2, d4, d_na), na_rm = FALSE), na_phint)
})

test_that("datetime_squash() works as expected", {
  test_that("datetime_squash() works as expected", {
    t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
    t2 <- as.POSIXct("2021-01-01 00:05:00", tz = "UTC")
    t3 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
    t4 <- as.POSIXct("2021-01-01 00:15:00", tz = "UTC")
    t5 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
    t6 <- as.POSIXct("2021-01-01 00:25:00", tz = "UTC")
    t_na <- as.POSIXct(NA, tz = "UTC")
    t_neg_inf <- as.POSIXct(-Inf, tz = "UTC")
    t_pos_inf <- as.POSIXct(Inf, tz = "UTC")

    # Squash scalar
    expect_equal(datetime_squash(t2, t2), phinterval(t2, t2))
    expect_equal(datetime_squash(t1, t2), phinterval(t1, t2))

    # Squash abutting
    expect_equal(datetime_squash(c(t1, t2), c(t2, t3)), phinterval(t1, t3))
    expect_equal(datetime_squash(c(t4, t1), c(t6, t4)), phinterval(t1, t6))

    # Squash overlapping
    expect_equal(datetime_squash(c(t1, t1), c(t2, t4)), phinterval(t1, t4))
    expect_equal(datetime_squash(c(t1, t1), c(t2, t4)), phinterval(t1, t4))
    expect_equal(datetime_squash(c(t1, t2), c(t2, t2)), phinterval(t1, t2))
    expect_equal(datetime_squash(c(t1, t3), c(t4, t5)), phinterval(t1, t5))
    expect_equal(datetime_squash(c(t4, t3), c(t6, t5)), phinterval(t3, t6))

    # Squash non-overlapping
    expect_equal(
      datetime_squash(c(t1, t3, t4), c(t2, t4, t5)),
      phinterval(c(t1, t3, t4), c(t2, t4, t5), by = 1)
    )
    expect_equal(
      datetime_squash(c(t_na, t1, t3), c(t_na, t2, t4)),
      phinterval(c(t1, t3), c(t2, t4), by = 1)
    )

    # Squash infinite
    expect_equal(
      datetime_squash(t_neg_inf, t_pos_inf),
      phinterval(t_neg_inf, t_pos_inf)
    )
    expect_equal(
      datetime_squash(c(t_neg_inf, t1), c(t_pos_inf, t2)),
      phinterval(t_neg_inf, t_pos_inf)
    )
  })
})

# datetime_squash_by -----------------------------------------------------------

test_that("datetime_squash_by() errors on invalid inputs", {
  t0 <- as.POSIXct(numeric(), tz = "UTC")
  t1 <- as.POSIXct(1, tz = "UTC")
  t3 <- as.POSIXct(2:4, tz = "UTC")

  expect_error(datetime_squash_by(t1, 10, by = 1L))
  expect_error(datetime_squash_by("A", t1, by = 1L))

  expect_error(datetime_squash_by(t3, t3, by = 1:2))
  expect_error(datetime_squash_by(t3, t3, by = numeric()))
  expect_error(datetime_squash_by(t3, t3, by = mean))

  expect_error(datetime_squash_by(t1, t1, by = 1L, na_rm = NA))
  expect_error(datetime_squash_by(t0, t0, by = 1L, empty_to = "span"))
  expect_error(datetime_squash_by(t1, t1, by = 1L, order_by = NA))
})

test_that("datetime_squash_by() standardizes starts/ends", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:05:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:15:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:25:00", tz = "UTC")

  by <- c(4, 4, 2, 2, 1)
  expect_equal(
    datetime_squash_by(c(t1, t4, t6, t6, t2), c(t2, t3, t5, t6, t4), by = by),
    datetime_squash_by(c(t1, t3, t5, t6, t4), c(t2, t4, t6, t6, t2), by = by)
  )
  expect_equal(
    datetime_squash_by(c(t1, t4, t6, t6, t2), c(t2, t3, t5, t6, t4), by = by, order_by = TRUE),
    datetime_squash_by(c(t1, t3, t5, t6, t4), c(t2, t4, t6, t6, t2), by = by, order_by = TRUE)
  )
})

test_that("datetime_squash_by() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:05:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:15:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:25:00", tz = "UTC")
  t_neg_inf <- as.POSIXct(-Inf, tz = "UTC")
  t_pos_inf <- as.POSIXct(Inf, tz = "UTC")

  phint <- phinterval(
    c(t1, t2, t3, t5, t5, t_neg_inf, NA_POSIXct_, NA_POSIXct_),
    c(t2, t3, t4, t5, t6, t_pos_inf, NA_POSIXct_, NA_POSIXct_)
  )

  expect_same_squash <- function(phint, ...) {
    expect_equal(
      phint_squash_by(phint, ...),
      datetime_squash_by(phint_start(phint), phint_end(phint), ...)
    )
  }

  # `by` with groups of size 1
  expect_same_squash(phint, by = 1:8)
  expect_same_squash(phint, by = 8:1, order_by = TRUE)
  expect_same_squash(phint, by = 8:1, order_by = FALSE)

  # order_by = FALSE
  expect_same_squash(phint, by = c(8, 8, 2, 2, 2, 3, 3, 3), order_by = FALSE)
  expect_same_squash(phint, by = c(8, 8, 2, 2, 2, 3, 3, 3), order_by = FALSE, na_rm = FALSE)
  expect_same_squash(phint, by = c(1, 2, 3, 2, 3, 3, 4, 1), order_by = FALSE)

  # order_by = TRUE
  expect_same_squash(phint, by = c(8, 8, 2, 2, 2, 3, 3, 3), order_by = TRUE)
  expect_same_squash(phint, by = c(1, 4, 3, 4, 3, 3, 2, 1), order_by = TRUE)

  # datetime_squash() and datetime_squash_by(by = 1) are equivalent
  starts <- phint_start(phint)
  ends <- phint_end(phint)
  expect_equal(
    datetime_squash_by(starts[1:4], ends[1:4], by = 1)$phint,
    datetime_squash(starts[1:4], ends[1:4])
  )
  expect_equal(
    datetime_squash_by(starts, ends, by = 1)$phint,
    datetime_squash(starts, ends)
  )
  expect_equal(
    datetime_squash_by(starts, ends, by = 1, na_rm = FALSE)$phint,
    datetime_squash(starts, ends, na_rm = FALSE)
  )

  # `by` works with a data.frame
  by <- data.frame(
    x = c("E", "E", "D", "D", "C", "C", "B", "A"),
    y = c(1, 1, 1, 1, 2, 2, 3, 3)
  )
  expect_same_squash(phint, by = by, order_by = FALSE)
  expect_same_squash(phint, by = by, order_by = TRUE)
})

test_that("datetime_squash_by() with handles empty inputs correctly", {
  empty <- phinterval(tzone = "UTC")

  expect_same_squash <- function(phint, ...) {
    expect_equal(
      phint_squash_by(phint, ...),
      datetime_squash_by(phint_start(phint), phint_end(phint), ...)
    )
  }

  # Empty input with size-0 by
  expect_same_squash(empty, by = character(), empty_to = "hole")
  expect_same_squash(empty, by = integer(), empty_to = "na")
  expect_same_squash(
    empty,
    by = data.frame(x = numeric(), y = logical()),
    empty_to = "hole"
  )

  # Empty input with size-1 by
  expect_same_squash(empty, by = "A", empty_to = "hole")
  expect_same_squash(empty, by = 1L, empty_to = "na")
  expect_same_squash(empty, by = data.frame(x = 1.0, y = TRUE), empty_to = "na")
})
