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
  expect_equal(phint_squash(phint, empty_to = "empty"), empty)

  expect_equal(phint_squash(intvl, empty_to = "hole"), hole)
  expect_equal(phint_squash(intvl, empty_to = "na"), na)
  expect_equal(phint_squash(intvl, empty_to = "empty"), empty)
})

test_that("phint_squash() respects na.rm argument", {
  d1 <- as.Date("2021-01-01")
  d2 <- as.Date("2021-01-02")
  d3 <- as.Date("2021-01-04")
  d4 <- as.Date("2021-01-05")

  na_int <- interval(NA, NA, tzone = "UTC")
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")

  int12 <- phinterval(d1, d2)
  int34 <- phinterval(d3, d4)
  phint <- phint_squash(c(int12, int34))

  expect_equal(phint_squash(na_int, na.rm = TRUE), na_phint)
  expect_equal(phint_squash(c(na_int, na_int), na.rm = TRUE), na_phint)
  expect_equal(phint_squash(na_int, na.rm = FALSE), na_phint)
  expect_equal(phint_squash(na_phint, na.rm = TRUE), na_phint)
  expect_equal(phint_squash(c(na_phint, na_phint), na.rm = TRUE), na_phint)
  expect_equal(phint_squash(na_phint, na.rm = FALSE), na_phint)

  expect_equal(phint_squash(c(int12, int34, na_int), na.rm = TRUE), phint)
  expect_equal(phint_squash(c(int12, int34, na_int), na.rm = FALSE), na_phint)
})

test_that("phint_squash() works with <Interval> or <phinterval> inputs", {
  starts <- as.POSIXct(seq(0, 100, by = 10), tz = "UTC")
  ends <- starts + seq(-25, 25, by = 5)

  intvl <- interval(starts, ends)
  phint <- phinterval(starts, ends)

  expect_equal(phint_squash(phint), phint_squash(intvl))
  expect_equal(
    phint_squash(phint, by = 11:1, order_by = FALSE),
    phint_squash(intvl, by = 11:1, order_by = FALSE)
  )
  expect_equal(
    phint_squash(phint, by = 11:1, order_by = TRUE),
    phint_squash(intvl, by = 11:1, order_by = TRUE)
  )
  expect_equal(
    phint_squash(phint, by = c(1:5, 3:8), order_by = FALSE),
    phint_squash(intvl, by = c(1:5, 3:8), order_by = FALSE)
  )
})

test_that("phint_squash() errors on invalid inputs", {
  expect_error(phint_squash(phinterval(), na.rm = NA))
  expect_error(phint_squash(phinterval(), empty_to = ""))
  expect_error(phint_squash(phinterval(), empty_to = 10))
  expect_error(phint_squash(as.Date("2020-01-01")))
  expect_error(phint_squash(interval(NA, NA), order_by = "yes"))
  expect_error(phint_squash(as_phinterval(as.Date(1:2)), by = 1:3))
  expect_error(phint_squash(as_phinterval(as.Date(1:2)), by = mean))
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

test_that("phint_squash() with `by` and `order_by` works as expected", {
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
  expect_equal(phint_squash(phint, by = 1:8), phint)
  expect_equal(phint_squash(phint, by = 8:1, order_by = TRUE), rev(phint))

  # order_by = FALSE
  expect_equal(
    phint_squash(phint, by = c(8, 8, 2, 2, 2, 3, 3, 3), order_by = FALSE),
    c(
      phint_squash(phint[1:2]),
      phint_squash(phint[3:5]),
      phint_squash(phint[6:8])
    )
  )
  expect_equal(
    phint_squash(phint, by = c(8, 8, 2, 2, 2, 3, 3, 3), order_by = FALSE, na.rm = FALSE),
    c(
      phint_squash(phint[1:2], na.rm = FALSE),
      phint_squash(phint[3:5], na.rm = FALSE),
      phint_squash(phint[6:8], na.rm = FALSE)
    )
  )
  expect_equal(
    phint_squash(phint, by = c(1, 2, 3, 2, 3, 3, 4, 1), order_by = FALSE),
    c(
      phint_squash(phint[c(1, 8)]),
      phint_squash(phint[c(2, 4)]),
      phint_squash(phint[c(3, 5:6)]),
      phint_squash(phint[7])
    )
  )

  # order_by = TRUE
  expect_equal(
    phint_squash(phint, by = c(8, 8, 2, 2, 2, 3, 3, 3), order_by = TRUE),
    c(
      phint_squash(phint[3:5]), # by = 2
      phint_squash(phint[6:8]), # by = 3
      phint_squash(phint[1:2])  # by = 8
    )
  )
  expect_equal(
    phint_squash(phint, by = c(1, 4, 3, 4, 3, 3, 2, 1), order_by = TRUE),
    c(
      phint_squash(phint[c(1, 8)]),   # by = 1
      phint_squash(phint[7]),         # by = 2
      phint_squash(phint[c(3, 5:6)]), # by = 3
      phint_squash(phint[c(2, 4)])    # by = 4
    )
  )

  # phint_squash() and phint_squash(by = 1) are equivalent
  expect_equal(phint_squash(phint[1:4], by = 1), phint_squash(phint[1:4]))
  expect_equal(phint_squash(phint, by = 1), phint_squash(phint))
  expect_equal(
    phint_squash(phint, by = 1, na.rm = FALSE),
    phint_squash(phint, na.rm = FALSE)
  )

  # `by` works with a data.frame
  by <- data.frame(
    x = c("E", "E", "D", "D", "C", "C", "B", "A"),
    y = c(1, 1, 1, 1, 2, 2, 3, 3)
  )
  expect_equal(
    phint_squash(phint, by = by, order_by = FALSE),
    c(
      phint_squash(phint[1:2]), # E1
      phint_squash(phint[3:4]), # D1
      phint_squash(phint[5:6]), # C2
      phint_squash(phint[7]),   # B3
      phint_squash(phint[8])    # A3
    )
  )
  expect_equal(
    phint_squash(phint, by = by, order_by = TRUE),
    c(
      phint_squash(phint[8]),   # A3
      phint_squash(phint[7]),   # B3
      phint_squash(phint[5:6]), # C2
      phint_squash(phint[3:4]), # D1
      phint_squash(phint[1:2])  # E1
    )
  )
})

test_that("phint_squash() with `keep_by = TRUE` works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:05:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:15:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:25:00", tz = "UTC")
  t_neg_inf <- as.POSIXct(-Inf, tz = "UTC")
  t_pos_inf <- as.POSIXct(Inf, tz = "UTC")

  starts <- c(t1, t2, t3, t5, t5, t_neg_inf, NA_POSIXct_)
  ends <- c(t2, t3, t4, t5, t6, t_pos_inf, NA_POSIXct_)
  phint <- phinterval(starts, ends)

  # keep_by = TRUE errors when by = NULL
  expect_error(phint_squash(phint, keep_by = TRUE))

  # keep_by = TRUE with numeric by
  expect_equal(
    phint_squash(phint, by = c(8, 8, 2, 2, 2, 3, 3), keep_by = TRUE),
    datetime_squash(starts, ends, by = c(8, 8, 2, 2, 2, 3, 3), keep_by = TRUE)
  )

  # keep_by = TRUE with order_by = TRUE
  expect_equal(
    phint_squash(phint, by = c(8, 8, 2, 2, 2, 3, 3), keep_by = TRUE, order_by = TRUE),
    datetime_squash(starts, ends, by = c(8, 8, 2, 2, 2, 3, 3), keep_by = TRUE, order_by = TRUE)
  )

  # keep_by = TRUE with character by
  expect_equal(
    phint_squash(phint, by = c("A", "A", "B", "B", "B", "C", "C"), keep_by = TRUE),
    datetime_squash(starts, ends, by = c("A", "A", "B", "B", "B", "C", "C"), keep_by = TRUE)
  )

  # keep_by = TRUE with data.frame by
  by <- data.frame(
    x = c("E", "D", "D", "C", "C", "B", "A"),
    y = c(1, 1, 1, 2, 2, 3, 3)
  )

  expect_equal(
    phint_squash(phint, by = by, keep_by = TRUE, order_by = FALSE),
    datetime_squash(starts, ends, by = by, keep_by = TRUE, order_by = FALSE)
  )
  expect_equal(
    phint_squash(phint, by = by, keep_by = TRUE, order_by = TRUE),
    datetime_squash(starts, ends, by = by, keep_by = TRUE, order_by = TRUE)
  )

  # keep_by = TRUE with na.rm = FALSE
  expect_equal(
    phint_squash(phint, by = c(8, 8, 2, 2, 2, 3, 3), keep_by = TRUE, na.rm = FALSE),
    datetime_squash(starts, ends, by = c(8, 8, 2, 2, 2, 3, 3), keep_by = TRUE, na.rm = FALSE)
  )

  # keep_by = FALSE returns vector (default behavior)
  result_vector <- phint_squash(phint, by = c(8, 8, 2, 2, 2, 3, 3), keep_by = FALSE)
  expect_s3_class(result_vector, "phinterval")
  expect_equal(length(result_vector), 3)

  result_df <- phint_squash(phint, by = c(8, 8, 2, 2, 2, 3, 3), keep_by = TRUE)
  expect_s3_class(result_df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(result_df), 3)
  expect_equal(result_vector, result_df$phint)

  # With holes
  phint <- c(phint, hole())
  expect_equal(
    phint_squash(phint, by = c(8, 8, 2, 2, 2, 3, 3, 1), keep_by = TRUE),
    tibble::tibble(
      by = c(8, 2, 3, 1),
      phint = c(
        phint_squash(phint[1:2]),
        phint_squash(phint[3:5]),
        phint_squash(phint[6:7]),
        hole()
      )
    )
  )
})

test_that("phint_squash() with `keep_by = TRUE` handles empty inputs correctly", {
  empty <- phinterval(tzone = "UTC")

  # Empty input with length-0 by
  expect_equal(
    phint_squash(empty, by = character(), empty_to = "hole", keep_by = TRUE),
    tibble::tibble(
      by = NA_character_,
      phint = hole(tzone = "UTC")
    )
  )
  expect_equal(
    phint_squash(empty, by = integer(), empty_to = "na", keep_by = TRUE),
    tibble::tibble(
      by = NA_integer_,
      phint = phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")
    )
  )
  expect_equal(
    phint_squash(empty, by = numeric(), empty_to = "empty", keep_by = TRUE),
    tibble::tibble(
      by = numeric(),
      phint = phinterval(tzone = "UTC")
    )
  )

  # Empty input with length-1 by (gets sliced or kept based on empty_to)
  expect_equal(
    phint_squash(empty, by = "A", empty_to = "hole", keep_by = TRUE),
    tibble::tibble(
      by = "A",
      phint = hole(tzone = "UTC")
    )
  )
  expect_equal(
    phint_squash(empty, by = 1L, empty_to = "na", keep_by = TRUE),
    tibble::tibble(
      by = 1L,
      phint = phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")
    )
  )
  expect_equal(
    phint_squash(empty, by = 99, empty_to = "empty", keep_by = TRUE),
    tibble::tibble(
      by = numeric(),
      phint = phinterval(tzone = "UTC")
    )
  )

  # Empty input with length-1 data.frame by
  by_df <- data.frame(x = "A", y = 1)

  expect_equal(
    phint_squash(empty, by = by_df, empty_to = "hole", keep_by = TRUE),
    tibble::tibble(
      by = by_df,
      phint = hole(tzone = "UTC")
    )
  )
  expect_equal(
    phint_squash(empty, by = by_df, empty_to = "empty", keep_by = TRUE),
    tibble::tibble(
      by = by_df[0, ],
      phint = phinterval(tzone = "UTC"),
      row.names = NULL
    )
  )

  # Empty input with length-0 data.frame by
  by_df_empty <- data.frame(x = character(), y = integer())

  expect_equal(
    phint_squash(empty, by = by_df_empty, empty_to = "hole", keep_by = TRUE),
    tibble::tibble(
      by = data.frame(x = NA_character_, y = NA_integer_),
      phint = hole(tzone = "UTC")
    )
  )
  expect_equal(
    phint_squash(empty, by = by_df_empty, empty_to = "empty", keep_by = TRUE),
    tibble::tibble(
      by = by_df_empty,
      phint = phinterval(tzone = "UTC")
    )
  )
})

test_that("phint_squash() with `order_by = TRUE` matches dplyr::group_by() order", {
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
    phint_squash(phint, by = phint_tib$by, order_by = TRUE),
    phint_tib %>%
      dplyr::group_by(by) %>%
      dplyr::summarise(phint = phint_squash(phint), .groups = "drop") %>%
      dplyr::pull(phint)
  )
  expect_equal(
    phint_squash(phint, by = dplyr::select(phint_tib, x, y), order_by = TRUE),
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
  expect_equal(datetime_squash(starts, ends, empty_to = "empty"), empty)
})

test_that("datetime_squash() errors on invalid inputs", {
  t0 <- as.POSIXct(numeric(), tz = "UTC")
  t1 <- as.POSIXct(1, tz = "UTC")
  t3 <- as.POSIXct(2:4, tz = "UTC")

  expect_error(datetime_squash(t1, 10))
  expect_error(datetime_squash("A", t1))

  expect_error(datetime_squash(t3, t3, by = 1:2))
  expect_error(datetime_squash(t3, t3, by = numeric()))
  expect_error(datetime_squash(t3, t3, by = mean))

  expect_error(datetime_squash(t1, t1, na.rm = NA))
  expect_error(datetime_squash(t0, t0, empty_to = "span"))
  expect_error(datetime_squash(t1, t1, order_by = NA))
})

test_that("datetime_squash() recycles inputs", {
  t0 <- as.POSIXct(numeric(), tz = "UTC")
  t1 <- as.POSIXct(1, tz = "UTC")
  t3 <- as.POSIXct(2:4, tz = "UTC")

  expect_equal(datetime_squash(t1, t3), datetime_squash(rep(t1, 3), t3))
  expect_equal(datetime_squash(t3, t1), datetime_squash(t3, rep(t1, 3)))
  expect_equal(
    datetime_squash(t1, t3, by = c(1, 2, 2)),
    datetime_squash(rep(t1, 3), t3, by = c(1, 2, 2))
  )
  expect_equal(
    datetime_squash(t3, t1, by = c(1, 2, 2)),
    datetime_squash(t3, rep(t1, 3), by = c(1, 2, 2))
  )
  expect_equal(datetime_squash(t0, t1), datetime_squash(t0, t0))
  expect_equal(datetime_squash(t1, t0), datetime_squash(t0, t0))

  expect_error(datetime_squash(t3, t0))
  expect_error(datetime_squash(rep(t1, 2), t3))
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

  by <- c(4, 4, 2, 2, 1)
  expect_equal(
    datetime_squash(c(t1, t4, t6, t6, t2), c(t2, t3, t5, t6, t4), by = by),
    datetime_squash(c(t1, t3, t5, t6, t4), c(t2, t4, t6, t6, t2), by = by)
  )
  expect_equal(
    datetime_squash(c(t1, t4, t6, t6, t2), c(t2, t3, t5, t6, t4), by = by, order_by = TRUE),
    datetime_squash(c(t1, t3, t5, t6, t4), c(t2, t4, t6, t6, t2), by = by, order_by = TRUE)
  )
})

test_that("datetime_squash() respects na.rm argument", {
  d1 <- as.Date("2021-01-01")
  d2 <- as.Date("2021-01-02")
  d3 <- as.Date("2021-01-04")
  d4 <- as.Date("2021-01-05")
  d_na <- as.Date(NA)

  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")
  phint <- phinterval(c(d1, d3), c(d2, d4), by = 1)

  expect_equal(datetime_squash(d_na, d_na, na.rm = TRUE), na_phint)
  expect_equal(datetime_squash(rep(d_na, 2), rep(d_na, 2), na.rm = TRUE), na_phint)
  expect_equal(datetime_squash(d_na, d1, na.rm = TRUE), na_phint)
  expect_equal(datetime_squash(d1, d_na, na.rm = TRUE), na_phint)

  expect_equal(datetime_squash(d_na, d_na, na.rm = FALSE), na_phint)
  expect_equal(datetime_squash(d1, d_na, na.rm = FALSE), na_phint)

  expect_equal(datetime_squash(c(d1, d3, d_na), c(d2, d4, d_na), na.rm = TRUE), phint)
  expect_equal(datetime_squash(c(d1, d3, d_na), c(d2, d4, d_na), na.rm = FALSE), na_phint)
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

test_that("datetime_squash() with `by` and `order_by` works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:05:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:15:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:25:00", tz = "UTC")
  t_neg_inf <- as.POSIXct(-Inf, tz = "UTC")
  t_pos_inf <- as.POSIXct(Inf, tz = "UTC")

  starts <- c(t1, t2, t3, t5, t5, t_neg_inf, NA_POSIXct_)
  ends <- c(t2, t3, t4, t5, t6, t_pos_inf, NA_POSIXct_)
  phint <- phinterval(starts, ends)

  # `by` with groups of size 1
  expect_equal(datetime_squash(starts, ends, by = 1:7), phint)
  expect_equal(datetime_squash(starts, ends, by = 7:1, order_by = TRUE), rev(phint))

  # order_by = FALSE
  expect_equal(
    datetime_squash(starts, ends, by = c(8, 8, 2, 2, 2, 3, 3), order_by = FALSE),
    c(
      phint_squash(phint[1:2]),
      phint_squash(phint[3:5]),
      phint_squash(phint[6:7])
    )
  )
  expect_equal(
    datetime_squash(starts, ends, by = c(8, 8, 2, 2, 2, 3, 3), order_by = FALSE, na.rm = FALSE),
    c(
      phint_squash(phint[1:2], na.rm = FALSE),
      phint_squash(phint[3:5], na.rm = FALSE),
      phint_squash(phint[6:7], na.rm = FALSE)
    )
  )
  expect_equal(
    datetime_squash(starts, ends, by = c(1, 2, 3, 2, 3, 3, 1), order_by = FALSE),
    c(
      phint_squash(phint[c(1, 7)]),
      phint_squash(phint[c(2, 4)]),
      phint_squash(phint[c(3, 5:6)])
    )
  )

  # order_by = TRUE
  expect_equal(
    datetime_squash(starts, ends, by = c(8, 8, 2, 2, 2, 3, 3), order_by = TRUE),
    c(
      phint_squash(phint[3:5]), # by = 2
      phint_squash(phint[6:7]), # by = 3
      phint_squash(phint[1:2])  # by = 8
    )
  )
  expect_equal(
    datetime_squash(starts, ends, by = c(1, 4, 3, 4, 3, 3, 2), order_by = TRUE),
    c(
      phint_squash(phint[1]),         # by = 1
      phint_squash(phint[7]),         # by = 2
      phint_squash(phint[c(3, 5:6)]), # by = 3
      phint_squash(phint[c(2, 4)])    # by = 4
    )
  )

  # datetime_squash() and datetime_squash(by = 1) are equivalent
  expect_equal(
    datetime_squash(starts[1:4], ends[1:4], by = 1),
    datetime_squash(starts[1:4], ends[1:4])
  )
  expect_equal(
    datetime_squash(starts, ends, by = 1),
    datetime_squash(starts, ends)
  )
  expect_equal(
    datetime_squash(starts, ends, by = 1, na.rm = FALSE),
    datetime_squash(starts, ends, na.rm = FALSE)
  )

  # `by` works with a data.frame
  by <- data.frame(
    x = c("E", "D", "D", "C", "C", "B", "A"),
    y = c(1, 1, 1, 2, 2, 3, 3)
  )
  expect_equal(
    datetime_squash(starts, ends, by = by, order_by = FALSE),
    c(
      phint_squash(phint[1]),   # E1
      phint_squash(phint[2:3]), # D1
      phint_squash(phint[4:5]), # C2
      phint_squash(phint[6]),   # B3
      phint_squash(phint[7])    # A3
    )
  )
  expect_equal(
    datetime_squash(starts, ends, by = by, order_by = TRUE),
    c(
      phint_squash(phint[7]),   # A3
      phint_squash(phint[6]),   # B3
      phint_squash(phint[4:5]), # C2
      phint_squash(phint[2:3]), # D1
      phint_squash(phint[1])    # E1
    )
  )
})

test_that("datetime_squash() with `keep_by = TRUE` works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:05:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:15:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:25:00", tz = "UTC")
  t_neg_inf <- as.POSIXct(-Inf, tz = "UTC")
  t_pos_inf <- as.POSIXct(Inf, tz = "UTC")

  starts <- c(t1, t2, t3, t5, t5, t_neg_inf, NA_POSIXct_)
  ends <- c(t2, t3, t4, t5, t6, t_pos_inf, NA_POSIXct_)
  phint <- phinterval(starts, ends)

  # keep_by = TRUE errors when by = NULL
  expect_error(datetime_squash(starts, ends, keep_by = TRUE))

  # keep_by = TRUE with numeric by
  expect_equal(
    datetime_squash(starts, ends, by = c(8, 8, 2, 2, 2, 3, 3), keep_by = TRUE),
    tibble::tibble(
      by = c(8, 2, 3),
      phint = c(
        phint_squash(phint[1:2]),
        phint_squash(phint[3:5]),
        phint_squash(phint[6:7])
      )
    )
  )

  # keep_by = TRUE with order_by = TRUE
  expect_equal(
    datetime_squash(starts, ends, by = c(8, 8, 2, 2, 2, 3, 3), keep_by = TRUE, order_by = TRUE),
    tibble::tibble(
      by = c(2, 3, 8),
      phint = c(
        phint_squash(phint[3:5]), # by = 2
        phint_squash(phint[6:7]), # by = 3
        phint_squash(phint[1:2])  # by = 8
      )
    )
  )

  # keep_by = TRUE with character by
  expect_equal(
    datetime_squash(starts, ends, by = c("A", "A", "B", "B", "B", "C", "C"), keep_by = TRUE),
    tibble::tibble(
      by = c("A", "B", "C"),
      phint = c(
        phint_squash(phint[1:2]),
        phint_squash(phint[3:5]),
        phint_squash(phint[6:7])
      )
    )
  )

  # keep_by = TRUE with data.frame by
  by <- data.frame(
    x = c("E", "D", "D", "C", "C", "B", "A"),
    y = c(1, 1, 1, 2, 2, 3, 3)
  )
  set_rownames <- function(x, nms) {
    rownames(x) <- nms
    x
  }

  expect_equal(
    datetime_squash(starts, ends, by = by, keep_by = TRUE, order_by = FALSE),
    tibble::tibble(
      by = set_rownames(by[c(1, 2, 4, 6, 7), ], 1:5),
      phint = c(
        phint_squash(phint[1]),   # E1
        phint_squash(phint[2:3]), # D1
        phint_squash(phint[4:5]), # C2
        phint_squash(phint[6]),   # B3
        phint_squash(phint[7])    # A3
      )
    )
  )

  expect_equal(
    datetime_squash(starts, ends, by = by, keep_by = TRUE, order_by = TRUE),
    tibble::tibble(
      by = set_rownames(by[c(7, 6, 4, 2, 1), ], 1:5),
      phint = c(
        phint_squash(phint[7]),   # A3
        phint_squash(phint[6]),   # B3
        phint_squash(phint[4:5]), # C2
        phint_squash(phint[2:3]), # D1
        phint_squash(phint[1])    # E1
      )
    )
  )

  # keep_by = TRUE with na.rm = FALSE
  expect_equal(
    datetime_squash(starts, ends, by = c(8, 8, 2, 2, 2, 3, 3), keep_by = TRUE, na.rm = FALSE),
    tibble::tibble(
      by = c(8, 2, 3),
      phint = c(
        phint_squash(phint[1:2], na.rm = FALSE),
        phint_squash(phint[3:5], na.rm = FALSE),
        phint_squash(phint[6:7], na.rm = FALSE)
      )
    )
  )

  # keep_by = FALSE returns vector (default behavior)
  result_vector <- datetime_squash(starts, ends, by = c(8, 8, 2, 2, 2, 3, 3), keep_by = FALSE)
  expect_s3_class(result_vector, "phinterval")
  expect_equal(length(result_vector), 3)

  result_df <- datetime_squash(starts, ends, by = c(8, 8, 2, 2, 2, 3, 3), keep_by = TRUE)
  expect_s3_class(result_df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(result_df), 3)
  expect_equal(result_vector, result_df$phint)
})

test_that("datetime_squash() with `keep_by = TRUE` handles empty inputs correctly", {
  # Empty input with length-0 by
  expect_equal(
    datetime_squash(POSIXct(), POSIXct(), by = character(), empty_to = "hole", keep_by = TRUE),
    tibble::tibble(
      by = NA_character_,
      phint = hole(tzone = "UTC")
    )
  )
  expect_equal(
    datetime_squash(POSIXct(), POSIXct(), by = integer(), empty_to = "na", keep_by = TRUE),
    tibble::tibble(
      by = NA_integer_,
      phint = phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")
    )
  )
  expect_equal(
    datetime_squash(POSIXct(), POSIXct(), by = numeric(), empty_to = "empty", keep_by = TRUE),
    tibble::tibble(
      by = numeric(),
      phint = phinterval(tzone = "UTC")
    )
  )

  # Empty input with length-1 by (gets sliced or kept based on empty_to)
  expect_equal(
    datetime_squash(POSIXct(), POSIXct(), by = "A", empty_to = "hole", keep_by = TRUE),
    tibble::tibble(
      by = "A",
      phint = hole(tzone = "UTC")
    )
  )
  expect_equal(
    datetime_squash(POSIXct(), POSIXct(), by = 1L, empty_to = "na", keep_by = TRUE),
    tibble::tibble(
      by = 1L,
      phint = phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")
    )
  )
  expect_equal(
    datetime_squash(POSIXct(), POSIXct(), by = 99, empty_to = "empty", keep_by = TRUE),
    tibble::tibble(
      by = numeric(),
      phint = phinterval(tzone = "UTC")
    )
  )

  # Empty input with length-1 data.frame by
  by_df <- data.frame(x = "A", y = 1)

  expect_equal(
    datetime_squash(POSIXct(), POSIXct(), by = by_df, empty_to = "hole", keep_by = TRUE),
    tibble::tibble(
      by = by_df,
      phint = hole(tzone = "UTC")
    )
  )
  expect_equal(
    datetime_squash(POSIXct(), POSIXct(), by = by_df, empty_to = "empty", keep_by = TRUE),
    tibble::tibble(
      by = by_df[0, ],
      phint = phinterval(tzone = "UTC"),
      row.names = NULL
    )
  )

  # Empty input with length-0 data.frame by
  by_df_empty <- data.frame(x = character(), y = integer())

  expect_equal(
    datetime_squash(POSIXct(), POSIXct(), by = by_df_empty, empty_to = "hole", keep_by = TRUE),
    tibble::tibble(
      by = data.frame(x = NA_character_, y = NA_integer_),
      phint = hole(tzone = "UTC")
    )
  )
  expect_equal(
    datetime_squash(POSIXct(), POSIXct(), by = by_df_empty, empty_to = "empty", keep_by = TRUE),
    tibble::tibble(
      by = by_df_empty,
      phint = phinterval(tzone = "UTC")
    )
  )
})

test_that("datetime_squash() with `order_by = TRUE` matches dplyr::group_by() order", {
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

  start <- c(t1, t2, t3, t5, t5, t_neg_inf, NA_POSIXct_)
  end <- c(t2, t3, t4, t5, t6, t_pos_inf, NA_POSIXct_)

  phint_tib <- dplyr::tibble(
    start = start,
    end = end,
    by = c(1, 4, 3, 4, 3, 3, 2),
    x = c("E", "E", "D", "D", "C", "C", "B"),
    y = c(1, 1, 1, 1, 2, 2, 3)
  )

  expect_equal(
    datetime_squash(start, end, by = phint_tib$by, order_by = TRUE),
    phint_tib %>%
      dplyr::group_by(by) %>%
      dplyr::summarise(phint = datetime_squash(start, end), .groups = "drop") %>%
      dplyr::pull(phint)
  )
  expect_equal(
    datetime_squash(start, end, by = dplyr::select(phint_tib, x, y), order_by = TRUE),
    phint_tib %>%
      dplyr::group_by(x, y) %>%
      dplyr::summarise(phint = datetime_squash(start, end,), .groups = "drop") %>%
      dplyr::pull(phint)
  )
})
