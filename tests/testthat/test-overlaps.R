# locate_overlaps --------------------------------------------------------------
test_that("span count is correct", {
  origin <- .POSIXct(0L, tz = "UTC")
  int <- lubridate::interval(
    origin + lubridate::dseconds(c(0, 100, 200, 300, 400, 425)),
    origin + lubridate::dseconds(c(50, 250, 250, 500, 450, 430))
  )
  phint <- as_phinterval(int, tzone = "UTC")

  n <- c(1L, 2L, 3L)
  spans <- new_phinterval(
    reference_time = .POSIXct(rep.int(0, length(n)), tz = "UTC"),
    range_starts = list(
      c(0, 100, 300, 450),
      c(200, 400, 430),
      425
    ),
    range_ends = list(
      c(50, 200, 400, 500),
      c(250, 425, 450),
      430
    )
  )

  expect_identical(
    locate_overlaps(phint),
    list(n = n, spans = spans)
  )
})

test_that("non-overlapping spans result in `n = 1`", {
  int1_start <- lubridate::ymd(20000101, tz = "UTC")
  int1 <- lubridate::interval(
    int1_start + lubridate::dhours(c(0, 10, 30, 200)),
    int1_start + lubridate::dhours(c(5, 20, 40, 250))
  )
  int2_start <- lubridate::ymd(20201011, tz = "UTC")
  int2 <- lubridate::interval(int2_start, int2_start + lubridate::dhours(50))

  phint <- phinterval(intervals = list(int1, int2), tzone = "UTC")

  n <- 1L
  spans <- new_phinterval(
    reference_time = .POSIXct(rep.int(0, length(n)), tz = "UTC"),
    range_starts = list(
      c(
        (c(0, 10, 30, 200) * 60 * 60) + as.double(int1_start),
        (0 * 60 * 60) + as.double(int2_start)
      )
    ),
    range_ends = list(
      c(
        (c(5, 20, 40, 250) * 60 * 60) + as.double(int1_start),
        (50 * 60 * 60) + as.double(int2_start)
      )
    )
  )

  expect_identical(
    locate_overlaps(phint),
    list(n = n, spans = spans)
  )
})

test_that("aligned spans are counted correctly", {
  origin <- .POSIXct(0L, tz = "UTC")
  int <- lubridate::interval(
    origin + lubridate::dseconds(c(0, 100, 100, 100, 400, 400)),
    origin + lubridate::dseconds(c(50, 200, 200, 250, 500, 500))
  )
  phint <- as_phinterval(int, tzone = "UTC")

  n <- c(1L, 2L, 3L)
  spans <- new_phinterval(
    reference_time = .POSIXct(rep.int(0, length(n)), tz = "UTC"),
    range_starts = list(
      c(0, 200),
      400,
      100
    ),
    range_ends = list(
      c(50, 250),
      500,
      200
    )
  )

  expect_identical(
    locate_overlaps(phint),
    list(n = n, spans = spans)
  )
})

test_that("adjacent spans are counted correctly when `alignment = FALSE`.", {
  origin <- .POSIXct(0L, tz = "UTC")
  int <- lubridate::interval(
    origin + lubridate::dseconds(c(0, 100)),
    origin + lubridate::dseconds(c(100, 200))
  )
  phint <- as_phinterval(int, tzone = "UTC")

  n <- 1L
  spans <- new_phinterval(
    reference_time = .POSIXct(rep.int(0, length(n)), tz = "UTC"),
    range_starts = list(0),
    range_ends = list(200)
  )

  expect_identical(
    locate_overlaps(phint, alignment = FALSE),
    list(n = n, spans = spans)
  )
})

test_that("adjacent spans are counted correctly when `alignment = TRUE`", {
  origin <- .POSIXct(0L, tz = "UTC")
  int <- lubridate::interval(
    origin + lubridate::dseconds(c(0, 100)),
    origin + lubridate::dseconds(c(100, 200))
  )
  phint <- as_phinterval(int, tzone = "UTC")

  n <- c(1L, 2L)
  spans <- new_phinterval(
    reference_time = .POSIXct(rep.int(0, length(n)), tz = "UTC"),
    range_starts = list(
      0,
      100
    ),
    range_ends = list(
      200,
      100
    )
  )

  expect_identical(
    locate_overlaps(phint, alignment = TRUE),
    list(n = n, spans = spans)
  )
})

test_that("instants are counted as expected with `alignment = FALSE`", {
  origin <- .POSIXct(0L, tz = "UTC")
  int <- lubridate::interval(
    origin + lubridate::dseconds(c(0, 100, 200, 300, 300, 300, 300)),
    origin + lubridate::dseconds(c(0, 200, 200, 300, 300, 300, 300))
  )
  phint <- as_phinterval(int, tzone = "UTC")

  n <- c(1L, 2L, 4L)
  spans <- new_phinterval(
    reference_time = .POSIXct(rep.int(0, length(n)), tz = "UTC"),
    range_starts = list(
      c(0, 100),
      200,
      300
    ),
    range_ends = list(
      c(0, 200),
      200,
      300
    )
  )

  expect_identical(
    locate_overlaps(phint),
    list(n = n, spans = spans)
  )
})

test_that("nested overlaps are counted correctly", {
  origin <- .POSIXct(0L, tz = "UTC")
  int <- lubridate::interval(
    origin + lubridate::dseconds(c(0, 1, 2, 3)),
    origin + lubridate::dseconds(c(7, 6, 5, 4))
  )
  phint <- as_phinterval(int, tzone = "UTC")

  n <- c(1L, 2L, 3L, 4L)
  spans <- new_phinterval(
    reference_time = .POSIXct(rep.int(0, length(n)), tz = "UTC"),
    range_starts = list(
      c(0, 6),
      c(1, 5),
      c(2, 4),
      3
    ),
    range_ends = list(
      c(1, 7),
      c(2, 6),
      c(3, 5),
      4
    )
  )

  expect_identical(
    locate_overlaps(phint),
    list(n = n, spans = spans)
  )
})

# There is one edge case to make a discontinuous `phinterval` with `count == "exact"`.
# Ex. If you have the ranges [1, 3], [2, 2], then the output ranges for n = 1 are
# [1, 2], [2, 3] and n = 2 are [2, 2]. The n = 1 ranges have a point discontinuity,
# which should be flattened ([1, 2], [2, 3] -> [1, 3]).
test_that("instants don't create phinterval spans with point discontinuities", {
  origin <- .POSIXct(0L, tz = "UTC")
  int <- lubridate::interval(
    origin + lubridate::dseconds(c(1, 2)),
    origin + lubridate::dseconds(c(3, 2))
  )
  phint <- as_phinterval(int, tzone = "UTC")

  n <- c(1L, 2L)
  spans <- new_phinterval(
    reference_time = .POSIXct(rep.int(0, length(n)), tz = "UTC"),
    range_starts = list(
      1,
      2
    ),
    range_ends = list(
      3,
      2
    )
  )

  expect_identical(
    locate_overlaps(phint),
    list(n = n, spans = spans)
  )
})

test_that("completely NA inputs result in NA ouputs", {
  int <- lubridate::interval(NA_POSIXct_, NA_POSIXct_)
  phint <- as_phinterval(int, tzone = "UTC")

  n <- NA_integer_
  spans <- new_phinterval(
    reference_time = NA_POSIXct_,
    range_starts = list(NA_real_),
    range_ends = list(NA_real_),
    tzone = "UTC"
  )

  expect_identical(
    locate_overlaps(phint, na.rm = TRUE),
    list(n = n, spans = spans)
  )
  expect_identical(
    locate_overlaps(phint, na.rm = FALSE),
    list(n = n, spans = spans)
  )
})

test_that("partial NA inputs are ignored when `na.rm`", {
  origin <- .POSIXct(0L, tz = "UTC")
  int <- lubridate::interval(
    origin + lubridate::dseconds(c(0, 1, 2, 3, NA_real_, 0, NA_real_)),
    origin + lubridate::dseconds(c(7, 6, 5, 4, NA_real_, NA_real_, 10))
  )
  phint <- as_phinterval(int, tzone = "UTC")

  n <- c(1L, 2L, 3L, 4L)
  spans <- new_phinterval(
    reference_time = .POSIXct(rep.int(0, length(n)), tz = "UTC"),
    range_starts = list(
      c(0, 6),
      c(1, 5),
      c(2, 4),
      3
    ),
    range_ends = list(
      c(1, 7),
      c(2, 6),
      c(3, 5),
      4
    )
  )

  expect_identical(
    locate_overlaps(phint, na.rm = TRUE),
    list(n = n, spans = spans)
  )
})

test_that("partial NA inputs cause NA output when not `na.rm`", {
  origin <- .POSIXct(0L, tz = "UTC")
  int <- lubridate::interval(
    origin + lubridate::dseconds(c(0, 1, 2, 3, NA_real_, 0, NA_real_)),
    origin + lubridate::dseconds(c(7, 6, 5, 4, NA_real_, NA_real_, 10))
  )
  phint <- as_phinterval(int, tzone = "UTC")

  n <- NA_integer_
  spans <- new_phinterval(
    reference_time = NA_POSIXct_,
    range_starts = list(NA_real_),
    range_ends = list(NA_real_),
    tzone = "UTC"
  )

  expect_identical(
    locate_overlaps(phint, na.rm = FALSE),
    list(n = n, spans = spans)
  )
})

# extract_overlaps -------------------------------------------------------------

test_that("correct overlaps are extracted with `at = 'least'`", {
  origin <- .POSIXct(0, tz = "UTC")
  int <- lubridate::interval(
    origin + lubridate::dseconds(c(0, 100, 200, 300, 400, 425)),
    origin + lubridate::dseconds(c(50, 250, 250, 500, 450, 430))
  )
  phint <- as_phinterval(int, tzone = "UTC")

  ## `n = 1`
  expect_identical(
    extract_overlaps(phint, n = 1, at = "least"),
    new_phinterval(
      reference_time = origin,
      range_starts = list(c(0, 100, 300)),
      range_ends = list(c(50, 250, 500))
    )
  )

  ## `n = 2`
  expect_identical(
    extract_overlaps(phint, n = 2, at = "least"),
    new_phinterval(
      reference_time = origin,
      range_starts = list(c(200, 400)),
      range_ends = list(c(250, 450))
    )
  )

  ## `n = 3`
  expect_identical(
    extract_overlaps(phint, n = 3, at = "least"),
    new_phinterval(
      reference_time = origin,
      range_starts = list(425),
      range_ends = list(430)
    )
  )
})

test_that("correct overlaps are extracted with `at = 'exactly'`", {
  origin <- .POSIXct(0, tz = "UTC")
  int <- lubridate::interval(
    origin + lubridate::dseconds(c(0, 100, 200, 300, 400, 425)),
    origin + lubridate::dseconds(c(50, 250, 250, 500, 450, 430))
  )
  phint <- as_phinterval(int, tzone = "UTC")
  overlaps <- locate_overlaps(phint)

  ## `n = 1`
  expect_identical(
    extract_overlaps(phint, n = 1, at = "exactly"),
    overlaps$spans[[which(overlaps$n == 1)]]
  )

  ## `n = 2`
  expect_identical(
    extract_overlaps(phint, n = 2, at = "exactly"),
    overlaps$spans[[which(overlaps$n == 2)]]
  )

  ## `n = 3`
  expect_identical(
    extract_overlaps(phint, n = 3, at = "exactly"),
    overlaps$spans[[which(overlaps$n == 3)]]
  )
})

test_that("aligned spans are extracted correctly with `alignment = FALSE`", {
  origin <- .POSIXct(0L, tz = "UTC")
  int <- lubridate::interval(
    origin + lubridate::dseconds(c(0, 50, 50, 100, 500)),
    origin + lubridate::dseconds(c(50, 50, 100, 200, 600))
  )
  phint <- as_phinterval(int, tzone = "UTC")

  ## `n = 1`
  expect_identical(
    extract_overlaps(phint, n = 1, at = "least", alignment = FALSE),
    new_phinterval(
      reference_time = .POSIXct(0, tz = "UTC"),
      range_starts = list(c(0, 500)),
      range_ends = list(c(200, 600)),
      tzone = "UTC"
    )
  )

  ## `n = 2`
  expect_identical(
    extract_overlaps(phint, n = 2, at = "least", alignment = FALSE),
    na_phinterval(tzone = "UTC")
  )
})
