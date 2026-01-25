# preamble ---------------------------------------------------------------------

# Benchmarks for intersect, union, setdifference.

# setup ------------------------------------------------------------------------

load_all()
set.seed(123)

`%<-%` <- zeallot::`%<-%`

# data -------------------------------------------------------------------------

compare_phint_intvl <- function(phint, intvl) {
  phint[is_hole(phint)] <- NA
  waldo::compare(phint, as_phinterval(intvl))
}

# Generate spans such that:
# - [x_starts, x_ends] and [y_starts, y_ends] sometimes overlap
# - No spans are reversed or instantaneous (so they work with {ivs} and {lubridate})
sample_spans <- function(n) {
  tibble::lst(
    x_starts = as.numeric(seq(1, n * 5, by = 5)),
    x_ends   = x_starts + 1 + sample(0:3, n, TRUE),
    y_starts = x_ends + 1 - sample(0:4, n, TRUE),
    y_ends   = y_starts + 1 + sample(0:3, n, TRUE)
  ) |> purrr::map(as.POSIXct)
}

c(x_starts_100, x_ends_100, y_starts_100, y_ends_100) %<-% sample_spans(100)
c(x_starts_10K, x_ends_10K, y_starts_10K, y_ends_10K) %<-% sample_spans(10 * 1000)
c(x_starts_1M, x_ends_1M, y_starts_1M, y_ends_1M)     %<-% sample_spans(1000 * 1000)

by_n100_g20  <- sample(seq(20), 100, TRUE)
by_n10K_g2K  <- sample(seq(2000), 10*1000, TRUE)
by_n1M_g200K <- sample(seq(200*1000), 1000*1000, TRUE)

# Interval
x_intvl_100 <- interval(x_starts_100, x_ends_100)
y_intvl_100 <- interval(y_starts_100, y_ends_100)
x_intvl_10K <- interval(x_starts_10K, x_ends_10K)
y_intvl_10K <- interval(y_starts_10K, y_ends_10K)
x_intvl_1M <- interval(x_starts_1M, x_ends_1M)
y_intvl_1M <- interval(y_starts_1M, y_ends_1M)

# Phinterval (span)
x_phint_100 <- phinterval(x_starts_100, x_ends_100)
y_phint_100 <- phinterval(y_starts_100, y_ends_100)
x_phint_10K <- phinterval(x_starts_10K, x_ends_10K)
y_phint_10K <- phinterval(y_starts_10K, y_ends_10K)
x_phint_1M <- phinterval(x_starts_1M, x_ends_1M)
y_phint_1M <- phinterval(y_starts_1M, y_ends_1M)

# Phinterval (set)
x_phint_g20  <- phinterval(x_starts_100, x_ends_100, by = by_n100_g20)
y_phint_g20  <- phinterval(y_starts_100, y_ends_100, by = by_n100_g20)
x_phint_g2K  <- phinterval(x_starts_10K, x_ends_10K, by = by_n10K_g2K)
y_phint_g2K  <- phinterval(y_starts_10K, y_ends_10K, by = by_n10K_g2K)
x_phint_g200K <- phinterval(x_starts_1M, x_ends_1M, by = by_n1M_g200K)
y_phint_g200K <- phinterval(y_starts_1M, y_ends_1M, by = by_n1M_g200K)

# benchmarks -------------------------------------------------------------------

## intersect -------------------------------------------------------------------

compare_phint_intvl(
  phint = phint_intersect(x_intvl_100, y_intvl_100),
  intvl = lubridate::intersect(x_intvl_100, y_intvl_100)
)

# Intersect 100
bench::mark(
  intvl_x_intvl_lub = lubridate::intersect(x_intvl_100, y_intvl_100),
  intvl_x_intvl_pht = phint_intersect(x_intvl_100, y_intvl_100),
  phint_x_phint_spn = phint_intersect(x_phint_100, y_phint_100),
  phint_x_phint_set = phint_intersect(x_phint_g20, y_phint_g20),
  check = FALSE,
  relative = FALSE
)[1:6]

# Intersect 10K
bench::mark(
  intvl_x_intvl_lub = lubridate::intersect(x_intvl_10K, y_intvl_10K),
  intvl_x_intvl_pht = phint_intersect(x_intvl_10K, y_intvl_10K),
  phint_x_phint_spn = phint_intersect(x_phint_10K, y_phint_10K),
  phint_x_phint_set = phint_intersect(x_phint_g2K, y_phint_g2K),
  check = FALSE,
  relative = FALSE
)[1:6]

# Intersect 1M
bench::mark(
  intvl_x_intvl_lub = lubridate::intersect(x_intvl_1M, y_intvl_1M),
  intvl_x_intvl_pht = phint_intersect(x_intvl_1M, y_intvl_1M),
  phint_x_phint_spn = phint_intersect(x_phint_1M, y_phint_1M),
  phint_x_phint_set = phint_intersect(x_phint_g200K, y_phint_g200K),
  check = FALSE,
  relative = FALSE,
  iterations = 10
)[1:6]

# Recycling
local({
  x_phint_1 <- x_phint_10K[[1]]
  x_phint_g1 <- x_phint_g2K[[1]]

  bench::mark(
    span = phint_intersect(x_phint_10K, y_phint_10K),
    span_rec = phint_intersect(x_phint_1, y_phint_10K),
    set = phint_intersect(x_phint_g2K, y_phint_g2K),
    set_rec = phint_intersect(x_phint_g1, y_phint_g2K),
    check = FALSE,
    relative = FALSE
  )[1:6]
})

## union -----------------------------------------------------------------------

local({
  overlaps <- phint_overlaps(x_intvl_100, y_intvl_100)
  compare_phint_intvl(
    phint = phint_union(x_intvl_100[overlaps], y_intvl_100[overlaps]),
    intvl = lubridate::union(x_intvl_100[overlaps], y_intvl_100[overlaps])
  )
})

# Union 100
bench::mark(
  intvl_x_intvl_lub = lubridate::union(x_intvl_100, y_intvl_100),
  intvl_x_intvl_pht = phint_union(x_intvl_100, y_intvl_100),
  phint_x_phint_spn = phint_union(x_phint_100, y_phint_100),
  phint_x_phint_set = phint_union(x_phint_g20, y_phint_g20),
  check = FALSE,
  relative = FALSE
)[1:6]

# Union 10K
bench::mark(
  intvl_x_intvl_lub = lubridate::union(x_intvl_10K, y_intvl_10K),
  intvl_x_intvl_pht = phint_union(x_intvl_10K, y_intvl_10K),
  phint_x_phint_spn = phint_union(x_phint_10K, y_phint_10K),
  phint_x_phint_set = phint_union(x_phint_g2K, y_phint_g2K),
  check = FALSE,
  relative = FALSE
)[1:6]

# Union 1M
bench::mark(
  intvl_x_intvl_lub = lubridate::union(x_intvl_1M, y_intvl_1M),
  intvl_x_intvl_pht = phint_union(x_intvl_1M, y_intvl_1M),
  phint_x_phint_spn = phint_union(x_phint_1M, y_phint_1M),
  phint_x_phint_set = phint_union(x_phint_g200K, y_phint_g200K),
  check = FALSE,
  relative = FALSE,
  iterations = 10
)[1:6]

# Recycling
local({
  x_phint_1 <- x_phint_10K[[1]]
  x_phint_g1 <- x_phint_g2K[[1]]
  bench::mark(
    span = phint_union(x_phint_10K, y_phint_10K),
    span_rec = phint_union(x_phint_1, y_phint_10K),
    set = phint_union(x_phint_g2K, y_phint_g2K),
    set_rec = phint_union(x_phint_g1, y_phint_g2K),
    check = FALSE,
    relative = FALSE
  )[1:6]
})
# - Recycling is slightly slower, but also very dependent on which element was chosen

## setdiff ---------------------------------------------------------------------

local({
  # NOTE: There are a bunch of 1 second differences!
  not_within <- !phint_within(y_intvl_100, x_intvl_100) # If y in x, holes are punched
  non_equal <- x_intvl_100 == y_intvl_100               # Generates a hole
  good <- not_within & non_equal
  compare_phint_intvl(
    phint = phint_setdiff(x_intvl_100[good], y_intvl_100[good]),
    intvl = lubridate::setdiff(x_intvl_100[good], y_intvl_100[good])
  )

  # NOTE: lubridate is off by 1 second for set-differences
  x <- interval(as.POSIXct(0), as.POSIXct(5))
  y <- interval(as.POSIXct(6), as.POSIXct(10))

  lubridate::int_overlaps(x, y) # FALSE
  lubridate::setdiff(x, y) == x # FALSE
  lubridate::setdiff(x, y)      # [0, 6], this is [x_start, y_start]

  phint_overlaps(x, y)     # FALSE
  phint_setdiff(x, y) == x # TRUE
  phint_setdiff(x, y)      # [0, 5], this is [x_start, x_end]

  # This doesn't happen if the gap is > 1 second
  x <- interval(as.POSIXct(0), as.POSIXct(5))
  y <- interval(as.POSIXct(7), as.POSIXct(10))

  lubridate::int_overlaps(x, y) # FALSE
  lubridate::setdiff(x, y) == x # FALSE
  lubridate::setdiff(x, y)      # [0, 5], this is [x_start, x_end]
})

compare_phint_intvl(
  phint = phint_setdiff(x_intvl_100, y_intvl_100),
  intvl = lubridate::setdiff(x_intvl_100, y_intvl_100)
)

# Setdiff 100
bench::mark(
  # intvl_x_intvl_lub = lubridate::setdiff(x_intvl_100, y_intvl_100),
  intvl_x_intvl_pht = phint_setdiff(x_intvl_100, y_intvl_100),
  phint_x_phint_spn = phint_setdiff(x_phint_100, y_phint_100),
  phint_x_phint_set = phint_setdiff(x_phint_g20, y_phint_g20),
  check = FALSE,
  relative = FALSE
)[1:6]

# Setdiff 10K
bench::mark(
  # intvl_x_intvl_lub = lubridate::setdiff(x_intvl_10K, y_intvl_10K),
  intvl_x_intvl_pht = phint_setdiff(x_intvl_10K, y_intvl_10K),
  phint_x_phint_spn = phint_setdiff(x_phint_10K, y_phint_10K),
  phint_x_phint_set = phint_setdiff(x_phint_g2K, y_phint_g2K),
  check = FALSE,
  relative = FALSE
)[1:6]

# Setdiff 1M
bench::mark(
  # intvl_x_intvl_lub = lubridate::setdiff(x_intvl_1M, y_intvl_1M),
  intvl_x_intvl_pht = phint_setdiff(x_intvl_1M, y_intvl_1M),
  phint_x_phint_spn = phint_setdiff(x_phint_1M, y_phint_1M),
  phint_x_phint_set = phint_setdiff(x_phint_g200K, y_phint_g200K),
  check = FALSE,
  relative = FALSE,
  iterations = 10
)[1:6]

# Recycling
local({
  x_phint_1 <- x_phint_10K[[1]]
  x_phint_g1 <- x_phint_g2K[[1]]
  bench::mark(
    span = phint_setdiff(x_phint_10K, y_phint_10K),
    span_rec = phint_setdiff(x_phint_1, y_phint_10K),
    set = phint_setdiff(x_phint_g2K, y_phint_g2K),
    set_rec = phint_setdiff(x_phint_g1, y_phint_g2K),
    check = FALSE,
    relative = FALSE
  )[1:6]
})

## setdiff (hole punch) --------------------------------------------------------

x_wide_10K <- phinterval(x_starts_10K, x_ends_10K + 10)
y_middle_10K <- phinterval(x_starts_10K + 2, x_ends_10K + 3)
x_wide_g2K <- x_wide_10K |> phint_squash(by = by_n10K_g2K)
y_middle_g2K <- y_middle_10K |> phint_squash(by = by_n10K_g2K)

bench::mark(
  punch_holes_10K = phint_setdiff(x_wide_10K, y_middle_10K),
  punch_holes_g2K = phint_setdiff(x_wide_g2K, y_middle_g2K),
  check = FALSE,
  relative = FALSE
)[1:6]
