# preamble ---------------------------------------------------------------------

# Benchmarks for intersection and squashing.

# setup ------------------------------------------------------------------------

load_all()
set.seed(123)

`%<-%` <- zeallot::`%<-%`

# data -------------------------------------------------------------------------

## endpoints -------------------------------------------------------------------

# Generate spans such that:
# - [x_starts, x_ends] and [y_starts, y_ends] sometimes overlap
# - No spans are reversed or instantaneous (so they work with {ivs} and {lubridate})
sample_spans <- function(n) {
  tibble::lst(
    x_starts = as.numeric(seq(1, n * 5, by = 5)),
    x_ends   = x_starts + 1 + sample(0:3, n, TRUE),
    y_starts = x_ends + 1 - sample(0:4, n, TRUE),
    y_ends   = y_starts + 1 + sample(0:3, n, TRUE)
  )
}

c(x_starts_100, x_ends_100, y_starts_100, y_ends_100) %<-% sample_spans(100)
c(x_starts_10K, x_ends_10K, y_starts_10K, y_ends_10K) %<-% sample_spans(10 * 1000)
c(x_starts_1M, x_ends_1M, y_starts_1M, y_ends_1M)     %<-% sample_spans(1000 * 1000)

## intervals -------------------------------------------------------------------

new_interval <- function(starts, ends, class) {
  if (is.list(starts)) {
    sizes <- lengths(starts)
    sizes[vapply(starts, is.null, TRUE)] <- NA_integer_
  } else {
    sizes <- rep(1L, length(starts))
  }
  switch(
    class,
    interval_spans = new_interval_spans(starts, ends),
    interval_sets = new_interval_sets(sizes, as.list(starts), as.list(ends)),
    iv = ivs::iv(starts, ends),
    Interval = lubridate::interval(as.Date(starts), as.Date(ends))
  )
}

# interval_sets
x_interval_sets_100 <- new_interval(x_starts_100, x_ends_100, "interval_sets")
y_interval_sets_100 <- new_interval(y_starts_100, y_ends_100, "interval_sets")
x_interval_sets_10K <- new_interval(x_starts_10K, x_ends_10K, "interval_sets")
y_interval_sets_10K <- new_interval(y_starts_10K, y_ends_10K, "interval_sets")
x_interval_sets_1M  <- new_interval(x_starts_1M, x_ends_1M, "interval_sets")
y_interval_sets_1M  <- new_interval(y_starts_1M, y_ends_1M, "interval_sets")

# interval_spans
x_interval_spans_100 <- new_interval(x_starts_100, x_ends_100, "interval_spans")
y_interval_spans_100 <- new_interval(y_starts_100, y_ends_100, "interval_spans")
x_interval_spans_10K <- new_interval(x_starts_10K, x_ends_10K, "interval_spans")
y_interval_spans_10K <- new_interval(y_starts_10K, y_ends_10K, "interval_spans")
x_interval_spans_1M  <- new_interval(x_starts_1M, x_ends_1M, "interval_spans")
y_interval_spans_1M  <- new_interval(y_starts_1M, y_ends_1M, "interval_spans")

# ivs
x_ivs_100 <- new_interval(x_starts_100, x_ends_100, "iv")
y_ivs_100 <- new_interval(y_starts_100, y_ends_100, "iv")
x_ivs_10K <- new_interval(x_starts_10K, x_ends_10K, "iv")
y_ivs_10K <- new_interval(y_starts_10K, y_ends_10K, "iv")
x_ivs_1M  <- new_interval(x_starts_1M, x_ends_1M, "iv")
y_ivs_1M  <- new_interval(y_starts_1M, y_ends_1M, "iv")

# lubridate
x_lubridate_100 <- new_interval(x_starts_100, x_ends_100, "Interval")
y_lubridate_100 <- new_interval(y_starts_100, y_ends_100, "Interval")
x_lubridate_10K <- new_interval(x_starts_10K, x_ends_10K, "Interval")
y_lubridate_10K <- new_interval(y_starts_10K, y_ends_10K, "Interval")
x_lubridate_1M  <- new_interval(x_starts_1M, x_ends_1M, "Interval")
y_lubridate_1M  <- new_interval(y_starts_1M, y_ends_1M, "Interval")

# benchmark --------------------------------------------------------------------

## intersection ----------------------------------------------------------------

# equivalence test
waldo::compare(
  interval_sets_intersect(x_interval_sets_100, y_interval_sets_100),
  interval_sets_intersect(x_interval_spans_100, y_interval_spans_100)
)

# intersect 100
bench::mark(
  intersect_set = interval_sets_intersect(x_interval_sets_100, y_interval_sets_100),
  intersect_spn = interval_sets_intersect(x_interval_spans_100, y_interval_spans_100),
  intersect_lub = lubridate::intersect(x_lubridate_100, y_lubridate_100),
  check = FALSE,
  relative = FALSE
)[1:6]
# A tibble: 3 × 6
# expression         min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr>    <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# intersect_set  45.76µs  62.28µs    13488.     3.4KB     0
# intersect_spn  42.64µs  46.58µs    17837.     3.4KB     2.15
# intersect_lub   1.39ms   1.44ms      670.    42.8KB     2.04

# intersect 10K
bench::mark(
  intersect_set = interval_sets_intersect(x_interval_sets_10K, y_interval_sets_10K),
  intersect_spn = interval_sets_intersect(x_interval_spans_10K, y_interval_spans_10K),
  intersect_lub = lubridate::intersect(x_lubridate_10K, y_lubridate_10K),
  check = FALSE,
  relative = FALSE
)[1:6]
# A tibble: 3 × 6
# expression         min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr>    <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# intersect_set 708.73µs  806.8µs     1219.  196.76KB     2.04
# intersect_spn 585.03µs 673.22µs     1270.  196.76KB     2.01
# intersect_lub   2.02ms   2.32ms      426.    3.05MB     6.59

# intersect 1M
bench::mark(
  intersect_set = interval_sets_intersect(x_interval_sets_1M, y_interval_sets_1M),
  intersect_spn = interval_sets_intersect(x_interval_spans_1M, y_interval_spans_1M),
  intersect_lub = lubridate::intersect(x_lubridate_1M, y_lubridate_1M),
  check = FALSE,
  relative = FALSE
)[1:6]
# expression         min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr>    <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# intersect_set   72.6ms   76.6ms     11.9     19.1MB     3.96
# intersect_spn   57.1ms   60.4ms     11.0     19.1MB     3.66
# intersect_lub  198.2ms  237.4ms      4.26   303.4MB     8.52

## squash ----------------------------------------------------------------------

### equivilance checks ---------------------------------------------------------

squash_and_compare <- function(x_interval_sets, x_iv, by = NULL) {
  if (is.null(by)) {
    squash_set <- interval_sets_squash(x_interval_sets)
    squash_ivs <- ivs::iv_groups(x_ivs_100)

    out_interval_sets <- list(
      starts = field(squash_set, "starts")[[1]],
      ends = field(squash_set, "ends")[[1]]
    )
    out_iv <- list(
      starts = ivs::iv_start(squash_ivs),
      ends = ivs::iv_end(squash_ivs)
    )
  } else {
    squash_by_set <- interval_sets_squash(x_interval_sets_100, by = by)
    squash_by_ivs <- vctrs::vec_split(x_ivs_100, by = by)[["val"]] |> purrr::map(ivs::iv_groups)

    out_interval_sets <- list(
      starts = field(squash_by_set, "starts"),
      ends = field(squash_by_set, "ends")
    )
    out_iv <- list(
      starts = squash_by_ivs |> map(ivs::iv_start),
      ends = squash_by_ivs |> map(ivs::iv_end)
    )
  }

  waldo::compare(out_interval_sets, out_iv)
}

# interval_sets vs interval_spans
waldo::compare(
  interval_sets_squash(x_interval_sets_100),
  interval_sets_squash(x_interval_spans_100)
)

# interval_sets vs ivs
squash_and_compare(x_interval_sets_100, x_ivs_100)

# interval_sets vs ivs (with `by`)
local({
  by <- sample(seq(20), 100, TRUE)
  squash_and_compare(x_interval_sets_100, x_ivs_100, by = by)
})

### benchmarks -----------------------------------------------------------------

# squash 100
bench::mark(
  squash_set = interval_sets_squash(x_interval_sets_100),
  squash_spn = interval_sets_squash(x_interval_spans_100),
  squash_ivs = ivs::iv_groups(x_ivs_100),
  check = FALSE,
  relative = FALSE
)[1:6]
# expression      min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# squash_set   34.7µs   36.7µs    25632.    6.27KB     2.56
# squash_spn   70.2µs   74.7µs    12734.   12.32KB     4.21
# squash_ivs   11.6µs   12.8µs    74720.    4.38KB     7.47

# squash by 100
local({
  by <- sample(seq(20), 100, TRUE)
  x_ivs_tibble_100 <- tibble::tibble(x = x_ivs_100, by = by)

  bench::mark(
    squash_by_set = interval_sets_squash(x_interval_sets_100, by = by),
    squash_by_ivs = vctrs::vec_split(x_ivs_100, by = by)[["val"]] |> purrr::map(ivs::iv_groups),
    squash_by_tib = x_ivs_tibble_100 |> dplyr::group_by(by) |> dplyr::reframe(x = ivs::iv_groups(x)),
    check = FALSE,
    relative = FALSE
  )[1:6]
})
# expression         min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr>    <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# squash_by_set   59.4µs  68.43µs    14115.    4.06KB     2.05
# squash_by_ivs    289µs 305.59µs     2957.   13.01KB     4.14
# squash_by_tib    1.9ms   2.02ms      478.   19.61KB     4.14

# squash 10K
bench::mark(
  squash_set = interval_sets_squash(x_interval_sets_10K),
  squash_spn = interval_sets_squash(x_interval_spans_10K),
  squash_ivs = ivs::iv_groups(x_ivs_10K),
  check = FALSE,
  relative = FALSE
)[1:6]
# expression      min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# squash_set  609.3µs    671µs     1438.     470KB     6.51
# squash_spn  880.9µs    989µs      924.     902KB     8.85
# squash_ivs   91.6µs    113µs     8832.     352KB    26.8

# squash by 10K
local({
  by <- sample(seq(2000), 10*1000, TRUE)
  x_ivs_tibble_10K <- tibble::tibble(x = x_ivs_10K, by = by)

  bench::mark(
    squash_by_set = interval_sets_squash(x_interval_sets_10K, by = by),
    squash_by_ivs = vctrs::vec_split(x_ivs_10K, by = by)[["val"]] |> purrr::map(ivs::iv_groups),
    squash_by_tib = x_ivs_tibble_10K |> dplyr::group_by(by) |> dplyr::reframe(x = ivs::iv_groups(x)),
    check = FALSE,
    relative = FALSE
  )[1:6]
})
# expression         min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr>    <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# squash_by_set   2.12ms   2.21ms     444.   308.94KB     2.04
# squash_by_ivs  28.41ms  28.94ms      34.5    1.28MB     5.74
# squash_by_tib  88.79ms  92.13ms      10.4    1.45MB     2.61
