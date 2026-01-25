# preamble ---------------------------------------------------------------------

# Benchmarks for whether using a dispatch wrapper for `phint_intersect()`
# is noticeably slower.

# setup ------------------------------------------------------------------------

load_all()
set.seed(123)

`%<-%` <- zeallot::`%<-%`

# data -------------------------------------------------------------------------

# Generate spans such that:
# - [x_starts, x_ends] and [y_starts, y_ends] sometimes overlap
# - No spans are reversed or instantaneous (so they work with {ivs} and {lubridate})
sample_spans <- function(n) {
  tibble::lst(
    x_starts = as.numeric(seq(1, n * 5, by = 5)),
    x_ends   = x_starts + 1 + sample(0:3, n, TRUE),
    y_starts = x_ends + 1 - sample(0:4, n, TRUE),
    y_ends   = y_starts + 1 + sample(0:3, n, TRUE)
  ) |> map(as.POSIXct)
}

c(x_starts_100, x_ends_100, y_starts_100, y_ends_100) %<-% sample_spans(100)
c(x_starts_10K, x_ends_10K, y_starts_10K, y_ends_10K) %<-% sample_spans(10 * 1000)

x_intvl_100 <- interval(x_starts_100, x_ends_100)
x_phint_100 <- phinterval(x_starts_100, x_ends_100)
y_intvl_100 <- interval(y_starts_100, y_ends_100)
y_phint_100 <- phinterval(y_starts_100, y_ends_100)

x_intvl_10K <- interval(x_starts_10K, x_ends_10K)
x_phint_10K <- phinterval(x_starts_10K, x_ends_10K)
y_intvl_10K <- interval(y_starts_10K, y_ends_10K)
y_phint_10K <- phinterval(y_starts_10K, y_ends_10K)

# benchmark --------------------------------------------------------------------

phint_intersect_no_dispatch <- function(phint1, phint2) {
  if (is_phinterval(phint1) && is_phinterval(phint2)) {
    out <- phint_phint_intersect_cpp(
      x_size = field(phint1, "size"),
      x_starts = field(phint1, "starts"),
      x_ends = field(phint1, "ends"),
      y_size = field(phint2, "size"),
      y_starts = field(phint2, "starts"),
      y_ends = field(phint2, "ends")
    )
  } else {
    check_phintish(phint1)
    check_phintish(phint2)
  }
  new_phinterval_bare(out, tzone = tz_union(phint1, phint2))
}

# NOTE: Dispatch adds ~10µs vs. the fastest non-dispatch scenario, which I
#       think is worth it for the code cleanliness.
bench::mark(
  no_dispatch_100 = phint_intersect_no_dispatch(x_phint_100, y_phint_100),
  dispatch_100 = phint_intersect(x_phint_100, y_phint_100),
  check = TRUE,
  relative = FALSE
)[1:6]
# expression           min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr>      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# no_dispatch_100   41.8µs     46µs    20212.     3.4KB     2.08
# dispatch_100      52.5µs   57.5µs    15149.     3.4KB     2.08

bench::mark(
  no_dispatch_10K = phint_intersect_no_dispatch(x_phint_10K, y_phint_10K),
  dispatch_10K = phint_intersect(x_phint_10K, y_phint_10K),
  check = TRUE,
  relative = FALSE
)[1:6]
# expression           min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr>      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# no_dispatch_10K    1.1ms   1.28ms      685.     197KB     4.31
# dispatch_10K      1.15ms   1.31ms      710.     197KB     2.08

bench::press(
  n = c(100, 1000, 10000, 100000),
  dispatch = c(TRUE, FALSE),
  {
    x <- rep(x_phint_100, n / 100)
    y <- rep(y_phint_100, n / 100)

    bench::mark(
      if (dispatch) phint_intersect(x, y) else phint_intersect_no_dispatch(x, y)
    )
  }
) |>
  dplyr::group_by(n) |>
  dplyr::summarize(
    median_dispatch = median[dispatch],
    median_nonpatch = median[!dispatch]
  ) |>
  dplyr::mutate(overhead = median_dispatch - median_nonpatch)
#      n median_dispatch median_nonpatch overhead
#  <dbl>        <bch:tm>        <bch:tm> <bch:tm>
#    100         54.73µs         43.66µs  11.07µs
#   1000        151.25µs        140.01µs  11.23µs
#  10000          1.14ms          1.13ms   5.86µs
# 100000         11.15ms         11.13ms  17.22µs
