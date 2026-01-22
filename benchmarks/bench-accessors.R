# preamble ---------------------------------------------------------------------

# Benchmarks for accessing starts, ends, lengths.

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

c(x_starts_100, x_ends_100) %<-% sample_spans(100)
c(x_starts_10K, x_ends_10K) %<-% sample_spans(10 * 1000)
c(x_starts_1M, x_ends_1M)   %<-% sample_spans(1000 * 1000)

by_n100_g20  <- sample(seq(20), 100, TRUE)
by_n10K_g2K  <- sample(seq(2000), 10*1000, TRUE)
by_n1M_g200K <- sample(seq(200*1000), 1000*1000, TRUE)

intvl_100 <- interval(x_starts_100, x_ends_100)
phint_100 <- phinterval(x_starts_100, x_ends_100)
phint_g20 <- phinterval(x_starts_100, x_ends_100, by = by_n100_g20)

intvl_10K <- interval(x_starts_10K, x_ends_10K)
phint_10K <- phinterval(x_starts_10K, x_ends_10K)
phint_g2K <- phinterval(x_starts_10K, x_ends_10K, by = by_n10K_g2K)

intvl_1M    <- interval(x_starts_1M, x_ends_1M)
phint_1M    <- phinterval(x_starts_1M, x_ends_1M)
phint_g200K <- phinterval(x_starts_1M, x_ends_1M, by = by_n1M_g200K)

# benchmarks -------------------------------------------------------------------

## start -----------------------------------------------------------------------

intvl_start_simple <- function(x) lubridate::int_start(lubridate::int_standardize(x))
phint_start_simple <- function(x) {
  starts <- field(x, "starts")
  starts[map_lgl(starts, is.null)] <- list(NA_real_)
  lubridate::with_tz(map_dbl(starts, `[`, 1) + lubridate::origin, get_tzone(x))
}

# Start 100
bench::mark(
  intvl_start_lub = lubridate::int_start(intvl_100),
  intvl_start_pht = phint_start(intvl_100),
  intvl_start_smp = intvl_start_simple(intvl_100), # NOTE: Much slower
  phint_start_smp = phint_start_simple(phint_100), # NOTE: ~9x slower, C++ worth it
  phint_start_spn = phint_start(phint_100),
  phint_start_set = phint_start(phint_g20),
  check = FALSE,
  relative = FALSE
)[1:6]
# expression           min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr>      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# intvl_start_lub 245.87ns 328.29ns  2488438.        0B     0
# intvl_start_pht   7.87µs   9.76µs    97790.    4.98KB     9.78
# intvl_start_smp  367.2µs 387.57µs     2476.    5.84KB     6.18
# phint_start_smp  47.03µs  50.84µs    19000.   26.38KB     6.20
# phint_start_spn   5.45µs   5.95µs   164942.      848B     0
# phint_start_set    4.8µs   5.17µs   190012.      208B    19.0
#
# - Can't get close to `int_start(int) { int@start }`, as it doesn't standardize

# Start 10K
bench::mark(
  intvl_start_lub = lubridate::int_start(intvl_10K),
  intvl_start_pht = phint_start(intvl_10K),
  phint_start_spn = phint_start(phint_10K),
  phint_start_set = phint_start(phint_g2K),
  check = FALSE,
  relative = FALSE
)[1:6]
# expression           min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr>      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# intvl_start_lub  245.9ns  368.8ns  2430656.        0B     0
# intvl_start_pht   97.7µs  107.7µs     8974.   156.3KB    19.0
# phint_start_spn   78.6µs   84.1µs    11327.    78.2KB    10.7
# phint_start_set   19.6µs   20.2µs    48262.    15.5KB     9.65

# Start 1M
bench::mark(
  intvl_start_lub = lubridate::int_start(intvl_1M),
  intvl_start_pht = phint_start(intvl_1M),
  phint_start_spn = phint_start(phint_1M),
  phint_start_set = phint_start(phint_g200K),
  check = FALSE,
  relative = FALSE
)[1:6]
# expression           min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr>      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# intvl_start_lub 122.94ns 246.34ns 2769013.         0B     0
# intvl_start_pht   9.55ms  10.43ms      95.1   22.89MB    28.0
# phint_start_spn  10.38ms  10.97ms      85.7    7.63MB     9.02
# phint_start_set   3.01ms   3.48ms     287.     1.52MB     6.25

## starts ----------------------------------------------------------------------

intvl_starts_simple <- function(x) as.list(lubridate::int_start(lubridate::int_standardize(x)))

# Starts 100
bench::mark(
  intvl_starts_pht = phint_starts(intvl_100),
  intvl_starts_smp = intvl_starts_simple(intvl_100), # NOTE: Much slower
  phint_starts_spn = phint_starts(phint_100),
  phint_starts_set = phint_starts(phint_g20),
  check = FALSE,
  relative = FALSE
)[1:6]
# expression            min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr>       <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# intvl_starts_pht   75.2µs   80.3µs    11606.    4.14KB     6.16
# intvl_starts_smp  439.4µs  463.2µs     1937.    6.66KB     6.16
# phint_starts_spn   34.6µs   36.5µs    26747.      848B     5.35
# phint_starts_set   10.9µs   11.7µs    83117.      208B     8.31

# Starts 10K
bench::mark(
  intvl_starts_pht = phint_starts(intvl_10K),
  phint_starts_spn = phint_starts(phint_10K),
  phint_starts_set = phint_starts(phint_g2K),
  check = FALSE,
  relative = FALSE
)[1:6]

# Starts 1M
bench::mark(
  intvl_starts_pht = phint_starts(intvl_1M),
  phint_starts_spn = phint_starts(phint_1M),
  phint_starts_set = phint_starts(phint_g200K),
  check = FALSE,
  relative = FALSE
)[1:6]
# expression            min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr>       <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# intvl_starts_pht    930ms  929.8ms      1.08   31.11MB     3.23
# phint_starts_spn    656ms  656.3ms      1.52    7.63MB     1.52
# phint_starts_set     69ms   85.2ms     11.6     1.52MB     1.66
#
# - Access slows at 1M, nearing the second mark at 656.3ms

## Set Access ------------------------------------------------------------------

bench::mark(
  start_100 = phint_start(phint_100),
  starts_100 = phint_starts(phint_100),
  start_10K = phint_start(phint_10K),
  starts_10K = phint_starts(phint_10K),
  start_1M = phint_start(phint_1M),
  starts_1M = phint_starts(phint_1M),
  check = FALSE,
  relative = FALSE
)[1:6]
# expression      min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# start_100    5.54µs   6.03µs 21608.         848B     2.16
# starts_100  34.81µs  39.11µs 21804.         848B     2.18
# start_10K   78.72µs  83.72µs 11011.      78.17KB     8.00
# starts_10K   3.06ms   3.22ms   277.      78.17KB     1.99
# start_1M    10.18ms  10.67ms    91.4      7.63MB     5.96
# starts_1M  381.92ms       1s     0.995    7.63MB     1.49

## length ----------------------------------------------------------------------

bench::mark(
  length_100 = phint_length(phint_100),
  lengths_100 = phint_lengths(phint_100),
  length_10K = phint_length(phint_10K),
  lengths_10K = phint_lengths(phint_10K),
  length_1M = phint_length(phint_1M),
  lengths_1M = phint_lengths(phint_1M),
  check = FALSE,
  relative = FALSE
)[1:6]
# expression       min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr>  <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# length_100    4.59µs   5.37µs 114662.        848B     0
# lengths_100   14.1µs  15.99µs  59155.        848B     0
# length_10K  141.66µs 151.33µs   6476.     78.17KB     2.24
# lengths_10K   1.08ms   1.16ms    812.     78.17KB     2.06
# length_1M    16.28ms   16.6ms     59.7     7.63MB     2.06
# lengths_1M  112.72ms 114.75ms      8.54    7.63MB     2.14
#
# - `phint_length()` is fast enough, good because lengths is less useful

## unnest ----------------------------------------------------------------------

# Unnest vs. Starts
bench::mark(
  unnest_100 = phint_unnest(phint_100),
  starts_100 = phint_starts(phint_100),
  unnest_g20 = phint_unnest(phint_g20),
  starts_g20 = phint_starts(phint_g20),

  unnest_10K = phint_unnest(phint_10K),
  starts_10K = phint_starts(phint_10K),
  unnest_g20K = phint_unnest(phint_g2K),
  starts_g20K = phint_starts(phint_g2K),

  unnest_1M = phint_unnest(phint_1M),
  starts_1M = phint_starts(phint_1M),
  unnest_g200K = phint_unnest(phint_g200K),
  starts_g200K = phint_starts(phint_g200K),

  check = FALSE,
  relative = FALSE
)[1:6]
# expression        min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# unnest_100     87.6µs  95.37µs   9746.      2.48KB     4.57
# starts_100     37.4µs  39.69µs  24168.        848B     4.83
# unnest_g20     85.1µs  88.44µs  10416.      2.48KB     4.25
# starts_g20     11.4µs  12.01µs  78495.        208B     7.85
# unnest_10K    394.5µs 428.82µs   2268.    234.52KB     6.63
# starts_10K      3.3ms   3.37ms    292.     78.17KB     5.09
# unnest_g20K   191.5µs 213.98µs   4515.    234.52KB     6.55
# starts_g20K   690.3µs 714.18µs   1372.     15.73KB     2.05
# unnest_1M      37.1ms   37.7ms     26.4    22.89MB     4.81
# starts_1M     346.9ms 346.93ms      2.88    7.63MB     2.88
# unnest_g200K   12.2ms  13.17ms     75.7    22.89MB    26.7
# starts_g200K   71.2ms  72.92ms     13.7     1.52MB     2.28
#
# - Unnest starts to beat starts as we avoid creating many list elements

# Unnest Argument Combos
local({
  phint_100[sample(seq(100), 20)] <- new_na_phinterval()
  phint_10K[sample(seq(10*1000), 2000)] <- new_na_phinterval()

  phint_100[sample(seq(100), 20)] <- new_hole()
  phint_10K[sample(seq(10*1000), 2000)] <- new_hole()

  bench::mark(
    unnest_na_false_100 = phint_unnest(phint_100, hole_to = "na", keep_size = FALSE),
    unnest_drop_false_100 = phint_unnest(phint_100, hole_to = "drop", keep_size = FALSE),
    unnest_na_true_100 = phint_unnest(phint_100, hole_to = "na", keep_size = TRUE),
    unnest_na_false_100 = phint_unnest(phint_100, hole_to = "na", keep_size = FALSE),

    unnest_na_false_10K = phint_unnest(phint_10K, hole_to = "na", keep_size = FALSE),
    unnest_drop_false_10K = phint_unnest(phint_10K, hole_to = "drop", keep_size = FALSE),
    unnest_na_true_10K = phint_unnest(phint_10K, hole_to = "na", keep_size = TRUE),
    unnest_na_false_10K = phint_unnest(phint_10K, hole_to = "na", keep_size = FALSE),

    check = FALSE,
    relative = FALSE
  )[1:6]
})
# - Not a ton of speed difference between the arguments

# Unnest Interval
intvl_unnest_simple <- function(int) {
  int <- lubridate::int_standardize(int)
  data.frame(
    key = seq_along(int),
    start = lubridate::int_start(int),
    end = lubridate::int_end(int)
  )
}

bench::mark(
  unnest_100_int = phint_unnest(intvl_100),
  unnest_100_smp = intvl_unnest_simple(intvl_100),
  unnest_10K_int = phint_unnest(intvl_10K),
  unnest_10K_smp = intvl_unnest_simple(intvl_10K),
  unnest_1M_int = phint_unnest(intvl_1M),
  unnest_1M_smp = intvl_unnest_simple(intvl_1M),
  check = FALSE,
  relative = FALSE
)[1:6]
# expression          min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr>     <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# unnest_100_int  92.25µs 107.01µs    8463.     4.14KB     2.06
# unnest_100_smp 481.75µs 546.28µs    1547.     6.66KB     0
# unnest_10K_int 280.36µs 332.84µs    2805.   312.69KB    13.1
# unnest_10K_smp 535.46µs 569.41µs    1735.   449.12KB     4.17
# unnest_1M_int   19.87ms  21.02ms      47.6   30.52MB     9.53
# unnest_1M_smp    7.29ms   7.88ms     127.    41.96MB    39.1
#
# - Simple approach is ~3x slower
