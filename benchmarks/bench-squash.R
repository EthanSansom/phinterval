# preamble ---------------------------------------------------------------------

# Benchmarks for phint_squash() vs ivs::iv_groups()

# setup ------------------------------------------------------------------------

load_all()
library(ivs)
set.seed(123)

`%<-%` <- zeallot::`%<-%`

# data -------------------------------------------------------------------------

# Generate overlapping spans for squashing
# - Some intervals overlap (will be merged)
# - Some are disjoint (will remain separate)
sample_overlapping_spans <- function(n) {
  starts <- as.POSIXct(seq(10, n * 10, by = 10))
  ends <- starts + sample(1:15, n, TRUE)

  list(starts = starts, ends = ends)
}

# Small data (100 spans)
c(x_starts_100, x_ends_100) %<-% sample_overlapping_spans(100)
by_n100_g20 <- sample(seq(20), 100, TRUE)

# Medium data (10K spans)
c(x_starts_10K, x_ends_10K) %<-% sample_overlapping_spans(10 * 1000)
by_n10K_g2K <- sample(seq(2000), 10 * 1000, TRUE)

# Large data (1M spans)
c(x_starts_1M, x_ends_1M) %<-% sample_overlapping_spans(1000 * 1000)
by_n1M_g200K <- sample(seq(200 * 1000), 1000 * 1000, TRUE)

# Create interval objects
phint_100 <- phinterval(x_starts_100, x_ends_100)
phint_10K <- phinterval(x_starts_10K, x_ends_10K)
phint_1M <- phinterval(x_starts_1M, x_ends_1M)

# ivs intervals
iv_100 <- iv(x_starts_100, x_ends_100)
iv_10K <- iv(x_starts_10K, x_ends_10K)
iv_1M <- iv(x_starts_1M, x_ends_1M)

# Grouped data
phint_g20 <- phinterval(x_starts_100, x_ends_100, by = by_n100_g20)
phint_g2K <- phinterval(x_starts_10K, x_ends_10K, by = by_n10K_g2K)
phint_g200K <- phinterval(x_starts_1M, x_ends_1M, by = by_n1M_g200K)

compare_squash_groups <- function(phint, iv) {
  stopifnot(length(phint) == 1L)
  phint_res <- list(
    starts = phint_starts(phint)[[1]],
    ends = phint_ends(phint)[[1]]
  )
  iv_res <- list(
    starts = iv_start(iv),
    ends = iv_end(iv)
  )
  waldo::compare(phint_res, iv_res)
}

# benchmarks -------------------------------------------------------------------

compare_squash_groups(
  phint = phint_squash(phint_100),
  iv = iv_groups(iv_100)
)

bench::mark(
  phint_squash = phint_squash(phint_100),
  dtime_squash = datetime_squash(x_starts_100, x_ends_100),
  iv_groups = iv_groups(iv_100),
  check = FALSE,
  relative = FALSE
)[1:6]

bench::mark(
  phint_squash = phint_squash(phint_10K),
  iv_groups = iv_groups(iv_10K),
  check = FALSE,
  relative = FALSE
)[1:6]

bench::mark(
  phint_squash = phint_squash(phint_1M),
  iv_groups = iv_groups(iv_1M),
  check = FALSE,
  relative = FALSE,
  iterations = 10
)[1:6]

local({
  by_2 <- seq(1000*1000) %% 2
  by_4 <- seq(1000*1000) %% 4
  bench::mark(
    phint_squash = phint_squash(phint_1M),
    phint_squash_by_2 = phint_squash(phint_1M, by = by_2),
    phint_squash_by_4 = phint_squash(phint_1M, by = by_4),
    check = FALSE,
    relative = FALSE,
    iterations = 10
  )[1:6]
})
# expression             min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr>        <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# phint_squash        59.2ms   61.3ms     14.0     39.7MB     9.35
# phint_squash_by_2  167.2ms  167.4ms      5.97    34.7MB     2.56
# phint_squash_by_4    223ms  224.3ms      4.44    34.7MB     2.96
#
# - Reasonable speed decrease with `by`

local({
  by_2 <- seq(1000*1000) %% 2
  by_4 <- seq(1000*1000) %% 4
  bench::mark(
    phint_squash = phint_squash(phint_1M),
    phint_squash_by_2 = phint_squash(phint_1M, by = by_2, keep_by = TRUE),
    phint_squash_by_4 = phint_squash(phint_1M, by = by_4, keep_by = TRUE),
    check = FALSE,
    relative = FALSE,
    iterations = 10
  )[1:6]
})

bench::mark(
  phint_squash_20 = phint_squash(phint_g20),
  phint_squash_2K = phint_squash(phint_g2K),
  phint_squash_200K = phint_squash(phint_g200K),
  check = FALSE,
  relative = FALSE
)[1:6]

# grouped vs dplyr workflow ----------------------------------------------------

library(dplyr)

df_100 <- tibble(phint = phint_100, by = by_n100_g20)
df_10K <- tibble(phint = phint_10K, by = by_n10K_g2K)
df_1M  <- tibble(phint = phint_1M, by = by_n1M_g200K)

# `group_by(...)` automatically arranges by `...`
all.equal(
  phint_squash(phint_10K, by = by_n10K_g2K, order_by = TRUE),
  df_10K |> group_by(by) |> summarize(phint = phint_squash(phint)) |> pull(phint)
)

bench::mark(
  phint_by_100 = tibble(phint = phint_squash(phint_100, by = by_n100_g20, order_by = TRUE)),
  dplyr_100 = df_100 |> group_by(by) |> summarize(phint = phint_squash(phint)),
  phint_by_10K = tibble(phint = phint_squash(phint_10K, by = by_n10K_g2K, order_by = TRUE)),
  dplyr_10K = df_10K |> group_by(by) |> summarize(phint = phint_squash(phint)),
  phint_by_1M = tibble(phint = phint_squash(phint_1M, by = by_n1M_g200K, order_by = TRUE)),
  # dplyr_1M = df_1M |> group_by(by) |> summarize(phint = phint_squash(phint)),
  check = FALSE,
  relative = FALSE
)[1:6]

# scaling ----------------------------------------------------------------------

sizes <- c(100, 500, 1000, 5000, 10000, 100*1000, 1000*1000, 2*1000*1000)

scaling_results <- bench::press(
  n = sizes,
  {
    c(starts, ends) %<-% sample_overlapping_spans(n)
    phint <- phinterval(starts, ends)
    iv <- iv(starts, ends)

    bench::mark(
      phint = phint_squash(phint),
      ivs = iv_groups(iv),
      check = FALSE,
      relative = TRUE,
      min_iterations = 10
    )
  }
)

library(ggplot2)
ggplot2::ggplot(scaling_results, aes(x = n, y = median, color = as.character(expression))) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = seq(1, 5, by = 0.5)) +
  labs(
    title = "Squash Performance Scaling",
    x = "Number of Intervals",
    y = "Median Time (relative)",
    color = "Method"
  )
