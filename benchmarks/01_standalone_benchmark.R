# TODO:
# - Add error checking, as in `01_comparison_benchmark.R`
# - Convert repeated operations into functions
#   - adorn_benchmark() -> add the now column, bench_safely() -> bench with tryCatch

# load -------------------------------------------------------------------------

load_all()
library(dplyr)
library(stringr)
library(tidyr)

now <- Sys.time()
first_run <- !file.exists("benchmarks/results/standalone.rds")

p1_1000 <- readRDS("benchmarks/data/p1_1000.rds")
p2_1000 <- readRDS("benchmarks/data/p2_1000.rds")
p1_10K <- readRDS("benchmarks/data/p1_10K.rds")
p2_10K <- readRDS("benchmarks/data/p2_10K.rds")

groups_5K <- readRDS("benchmarks/data/groups_5K.rds")
groups_50K <- readRDS("benchmarks/data/groups_50K.rds")

# benchmark --------------------------------------------------------------------

results <- bench::mark(
  "intersect (1k)" = phint_intersect(p1_1000, p2_1000),
  "union (1k)"     = phint_union(p1_1000, p2_1000),
  "setdiff (1k)"   = phint_setdiff(p1_1000, p2_1000),
  "overlaps (1k)"  = phint_overlaps(p1_1000, p2_1000),
  "within (1k)"    = phint_within(p1_1000, p2_1000),
  "starts (1k)"    = phint_starts(p1_1000),
  "ends (1k)"      = phint_ends(p1_1000),

  "intersect (10k)" = phint_intersect(p1_10K, p2_10K),
  "union (10k)"     = phint_union(p1_10K, p2_10K),
  "setdiff (10k)"   = phint_setdiff(p1_10K, p2_10K),
  "overlaps (10k)"  = phint_overlaps(p1_10K, p2_10K),
  "within (10k)"    = phint_within(p1_10K, p2_10K),
  "starts (10k)"    = phint_starts(p1_10K),
  "ends (10k)"      = phint_ends(p1_10K),

  "squash(5K)"      = groups_5K |>
    mutate(span = interval(start, end)) |>
    summarise(span = phint_squash(span), .by = group),
  "squash (50K)"    = groups_50K |>
    mutate(span = interval(start, end)) |>
    summarise(span = phint_squash(span), .by = group),

  check = FALSE,
  min_iterations = 5
) |>
  mutate(
    expression = as.character(expression),
    error = FALSE,
    error_msg = NA_character_,
    timestamp = now
  ) |>
  select(
    expression, min, median, `itr/sec`, mem_alloc,
    `gc/sec`, timestamp, error, error_msg
  )

# compare ----------------------------------------------------------------------

prior_results <- if (first_run) results else readRDS("benchmarks/results/comparison.rds")

clean_bench <- function(df) {
  df |>
    mutate(
      operation = str_extract(expression, "^[a-z]+"),          # e.g. "intersect"
      pkg       = str_extract(expression, "(?<=: )[a-z]+"),    # "ph", "lub", "ivs"
      size      = str_extract(expression, "(?<=\\().+(?=\\))") # "1k", "25k"
    )
}

prior_clean <- prior_results |>
  slice_max(n = 1L, order_by = timestamp, by = expression, with_ties = FALSE) |>
  clean_bench() |>
  select(operation, size, pkg, median_pre = median)

results_clean <- results |>
  clean_bench() |>
  left_join(prior_clean, by = c("operation", "size")) |>
  mutate(
    across(
      c(median_pre),
      ~ round(as.numeric(median) / as.numeric(.x), 2),
      .names = "{str_replace(.col, 'median_', 'ratio ')}"
    )
  ) |>
  select(
    expression,
    median,
    starts_with("ratio"),
    min,
    everything(),
    error,
    error_msg
  )

# save -------------------------------------------------------------------------

if (first_run) {
  saveRDS(results_clean, "benchmarks/results/standalone.rds")
} else {
  saveRDS(bind_rows(results_clean, prior_results), "benchmarks/results/standalone.rds")
}
