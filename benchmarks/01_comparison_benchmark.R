# load -------------------------------------------------------------------------

load_all()
library(lubridate)
library(ivs)

library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(bench)

now <- now()
first_run <- !file.exists("benchmarks/results/comparison.rds")

i1_1000 <- readRDS("benchmarks/data/i1_1000.rds")
i2_1000 <- readRDS("benchmarks/data/i2_1000.rds")
i1_25K <- readRDS("benchmarks/data/i1_25K.rds")
i2_25K <- readRDS("benchmarks/data/i2_25K.rds")
i1_1M <- readRDS("benchmarks/data/i1_1M.rds")
i2_1M <- readRDS("benchmarks/data/i2_1M.rds")

to_iv <- function(x) iv(int_start(x), int_end(x))
iv1_1000 <- to_iv(i1_1000)
iv2_1000 <- to_iv(i2_1000)
iv1_25K <- to_iv(i1_25K)
iv2_25K <- to_iv(i2_25K)
iv1_1M <- to_iv(i1_1M)
iv2_1M <- to_iv(i2_1M)

# benchmark --------------------------------------------------------------------

## lubridate and ivs -----------------------------------------------------------

baseline_results <- bench::mark(
  # 1,000
  "intersect: lub (1k)"   = intersect(i1_1000, i2_1000),
  "intersect: ivs (1k)"   = iv_pairwise_set_intersect(iv1_1000, iv2_1000),
  "union: lub (1k)"       = union(i1_1000, i2_1000),
  "union: ivs (1k)"       = iv_pairwise_set_union(iv1_1000, iv2_1000),
  "setdiff: lub (1k)"     = setdiff(i1_1000, i2_1000),
  "overlaps: lub (1k)"    = int_overlaps(i1_1000, i2_1000),
  "overlaps: ivs (1k)"    = iv_overlaps(iv1_1000, iv2_1000),
  "within: lub (1k)"      = i1_1000 %within% i2_1000,

  # 25,000
  "intersect: lub (25k)"  = intersect(i1_25K, i2_25K),
  "intersect: ivs (25k)"  = iv_pairwise_set_intersect(iv1_25K, iv2_25K),
  "union: lub (25k)"      = union(i1_25K, i2_25K),
  "union: ivs (25k)"      = iv_pairwise_set_union(iv1_25K, iv2_25K),
  "setdiff: lub (25k)"    = setdiff(i1_25K, i2_25K),
  "overlaps: lub (25k)"   = int_overlaps(i1_25K, i2_25K),
  "overlaps: ivs (25k)"   = iv_overlaps(iv1_25K, iv2_25K),
  "within: lub (25k)"     = i1_25K %within% i2_25K,

  # 1,000,000
  "intersect: lub (1M)"   = intersect(i1_1M, i2_1M),
  "intersect: ivs (1M)"   = iv_pairwise_set_intersect(iv1_1M, iv2_1M),
  "union: lub (1M)"       = union(i1_1M, i2_1M),
  "union: ivs (1M)"       = iv_pairwise_set_union(iv1_1M, iv2_1M),
  "setdiff: lub (1M)"     = setdiff(i1_1M, i2_1M),
  "overlaps: lub (1M)"    = int_overlaps(i1_1M, i2_1M),
  "overlaps: ivs (1M)"    = iv_overlaps(iv1_1M, iv2_1M),
  "within: lub (1M)"      = i1_1M %within% i2_1M,

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

## phinterval ------------------------------------------------------------------

benchmark_exprs <- list(
  # 1,000
  "intersect: ph (1k)"    = quote(phint_intersect(i1_1000, i2_1000)),
  "union: ph (1k)"        = quote(phint_union(i1_1000, i2_1000)),
  "setdiff: ph (1k)"      = quote(phint_setdiff(i1_1000, i2_1000)),
  "overlaps: ph (1k)"     = quote(phint_overlaps(i1_1000, i2_1000)),
  "within: ph (1k)"       = quote(phint_within(i1_1000, i2_1000)),

  # 25,000
  "intersect: ph (25k)"   = quote(phint_intersect(i1_25K, i2_25K)),
  "union: ph (25k)"       = quote(phint_union(i1_25K, i2_25K)),
  "setdiff: ph (25k)"     = quote(phint_setdiff(i1_25K, i2_25K)),
  "overlaps: ph (25k)"    = quote(phint_overlaps(i1_25K, i2_25K)),
  "within: ph (25k)"      = quote(phint_within(i1_25K, i2_25K)),

  # 1,000,000
  "intersect: ph (1M)"    = quote(phint_intersect(i1_1M, i2_1M)),
  "union: ph (1M)"        = quote(phint_union(i1_1M, i2_1M)),
  "setdiff: ph (1M)"      = quote(phint_setdiff(i1_1M, i2_1M)),
  "overlaps: ph (1M)"     = quote(phint_overlaps(i1_1M, i2_1M)),
  "within: ph (1M)"       = quote(phint_within(i1_1M, i2_1M))
)

results <- imap(benchmark_exprs, function(expr, name) {
  tryCatch(
    # Success
    {
      result <- bench::mark(
        exprs = setNames(list(expr), name),
        check = FALSE,
        min_iterations = 5
      )
      result |>
        mutate(
          expression = as.character(expression),
          error = FALSE,
          error_msg = NA_character_,
          timestamp = now
        )
    },
    # Error
    error = function(e) {
      bench::mark(NA) |>
        mutate(
          expression = name,
          timestamp = now,
          error = TRUE,
          error_msg = e$message,
          across(where(is.numeric), ~ .x[NA])
        )
    }
  )
}) |>
  bind_rows() |>
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

baseline_clean <- baseline_results |>
  clean_bench() |>
  select(operation, size, pkg, median) |>
  pivot_wider(
    id_cols = c(operation, size),
    names_from = pkg,
    values_from = median,
    names_glue = "{.value}_{pkg}"
  )

prior_clean <- prior_results |>
  filter(str_detect(expression, fixed(" ph "))) |>
  slice_max(n = 1L, order_by = timestamp, by = expression, with_ties = FALSE) |>
  clean_bench() |>
  select(operation, size, median_pre = median)

results_clean <- results |>
  clean_bench() |>
  left_join(baseline_clean, by = c("operation", "size")) |>
  left_join(prior_clean, by = c("operation", "size")) |>
  mutate(
    across(
      c(median_ivs, median_lub, median_pre),
      ~ round(as.numeric(median) / as.numeric(.x), 2),
      .names = "{str_replace(.col, 'median_', 'ratio ')}"
    )
  ) |>
  select(
    expression,
    median,
    starts_with("ratio"),
    min,
    starts_with("median_"),
    !starts_with("error"),
    error,
    error_msg
  ) |>
  select(-c(pkg, size, operation))

# save -------------------------------------------------------------------------

if (first_run) {
  saveRDS(results_clean, "benchmarks/results/comparison.rds")
} else {
  saveRDS(bind_rows(results_clean, prior_results), "benchmarks/results/comparison.rds")
}
