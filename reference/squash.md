# Squash overlapping intervals into non-overlapping spans

`phint_squash()` and `datetime_squash()` merge overlapping or adjacent
intervals into a minimal set of non-overlapping, non-adjacent time
spans.

- `phint_squash()` takes a `<phinterval>` or `<Interval>` vector

- `datetime_squash()` takes separate `start` and `end` datetime vectors

When `by = NULL` (the default), all intervals are merged into a single
phinterval element. When `by` is provided, intervals are grouped and
merged separately within each group, creating one phinterval element per
unique value of `by`.

## Usage

``` r
phint_squash(
  phint,
  by = NULL,
  na.rm = TRUE,
  empty_to = c("hole", "na", "empty"),
  order_by = FALSE,
  keep_by = FALSE
)

datetime_squash(
  start,
  end,
  by = NULL,
  na.rm = TRUE,
  empty_to = c("hole", "na", "empty"),
  order_by = FALSE,
  keep_by = FALSE
)
```

## Arguments

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

- by:

  `[vector / data.frame / NULL]`

  An optional grouping vector or data frame. When provided, intervals
  are grouped by `by` and merged separately within each group. If `NULL`
  (the default), all intervals are merged into a single phinterval
  element.

  For `datetime_squash()`, `by` must be recyclable with the recycled
  length of `start` and `end`.

  `by` may be any vector in the vctrs sense. See
  `[vctrs::obj_is_vector()]` for details.

- na.rm:

  `[TRUE / FALSE]]`

  Should `NA` elements be removed before squashing? If `FALSE` and any
  `NA` elements are present, the result for that group is `NA`. Defaults
  to `TRUE`.

- empty_to:

  `["hole" / "na" / "empty"]`

  How to handle empty inputs (length-0 vectors or groups with only `NA`
  values when `na.rm = TRUE`):

  - `"hole"` (default): Return a hole

  - `"na"`: Return an `NA` phinterval

  - `"empty"`: Return a length-0 phinterval vector

- order_by:

  `[TRUE / FALSE]`

  Should the output be ordered by the values in `by`? If `FALSE` (the
  default), the output order matches the first appearance of each group
  in `by`. If `TRUE`, the output is sorted by the unique values of `by`.
  Only used when `by` is not `NULL`.

- keep_by:

  `[TRUE / FALSE]`

  Should the `by` values be returned alongside the result? If `FALSE`
  (the default), returns a `<phinterval>` vector. If `TRUE`, returns a
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  with columns `by` and `phint`. Requires `by` to be non-`NULL`.

- start:

  `[POSIXct / POSIXlt / Date]`

  A vector of start times. Must be recyclable with `end`. Only used in
  `datetime_squash()`.

- end:

  `[POSIXct / POSIXlt / Date]`

  A vector of end times. Must be recyclable with `start`. Only used in
  `datetime_squash()`.

## Value

When `keep_by = FALSE`:

- If `by = NULL`: A length-1 `<phinterval>` vector (or length-0 if the
  input is empty and `empty_to = "empty"`)

- If `by` is provided: A `<phinterval>` vector with one element per
  unique value of `by`

When `keep_by = TRUE`: A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with columns `by` and `phint`.

## Details

These functions are particularly useful in aggregation workflows with
[`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html)
to combine intervals within groups.

## Examples

``` r
jan_1_to_5 <- interval(as.Date("2000-01-01"), as.Date("2000-01-05"))
jan_3_to_9 <- interval(as.Date("2000-01-03"), as.Date("2000-01-09"))
jan_11_to_12 <- interval(as.Date("2000-01-11"), as.Date("2000-01-12"))

# phint_squash: merge intervals from a phinterval/Interval vector
phint_squash(c(jan_1_to_5, jan_3_to_9, jan_11_to_12))
#> <phinterval<UTC>[1]>
#> [1] {2000-01-01--2000-01-09, 2000-01-11--2000-01-12}

# datetime_squash: merge intervals from start/end vectors
datetime_squash(
  start = as.Date(c("2000-01-01", "2000-01-03", "2000-01-11")),
  end = as.Date(c("2000-01-05", "2000-01-09", "2000-01-12"))
)
#> <phinterval<UTC>[1]>
#> [1] {2000-01-01--2000-01-09, 2000-01-11--2000-01-12}

# NA values are removed by default
phint_squash(c(jan_1_to_5, jan_3_to_9, jan_11_to_12, NA))
#> <phinterval<UTC>[1]>
#> [1] {2000-01-01--2000-01-09, 2000-01-11--2000-01-12}

# Set na.rm = FALSE to propagate NA values
phint_squash(c(jan_1_to_5, jan_3_to_9, jan_11_to_12, NA), na.rm = FALSE)
#> <phinterval<UTC>[1]>
#> [1] <NA>

# Squash within groups
phint_squash(
  c(jan_1_to_5, jan_3_to_9, jan_11_to_12),
  by = c(1, 1, 2)
)
#> <phinterval<UTC>[2]>
#> [1] {2000-01-01--2000-01-09} {2000-01-11--2000-01-12}

# Return a data frame with by values
phint_squash(
  c(jan_1_to_5, jan_3_to_9, jan_11_to_12),
  by = c("A", "A", "B"),
  keep_by = TRUE
)
#> # A tibble: 2 × 2
#>   by    phint                   
#>   <chr> <phint<UTC>>            
#> 1 A     {2000-01-01--2000-01-09}
#> 2 B     {2000-01-11--2000-01-12}

# Control output order with order_by
phint_squash(
  c(jan_1_to_5, jan_3_to_9, jan_11_to_12),
  by = c(2, 2, 1),
  order_by = TRUE
)
#> <phinterval<UTC>[2]>
#> [1] {2000-01-11--2000-01-12} {2000-01-01--2000-01-09}

# empty_to determines the result of empty inputs
empty <- phinterval()
phint_squash(empty, empty_to = "hole")
#> <phinterval<UTC>[1]>
#> [1] <hole>
phint_squash(empty, empty_to = "na")
#> <phinterval<UTC>[1]>
#> [1] <NA>
phint_squash(empty, empty_to = "empty")
#> <phinterval<UTC>[0]>
```
