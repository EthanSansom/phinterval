# Squash overlapping intervals into non-overlapping spans

`phint_squash()` and `datetime_squash()` merge overlapping or adjacent
intervals into a single `<phinterval>` element containing a minimal set
of non-overlapping, non-adjacent time spans.

- `phint_squash()` takes a `<phinterval>` or `<Interval>` vector.

- `datetime_squash()` takes separate `start` and `end` datetime vectors.

`phint_squash_by()` and `datetime_squash_by()` merge intervals within
groups defined by the `by` argument. The result is a `<phinterval>`
vector containing one element per unique value of `by`.

## Usage

``` r
phint_squash(phint, na_rm = TRUE, empty_to = c("hole", "na"))

datetime_squash(start, end, na_rm = TRUE, empty_to = c("hole", "na"))

phint_squash_by(
  phint,
  by,
  na_rm = TRUE,
  empty_to = c("hole", "na"),
  order_by = TRUE
)

datetime_squash_by(
  start,
  end,
  by,
  na_rm = TRUE,
  empty_to = c("hole", "na"),
  order_by = TRUE
)
```

## Arguments

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

- na_rm:

  `[TRUE / FALSE]`

  Should `NA` elements be removed before squashing? If `FALSE` and any
  `NA` elements are present, the result is `NA`. Defaults to `TRUE`.

- empty_to:

  `["hole" / "na"]`

  How to handle empty inputs (length-0 vectors):

  - `"hole"` (default): Return a hole.

  - `"na"`: Return an `NA` phinterval.

- start:

  `[POSIXct / POSIXlt / Date]`

  A vector of start times. Must be recyclable with `end`. Only used in
  `datetime_squash()` and `datetime_squash_by()`.

- end:

  `[POSIXct / POSIXlt / Date]`

  A vector of end times. Must be recyclable with `start`. Only used in
  `datetime_squash()` and `datetime_squash_by()`.

- by:

  `[vector / data.frame]`

  A grouping vector or data frame. Intervals are grouped by `by` and
  merged separately within each group, returning one `<phinterval>`
  element per unique value of `by`.

  For `datetime_squash_by()`, `by` must be recyclable with the recycled
  length of `start` and `end`.

  `by` may be any vector in the vctrs sense. See
  [`vctrs::obj_is_vector()`](https://vctrs.r-lib.org/reference/vector-checks.html)
  for details.

- order_by:

  `[TRUE / FALSE]`

  Should the output be ordered by the values in `by`? If `TRUE` (the
  default), the output is sorted by the unique values of `by`. If
  `FALSE`, the output order matches the first appearance of each group
  in `by`. Only used in `phint_squash_by()` and `datetime_squash_by()`.

## Value

`phint_squash()` and `datetime_squash()` return a length-1
`<phinterval>` vector.

`phint_squash_by()` and `datetime_squash_by()` return a
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with columns `by` and `phint`, with one row per unique value of `by`.

## Details

These functions are particularly useful in aggregation workflows with
[`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html)
to combine intervals within groups.

The `phint_squash_by()` and `datetime_squash_by()` variants are designed
to replicate a call to
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
followed by
[`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html),
but are typically faster. In particular, the following produce identical
results:

    phint_squash_by(phint, by = by)

    dplyr::tibble(phint = phint, by = by) |>
      dplyr::group_by(by) |>
      dplyr::summarize(phint = phint_squash(phint)) |>
      dplyr::ungroup()

## See also

[`phint_flatten()`](https://ethansansom.github.io/phinterval/reference/flatten.md)
and
[`datetime_flatten()`](https://ethansansom.github.io/phinterval/reference/flatten.md)
to merge a `<phinterval>` vector into a vector of scalar spans rather
than a single element.

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

# Set na_rm = FALSE to propagate NA values
phint_squash(c(jan_1_to_5, jan_3_to_9, jan_11_to_12, NA), na_rm = FALSE)
#> <phinterval<UTC>[1]>
#> [1] <NA>

# empty_to determines the result of empty inputs
phint_squash(phinterval(), empty_to = "hole")
#> <phinterval<UTC>[1]>
#> [1] <hole>
phint_squash(phinterval(), empty_to = "na")
#> <phinterval<UTC>[1]>
#> [1] <NA>

# phint_squash_by: squash within groups, returning a tibble
phint_squash_by(
  c(jan_1_to_5, jan_3_to_9, jan_11_to_12),
  by = c("A", "A", "B")
)
#> # A tibble: 2 × 2
#>   by    phint                   
#>   <chr> <phint<UTC>>            
#> 1 A     {2000-01-01--2000-01-09}
#> 2 B     {2000-01-11--2000-01-12}

# datetime_squash_by: squash from start/end vectors within groups
datetime_squash_by(
  start = as.Date(c("2000-01-01", "2000-01-03", "2000-01-11")),
  end = as.Date(c("2000-01-05", "2000-01-09", "2000-01-12")),
  by = c("A", "A", "B")
)
#> # A tibble: 2 × 2
#>   by    phint                   
#>   <chr> <phint<UTC>>            
#> 1 A     {2000-01-01--2000-01-09}
#> 2 B     {2000-01-11--2000-01-12}

# Control output order with order_by
phint_squash_by(
  c(jan_1_to_5, jan_3_to_9, jan_11_to_12),
  by = c(2, 2, 1),
  order_by = TRUE
)
#> # A tibble: 2 × 2
#>      by phint                   
#>   <dbl> <phint<UTC>>            
#> 1     1 {2000-01-11--2000-01-12}
#> 2     2 {2000-01-01--2000-01-09}
```
