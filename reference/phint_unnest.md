# Unnest a phinterval into a data frame

`phint_unnest()` converts a `<phinterval>` vector into a
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
where each time span becomes a row.

## Usage

``` r
phint_unnest(phint, key = NULL, hole_to = c("na", "drop"))
```

## Arguments

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector to unnest.

- key:

  `[vector / data.frame / NULL]`

  An optional vector or data frame to use as the `key` column in the
  output. If provided, must be the same length as `phint`. If `NULL`
  (the default), the `key` column contains row indices (position in
  `phint`).

  `key` may be any vector in the vctrs sense. See
  [`vctrs::obj_is_vector()`](https://vctrs.r-lib.org/reference/vector-checks.html)
  for details.

- hole_to:

  `["na" / "drop"]`

  How to handle hole elements (phintervals with zero spans). If `"na"`
  (the default), a row with `NA` start and end times is included for
  each hole. If `"drop"`, holes are excluded from the output.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with columns:

- `key`:

  - If `key = NULL`: A numeric vector identifying the index of the
    phinterval element.

  - Otherwise: The element of `key` corresponding to the phinterval
    element.

- `start`: POSIXct start time of the span.

- `end`: POSIXct end time of the span.

- `size`: Integer count of spans in the phinterval element.

## Details

`phint_unnest()` expands each phinterval element into its constituent
time spans, creating one row per span. The resulting data frame contains
a `key` column identifying which phinterval element each span came from,
a `size` column counting the number of elements, and a `start` and `end`
column for the span boundaries.

For phinterval elements containing multiple disjoint spans, all spans
are included with the same `key` value. Scalar phinterval elements
(single spans) produce a single row. Both `NA` elements and
[`hole()`](https://ethansansom.github.io/phinterval/reference/hole.md)s
produce `NA` values in the `start` and `end` columns, but have a `size`
of `NA` and `0` respectively.

## Examples

``` r
# Unnest scalar phintervals
phint <- phinterval(
  start = as.Date(c("2000-01-01", "2000-02-01")),
  end = as.Date(c("2000-01-15", "2000-02-15"))
)
phint_unnest(phint)
#> # A tibble: 2 × 4
#>     key start               end                  size
#>   <dbl> <dttm>              <dttm>              <int>
#> 1     1 2000-01-01 00:00:00 2000-01-15 00:00:00     1
#> 2     2 2000-02-01 00:00:00 2000-02-15 00:00:00     1

# Unnest multi-span phinterval
phint <- phinterval(
  start = as.Date(c("2000-01-01", "2000-03-01")),
  end = as.Date(c("2000-01-15", "2000-03-15")),
  by = 1
)
phint_unnest(phint)
#> # A tibble: 2 × 4
#>     key start               end                  size
#>   <dbl> <dttm>              <dttm>              <int>
#> 1     1 2000-01-01 00:00:00 2000-01-15 00:00:00     2
#> 2     1 2000-03-01 00:00:00 2000-03-15 00:00:00     2

# Handle holes
phint <- c(
  phinterval(as.Date("2000-01-01"), as.Date("2000-01-15")),
  hole(),
  phinterval(as.Date("2000-02-01"), as.Date("2000-02-15"))
)
phint_unnest(phint, hole_to = "na")
#> # A tibble: 3 × 4
#>     key start               end                  size
#>   <dbl> <dttm>              <dttm>              <int>
#> 1     1 2000-01-01 00:00:00 2000-01-15 00:00:00     1
#> 2     2 NA                  NA                      0
#> 3     3 2000-02-01 00:00:00 2000-02-15 00:00:00     1
phint_unnest(phint, hole_to = "drop")
#> # A tibble: 2 × 4
#>     key start               end                  size
#>   <dbl> <dttm>              <dttm>              <int>
#> 1     1 2000-01-01 00:00:00 2000-01-15 00:00:00     1
#> 2     3 2000-02-01 00:00:00 2000-02-15 00:00:00     1

# Use a custom `key`
phint_unnest(phint, key = c("A", "B", "C"), hole_to = "na")
#> # A tibble: 3 × 4
#>   key   start               end                  size
#>   <chr> <dttm>              <dttm>              <int>
#> 1 A     2000-01-01 00:00:00 2000-01-15 00:00:00     1
#> 2 B     NA                  NA                      0
#> 3 C     2000-02-01 00:00:00 2000-02-15 00:00:00     1
```
