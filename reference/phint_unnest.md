# Unnest a phinterval into a data frame

`phint_unnest()` converts a `<phinterval>` vector into a data frame
where each time span becomes a row.

## Usage

``` r
phint_unnest(phint, hole_to = c("drop", "na"), keep_size = FALSE, key = NULL)
```

## Arguments

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector to unnest.

- hole_to:

  `["drop" / "na"]`

  How to handle hole elements (phintervals with zero spans). If `"drop"`
  (the default), holes are excluded from the output. If `"na"`, a row
  with `NA` start and end times is included for each hole.

- keep_size:

  `[TRUE / FALSE]`

  Should a `size` column be included in the output? If `TRUE`, the
  output includes a `size` column containing the number of spans in the
  original phinterval element. If `FALSE` (the default), only `key`,
  `start`, and `end` columns are returned.

- key:

  `[vector / data.frame / NULL]`

  An optional vector or data frame to use as the `key` column in the
  output. If provided, must be the same length as `phint`. If `NULL`
  (the default), the `key` column contains row indices (position in
  `phint`).

  `key` may be any vector in the vctrs sense. See
  `[vctrs::obj_is_vector()]` for details.

## Value

A data frame with columns:

- `key`:

  - If `key = NULL`: A numeric vector identifying the index of the
    phinterval element

  - Otherwise: The element of `key` corresponding to the phinterval
    element

- `start`: POSIXct start time of the span

- `end`: POSIXct end time of the span

- `size`: (if `keep_size = TRUE`) Integer count of spans in the
  phinterval element

## Details

`phint_unnest()` expands each phinterval element into its constituent
time spans, creating one row per span. The resulting data frame contains
a `key` column identifying which phinterval element each span came from,
along with `start` and `end` columns for the span boundaries.

For phinterval elements containing multiple disjoint spans, all spans
are included with the same `key` value. Scalar phinterval elements
(single spans) produce a single row.

## Examples

``` r
# Unnest scalar phintervals
phint <- phinterval(
  start = as.Date(c("2000-01-01", "2000-02-01")),
  end = as.Date(c("2000-01-15", "2000-02-15"))
)
phint_unnest(phint)
#>   key      start        end
#> 1   1 2000-01-01 2000-01-15
#> 2   2 2000-02-01 2000-02-15

# Unnest multi-span phinterval
phint <- phinterval(
  start = as.Date(c("2000-01-01", "2000-03-01")),
  end = as.Date(c("2000-01-15", "2000-03-15")),
  by = 1
)
phint_unnest(phint)
#>   key      start        end
#> 1   1 2000-01-01 2000-01-15
#> 2   1 2000-03-01 2000-03-15

# Handle holes
phint <- c(
  phinterval(as.Date("2000-01-01"), as.Date("2000-01-15")),
  hole(),
  phinterval(as.Date("2000-02-01"), as.Date("2000-02-15"))
)
phint_unnest(phint, hole_to = "drop")
#>   key      start        end
#> 1   1 2000-01-01 2000-01-15
#> 2   3 2000-02-01 2000-02-15
phint_unnest(phint, hole_to = "na")
#>   key      start        end
#> 1   1 2000-01-01 2000-01-15
#> 2   2       <NA>       <NA>
#> 3   3 2000-02-01 2000-02-15

# Include size column
phint_unnest(phint, keep_size = TRUE, hole_to = "na")
#>   key      start        end size
#> 1   1 2000-01-01 2000-01-15    1
#> 2   2       <NA>       <NA>    0
#> 3   3 2000-02-01 2000-02-15    1

# Use a custom `key`
phint_unnest(phint, key = c("A", "B", "C"), hole_to = "na")
#>   key      start        end
#> 1   A 2000-01-01 2000-01-15
#> 2   B       <NA>       <NA>
#> 3   C 2000-02-01 2000-02-15
```
