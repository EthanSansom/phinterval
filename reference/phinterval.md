# Create a new phinterval

`phinterval()` creates a new `<phinterval>` vector from start and end
times. A phinterval (think "potentially holey interval") is a span of
time which may contain gaps.

## Usage

``` r
phinterval(
  start = POSIXct(),
  end = POSIXct(),
  tzone = NULL,
  by = NULL,
  order_by = FALSE
)
```

## Arguments

- start:

  `[POSIXct / POSIXlt / Date]`

  A vector of start times. Must be recyclable with `end`.

- end:

  `[POSIXct / POSIXlt / Date]`

  A vector of end times. Must be recyclable with `start`.

- tzone:

  `[character(1)]`

  A time zone to display the `<phinterval>` in. If `tzone` is `NULL`
  (the default), then the time zone is taken from that of `start`.

  `tzone` can be any non-`NA` string, but unrecognized time zones (see
  [`is_recognized_tzone()`](https://ethansansom.github.io/phinterval/reference/is_recognized_tzone.md))
  will be formatted using `"UTC"` with a warning.

- by:

  `[vector / data.frame / NULL]`

  An optional grouping vector or data frame. When provided, `start[i]`
  and `end[i]` pairs are grouped by `by[i]`, creating one phinterval
  element per unique value of `by`. Overlapping or abutting spans within
  each group are merged. If `NULL` (the default), each `start`/`end`
  pair creates a separate phinterval element. `by` is recycled to match
  the common length of `start` and `end`.

  `by` must be a vector in the vctrs sense. See
  `[vctrs::obj_is_vector()]` for details.

- order_by:

  `[TRUE / FALSE]`

  Should the output be ordered by the values in `by`? If `FALSE` (the
  default), the output order matches the first appearance of each group
  in `by`. If `TRUE`, the output is sorted by the unique values of `by`.
  Only used when `by` is not `NULL`.

## Value

When `by = NULL`, a `<phinterval>` vector the same length as the
recycled length of `start` and `end`.

When `by` is provided, a `<phinterval>` vector with one element per
unique value of `by`.

## Details

The `<phinterval>` class is designed as a generalization of the
[`lubridate::interval()`](https://lubridate.tidyverse.org/reference/interval.html).
While an `<Interval>` element represents a single contiguous span
between two fixed times, a `<phinterval>` element can represent a time
span that may be empty, contiguous, or disjoint (i.e. containing gaps).
Each element of a `<phinterval>` is stored as a (possibly empty) set of
non-overlapping and non-abutting time spans.

When `by = NULL` (the default), `phinterval()` creates scalar phinterval
elements, where each element contains a single time span from `start[i]`
to `end[i]`. This is equivilant to
[`lubridate::interval()`](https://lubridate.tidyverse.org/reference/interval.html):

    interval(start, end, tzone = tzone)   # <Interval> vector
    phinterval(start, end, tzone = tzone) # <phinterval> vector

When `by` is provided, `phinterval()` groups the `start`/`end` pairs by
the values in `by`, creating phinterval elements that may contain
multiple disjoint time spans. Overlapping or abutting spans within each
group are automatically merged.

## Examples

``` r
# Scalar phintervals (equivalent to interval())
phinterval(
  start = as.Date(c("2000-01-01", "2000-02-01")),
  end = as.Date(c("2000-02-01", "2000-03-01"))
)
#> <phinterval<UTC>[2]>
#> [1] {2000-01-01--2000-02-01} {2000-02-01--2000-03-01}

# Grouped phintervals with multiple spans per element
phinterval(
  start = as.Date(c("2000-01-01", "2000-03-01", "2000-02-01")),
  end = as.Date(c("2000-02-01", "2000-04-01", "2000-03-01")),
  by = c(1, 1, 2)
)
#> <phinterval<UTC>[2]>
#> [1] {2000-01-01--2000-02-01, 2000-03-01--2000-04-01}
#> [2] {2000-02-01--2000-03-01}                        

# Overlapping spans are merged within groups
phinterval(
  start = as.Date(c("2000-01-01", "2000-01-15")),
  end = as.Date(c("2000-02-01", "2000-02-15")),
  by = 1
)
#> <phinterval<UTC>[1]>
#> [1] {2000-01-01--2000-02-15}

# Empty phinterval
phinterval()
#> <phinterval<UTC>[0]>

# Specify time zone
phinterval(
  start = as.Date("2000-01-01"),
  end = as.Date("2000-02-01"),
  tzone = "America/New_York"
)
#> <phinterval<America/New_York>[1]>
#> [1] {1999-12-31 19:00:00--2000-01-31 19:00:00}
```
