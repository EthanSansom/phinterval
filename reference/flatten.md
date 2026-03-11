# Flatten a phinterval vector into a vector of spans or gaps

`phint_flatten()` and `datetime_flatten()` collapse all elements into a
single set of non-overlapping time spans, then return them as a flat
`<phinterval>` vector with one span per element. `NA` elements are
ignored.

- `phint_flatten()` takes a `<phinterval>` or `<Interval>` vector.

- `datetime_flatten()` takes separate `start` and `end` datetime
  vectors.

- `what = "spans"` (default): returns the time spans covered by any
  element of `phint`.

- `what = "holes"`: returns the gaps between those spans.

## Usage

``` r
phint_flatten(phint, what = c("spans", "holes"))

datetime_flatten(start, end, what = c("spans", "holes"))
```

## Arguments

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

- what:

  `["spans" / "holes"]`

  Whether to return the covered spans or the intervening gaps:

  - `"spans"` (default): Time spans covered by at least one element of
    `phint`.

  - `"holes"`: Gaps between covered spans (excludes the infinite extents
    before the first span and after the last span).

- start:

  `[POSIXct / POSIXlt / Date]`

  A vector of start times. Must be recyclable with `end`. Only used in
  `datetime_flatten()`.

- end:

  `[POSIXct / POSIXlt / Date]`

  A vector of end times. Must be recyclable with `start`. Only used in
  `datetime_flatten()`.

## Value

A `<phinterval>` vector with the invariant `all(n_spans(result) == 1L)`.

## See also

[`phint_squash()`](https://ethansansom.github.io/phinterval/reference/squash.md)
and
[`datetime_squash()`](https://ethansansom.github.io/phinterval/reference/squash.md)
to collapse into a single `<phinterval>` element rather than a vector of
scalar spans.

## Examples

``` r
monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
tuesday <- interval(as.Date("2025-11-11"), as.Date("2025-11-12"))
friday <- interval(as.Date("2025-11-14"), as.Date("2025-11-15"))
thurs_and_sat <- phint_union(
  interval(as.Date("2025-11-13"), as.Date("2025-11-14")),
  interval(as.Date("2025-11-15"), as.Date("2025-11-16"))
)
noon_wednesday <- as_phinterval(as.POSIXct("2025-11-12 12:00:00"))

# phint_flatten: flatten a phinterval/Interval vector
phint_flatten(c(monday, thurs_and_sat))
#> Error in FUN(X[[i]], ...): as.interval is not defined for class 'phinterval'as.interval is not defined for class 'vctrs_rcrd'as.interval is not defined for class 'vctrs_vctr'

# datetime_flatten: flatten from start/end vectors
datetime_flatten(
  start = as.Date(c("2025-11-10", "2025-11-13", "2025-11-15")),
  end = as.Date(c("2025-11-11", "2025-11-14", "2025-11-16"))
)
#> <phinterval<local>[3]>
#> [1] {1970-01-01 05:40:02--1970-01-01 05:40:03}
#> [2] {1970-01-01 05:40:05--1970-01-01 05:40:06}
#> [3] {1970-01-01 05:40:07--1970-01-01 05:40:08}

# Flatten into gaps between spans
phint_flatten(c(monday, thurs_and_sat), what = "holes")
#> Error in FUN(X[[i]], ...): as.interval is not defined for class 'phinterval'as.interval is not defined for class 'vctrs_rcrd'as.interval is not defined for class 'vctrs_vctr'
phint_flatten(thurs_and_sat, what = "holes") == friday
#> [1] TRUE

# Overlapping or adjacent elements are merged before flattening
phint_flatten(c(monday, tuesday, friday))
#> <phinterval<UTC>[2]>
#> [1] {2025-11-10--2025-11-12} {2025-11-14--2025-11-15}

# NA elements are ignored
phint_flatten(c(monday, NA, friday))
#> <phinterval<UTC>[2]>
#> [1] {2025-11-10--2025-11-11} {2025-11-14--2025-11-15}
phint_flatten(interval(NA, NA))
#> <phinterval<UTC>[0]>

# Instants are preserved when flattening into spans
phint_flatten(c(monday, noon_wednesday, friday), what = "spans")
#> Error in FUN(X[[i]], ...): as.interval is not defined for class 'phinterval'as.interval is not defined for class 'vctrs_rcrd'as.interval is not defined for class 'vctrs_vctr'

# Instants between two spans are ignored when flattening into gaps
phint_flatten(c(monday, noon_wednesday, friday), what = "holes")
#> Error in FUN(X[[i]], ...): as.interval is not defined for class 'phinterval'as.interval is not defined for class 'vctrs_rcrd'as.interval is not defined for class 'vctrs_vctr'
```
