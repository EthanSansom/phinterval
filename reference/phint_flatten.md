# Flatten a phinterval vector into a vector of spans or gaps

`phint_flatten()` collapses all elements of `phint` into a single set of
non-overlapping time spans, then returns them as a flat `<phinterval>`
vector with one span per element. `NA` elements are ignored.

- `what = "spans"` (default): returns the time spans covered by any
  element of `phint`.

- `what = "holes"`: returns the gaps between those spans.

## Usage

``` r
phint_flatten(phint, what = c("spans", "holes"))
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

## Value

A `<phinterval>` vector with the invariant `all(n_spans(phint) == 1L)`.

## Examples

``` r
monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
tuesday <- interval(as.Date("2025-11-11"), as.Date("2025-11-12"))
friday <- interval(as.Date("2025-11-14"), as.Date("2025-11-15"))
thurs_and_sat <- phint_union(
  interval(as.Date("2025-11-13"), as.Date("2025-11-14")),
  interval(as.Date("2025-11-15"), as.Date("2025-11-16"))
)

# Flatten into individual spans
phint_flatten(c(monday, thurs_and_sat))
#> Error in phint_flatten(c(monday, thurs_and_sat)): could not find function "phint_flatten"

# Flatten into gaps between spans
phint_flatten(c(monday, thurs_and_sat), what = "holes")
#> Error in phint_flatten(c(monday, thurs_and_sat), what = "holes"): could not find function "phint_flatten"
phint_flatten(thurs_and_sat, what = "holes") == friday
#> Error in phint_flatten(thurs_and_sat, what = "holes"): could not find function "phint_flatten"

# Overlapping or adjacent elements are merged before flattening
phint_flatten(c(monday, tuesday, friday))
#> Error in phint_flatten(c(monday, tuesday, friday)): could not find function "phint_flatten"

# NA elements are ignored
phint_flatten(c(monday, NA, friday))
#> Error in phint_flatten(c(monday, NA, friday)): could not find function "phint_flatten"
phint_flatten(interval(NA, NA))
#> Error in phint_flatten(interval(NA, NA)): could not find function "phint_flatten"
```
