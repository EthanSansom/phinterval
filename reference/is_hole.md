# Test for empty intervals

`is_hole()` checks for `<hole>` (empty) time spans in `phint`.

## Usage

``` r
is_hole(phint)
```

## Arguments

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

## Value

A logical vector the same length as `phint`.

## Examples

``` r
# Detect holes
y2000 <- interval(as.Date("2000-01-01"), as.Date("2001-01-01"))
y2025 <- interval(as.Date("2025-01-01"), as.Date("2025-01-01"))
is_hole(c(hole(), y2000, hole(), y2025, NA))
#> [1]  TRUE FALSE  TRUE FALSE    NA

# The intersection of disjoint intervals is a hole
is_hole(phint_intersect(y2000, y2025))
#> [1] TRUE
```
