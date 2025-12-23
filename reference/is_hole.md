# Test for empty intervals

`is_hole()` checks for `<hole>` (i.e. empty) time spans in `phint`.

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
y2000 <- interval(as.Date("2000-01-01"), as.Date("2001-01-01"))
y2025 <- interval(as.Date("2025-01-01"), as.Date("2025-01-01"))

# The intersection of disjoint intervals is a hole
is_hole(c(
 phint_intersect(y2000, y2025),
 y2000, y2025
))
#> [1]  TRUE FALSE FALSE
```
