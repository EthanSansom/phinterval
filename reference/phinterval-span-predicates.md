# Test for empty, contiguous, or disjoint intervals

These predicates test structural properties of each element of a
`<phinterval>` vector.

- `is_hole()`: Is the element empty (zero spans)?

- `is_span()`: Is the element a single contiguous span?

- `is_disjoint()`: Is the element made up of two or more disjoint spans?

## Usage

``` r
is_hole(phint)

is_span(phint)

is_disjoint(phint)
```

## Arguments

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

## Value

A logical vector the same length as `phint`.

## Under Development

`is_span()` and `is_disjoint()` are under development and may contain
bugs. To use these functions, install the development version of
phinterval from GitHub with `pak::pak("EthanSansom/phinterval")`.

## Examples

``` r
y2000 <- interval(as.Date("2000-01-01"), as.Date("2001-01-01"))
y2025 <- interval(as.Date("2025-01-01"), as.Date("2026-01-01"))
holey <- phint_union(y2000, y2025)

# Detect holes
is_hole(c(hole(), y2000, NA))
#> [1]  TRUE FALSE    NA

# The intersection of disjoint intervals is a hole
is_hole(phint_intersect(y2000, y2025))
#> [1] TRUE

# Detect single contiguous spans
is_span(c(y2000, holey, hole(), NA))
#> Error in FUN(X[[i]], ...): as.interval is not defined for class 'phinterval'as.interval is not defined for class 'vctrs_rcrd'as.interval is not defined for class 'vctrs_vctr'

# Detect disjoint (multi-span) elements
is_disjoint(c(y2000, holey, hole(), NA))
#> Error in FUN(X[[i]], ...): as.interval is not defined for class 'phinterval'as.interval is not defined for class 'vctrs_rcrd'as.interval is not defined for class 'vctrs_vctr'
```
