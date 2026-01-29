# Count the number of spans in a phinterval

`n_spans()` counts the number of disjoint time spans in each element of
`phint`.

## Usage

``` r
n_spans(phint)

# Default S3 method
n_spans(phint)

# S3 method for class 'Interval'
n_spans(phint)

# S3 method for class 'phinterval'
n_spans(phint)
```

## Arguments

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

## Value

An integer vector the same length as `phint`.

## Examples

``` r
# Count spans
y2000 <- interval(as.Date("2000-01-01"), as.Date("2001-01-01"))
y2025 <- interval(as.Date("2025-01-01"), as.Date("2025-01-01"))

n_spans(c(
 phint_union(y2000, y2025),
 phint_intersect(y2000, y2025),
 y2000, y2025
))
#> [1] 2 0 1 1
```
