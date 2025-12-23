# Test whether a date, time, or phinterval is within another phinterval

`phint_within()` tests whether the i-th element of `x` is contained
within the i-th element of `phint`, returning a logical vector. `x` may
be a date or time, while `phint` must be a phinterval. `x` and `phint`
are recycled to their common length using vctrs-style recycling rules.

Dates and times on an endpoint of an interval are considered to be
within the interval. An interval is considered to be within itself.

## Usage

``` r
phint_within(x, phint)
```

## Arguments

- x:

  A `<POSIXct>`, `<POSIXlt>`, `<Date>`, `<Interval>` or `<phinterval>`
  vector to test.

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

## Value

A logical vector.

## Examples

``` r
jan_1_to_5 <- interval(as.Date("2000-01-01"), as.Date("2000-01-05"))
jan_2_to_4 <- interval(as.Date("2000-01-03"), as.Date("2000-01-09"))
jan_3_to_9 <- interval(as.Date("2000-01-03"), as.Date("2000-01-09"))

phint_within(
  c(jan_2_to_4, jan_3_to_9, jan_1_to_5),
  c(jan_1_to_5, jan_1_to_5, NA)
)
#> [1] FALSE FALSE    NA

phint_within(as.Date(c("2000-01-06", "2000-01-20")), jan_3_to_9)
#> [1]  TRUE FALSE

# Intervals are within themselves
phint_within(jan_1_to_5, jan_1_to_5)
#> [1] TRUE

# Interval endpoints are considered to be within the interval
phint_within(as.Date("2000-01-01"), jan_1_to_5)
#> [1] TRUE

# Holes are never considered to be within an interval
hole <- phinterval(interval())
phint_within(c(hole, hole), c(hole, jan_1_to_5))
#> [1] FALSE FALSE
```
