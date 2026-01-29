# Test whether a datetime or phinterval is within another phinterval

`phint_within()` tests whether the i-th element of `x` is contained
within the i-th element of `phint`, returning a logical vector. `x` may
be a datetime (Date, POSIXct, POSIXlt),
[`lubridate::interval()`](https://lubridate.tidyverse.org/reference/interval.html),
or
[`phinterval()`](https://ethansansom.github.io/phinterval/reference/phinterval.md),
while `phint` must be a
[`lubridate::interval()`](https://lubridate.tidyverse.org/reference/interval.html)
or
[`phinterval()`](https://ethansansom.github.io/phinterval/reference/phinterval.md).
`x` and `phint` are recycled to their common length using vctrs-style
recycling rules.

Datetimes on an endpoint of an interval are considered to be within the
interval. An interval is considered to be within itself.

## Usage

``` r
phint_within(x, phint, bounds = c("[]", "()"))
```

## Arguments

- x:

  `[phinterval / Interval / Date / POSIXct / POSIXlt]`

  The object to test.

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

- bounds:

  `["[]" / "()"]`

  Whether span endpoints are inclusive or exclusive:

  - `"[]"` (default): Closed intervals - both endpoints are included

  - `"()"`: Open intervals - both endpoints are excluded

  This affects adjacency and overlap detection. For example, with
  `bounds = "[]"`, the intervals `[1, 5]` and `[5, 10]` are considered
  adjacent (they share the endpoint 5), while with `bounds = "()"`,
  `(1, 5)` and `(5, 10)` are disjoint (neither includes 5).

## Value

A logical vector.

## Examples

``` r
jan_1_to_5 <- interval(as.Date("2000-01-01"), as.Date("2000-01-05"))
jan_2_to_4 <- interval(as.Date("2000-01-02"), as.Date("2000-01-04"))
jan_3_to_9 <- interval(as.Date("2000-01-03"), as.Date("2000-01-09"))

phint_within(
  c(jan_2_to_4, jan_3_to_9, jan_1_to_5),
  c(jan_1_to_5, jan_1_to_5, NA)
)
#> [1]  TRUE FALSE    NA

phint_within(as.Date(c("2000-01-06", "2000-01-20")), jan_3_to_9)
#> [1]  TRUE FALSE

# Intervals are within themselves
phint_within(jan_1_to_5, jan_1_to_5)
#> [1] TRUE

# By default, interval endpoints are considered within
phint_within(as.Date("2000-01-01"), jan_1_to_5)
#> [1] TRUE

# Use bounds to consider intervals as exclusive of endpoints
phint_within(as.Date("2000-01-01"), jan_1_to_5, bounds = "()")
#> [1] FALSE

# Holes are never within any interval (including other holes)
hole <- hole()
phint_within(c(hole, hole), c(hole, jan_1_to_5))
#> [1] FALSE FALSE
```
