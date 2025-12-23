# Accessors for the endpoints of a phinterval

`phint_start()` and `phint_end()` return the earliest and latest
endpoint of a phinterval respectively. Empty (i.e. `<hole>`) time spans
are coerced to `NA` datetimes.

`phint_starts()` and `phint_ends()` return a list of starts and ends of
a phinterval respectively. Empty time spans are returned as length 0
`<POSIXct>` elements.

## Usage

``` r
phint_start(phint)

# Default S3 method
phint_start(phint)

# S3 method for class 'Interval'
phint_start(phint)

# S3 method for class 'phinterval'
phint_start(phint)

phint_end(phint)

# Default S3 method
phint_end(phint)

# S3 method for class 'Interval'
phint_end(phint)

# S3 method for class 'phinterval'
phint_end(phint)

phint_starts(phint)

# Default S3 method
phint_starts(phint)

# S3 method for class 'Interval'
phint_starts(phint)

# S3 method for class 'phinterval'
phint_starts(phint)

phint_ends(phint)

# Default S3 method
phint_ends(phint)

# S3 method for class 'Interval'
phint_ends(phint)

# S3 method for class 'phinterval'
phint_ends(phint)
```

## Arguments

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

## Value

For `phint_start()` and `phint_end()`, a `<POSIXct>` vector the same
length as `phint`.

For `phint_start()` and `phint_ends()`, a list of `<POSIXct>` vectors
the same length as `phint`.

## Examples

``` r
int1 <- interval(as.Date("2020-01-10"), as.Date("2020-02-01"))
int2 <- interval(as.Date("2023-05-02"), as.Date("2023-06-03"))

phint_start(int1)
#> [1] "2020-01-10 UTC"
phint_end(int1)
#> [1] "2020-02-01 UTC"

# Empty <hole> times spans have no start or end date. Time spans containing
# gaps have multiple starts and ends.
hole <- phint_intersect(int1, int2)
disjoint <- phint_union(int1, int2)

phint_start(c(hole, disjoint))
#> [1] NA               "2020-01-10 UTC"
phint_starts(c(hole, disjoint))
#> [[1]]
#> POSIXct of length 0
#> 
#> [[2]]
#> [1] "2020-01-10 UTC" "2023-05-02 UTC"
#> 

phint_end(c(hole, disjoint))
#> [1] NA               "2023-06-03 UTC"
phint_ends(c(hole, disjoint))
#> [[1]]
#> POSIXct of length 0
#> 
#> [[2]]
#> [1] "2020-02-01 UTC" "2023-06-03 UTC"
#> 

# phint_start() and phint_end() return the minimum and maximum endpoints
negative <- interval(as.Date("1980-01-01"), as.Date("1980-01-01") - 5)
phint_start(negative)
#> [1] "1979-12-27 UTC"
phint_end(negative)
#> [1] "1980-01-01 UTC"
```
