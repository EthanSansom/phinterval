# Convert a phinterval to a duration

`as_duration()` changes
[`lubridate::interval()`](https://lubridate.tidyverse.org/reference/interval.html)
and `<phinterval>` vectors into
[`lubridate::duration()`](https://lubridate.tidyverse.org/reference/duration.html)
vectors. The resulting duration measures the length of time in seconds
within each element of the interval or phinterval.

`as_duration()` is a wrapper around
[`lubridate::as.duration()`](https://lubridate.tidyverse.org/reference/as.duration.html).

## Usage

``` r
as_duration(x, ...)

# Default S3 method
as_duration(x, ...)

# S3 method for class 'phinterval'
as_duration(x, ...)
```

## Arguments

- x:

  An object to convert.

- ...:

  Parameters passed to other methods. Currently unused.

## Value

A `<Duration>` vector the same length as `x`.

## Examples

``` r
monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
friday <- interval(as.Date("2025-11-14"), as.Date("2025-11-15"))
mon_and_fri <- phint_union(monday, friday)

as_duration(c(mon_and_fri, monday))
#> [1] "172800s (~2 days)" "86400s (~1 days)" 
as_duration(mon_and_fri) == as_duration(monday) + as_duration(friday)
#> [1] TRUE
```
