# Convert an interval vector into a phinterval

`as_phinterval()` changes a
[`lubridate::interval()`](https://lubridate.tidyverse.org/reference/interval.html)
vector into the equivalent `<phinterval>` vector. Negative intervals are
flipped to positive (i.e. via
[`lubridate::int_standardize()`](https://lubridate.tidyverse.org/reference/interval.html)).

## Usage

``` r
as_phinterval(x, ...)

# Default S3 method
as_phinterval(x, ...)

# S3 method for class 'Interval'
as_phinterval(x, ...)
```

## Arguments

- x:

  An object to convert.

- ...:

  Parameters passed to other methods. Currently unused.

## Value

A `<phinterval>` vector the same length as `x`.

## See also

[`phinterval()`](https://ethansansom.github.io/phinterval/reference/phinterval.md)

## Examples

``` r
years <- interval(
  start = as.Date(c("2021-01-01", "2023-01-01")),
  end =  as.Date(c("2022-01-01", "2024-01-01"))
)
as_phinterval(years)
#> <phinterval<UTC>[2]>
#> [1] {2021-01-01--2022-01-01} {2023-01-01--2024-01-01}

# Negative intervals are standardized
(negative <- interval(as.Date("2000-10-11"), as.Date("2000-10-11") - 10))
#> [1] 2000-10-11 UTC--2000-10-01 UTC
as_phinterval(negative)
#> <phinterval<UTC>[1]>
#> [1] {2000-10-01--2000-10-11}

# A <phinterval> cannot have partially missing endpoints
(partial_na <- interval(NA, as.Date("1999-08-02")))
#> [1] NA--NA
as_phinterval(partial_na)
#> <phinterval<UTC>[1]>
#> [1] <NA>
```
