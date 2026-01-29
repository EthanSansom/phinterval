# Convert an interval or datetime vector into a phinterval

`as_phinterval()` converts a
[`lubridate::interval()`](https://lubridate.tidyverse.org/reference/interval.html),
Date, POSIXct, or POSIXlt vector into an equivalent `<phinterval>`
vector.

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

  `[Interval / Date / POSIXct / POSIXlt]`

  An object to convert.

- ...:

  Additional arguments passed to methods. Currently unused.

## Value

A `<phinterval>` vector the same length as `x`.

## Details

Negative intervals (where start \> end) are standardized to positive
intervals via
[`lubridate::int_standardize()`](https://lubridate.tidyverse.org/reference/interval.html).

Datetime vectors (Date, POSIXct, POSIXlt) are converted into
instantaneous intervals where the start and end are identical.

Spans with partially missing endpoints (e.g., `interval(NA, end)` or
`interval(start, NA)`) are converted to a fully `NA` element.

## See also

[`phinterval()`](https://ethansansom.github.io/phinterval/reference/phinterval.md)

## Examples

``` r
# Convert Interval vector
years <- interval(
  start = as.Date(c("2021-01-01", "2023-01-01")),
  end = as.Date(c("2022-01-01", "2024-01-01"))
)
as_phinterval(years)
#> <phinterval<UTC>[2]>
#> [1] {2021-01-01--2022-01-01} {2023-01-01--2024-01-01}

# Negative intervals are standardized
negative <- interval(as.Date("2000-10-11"), as.Date("2000-10-01"))
as_phinterval(negative)
#> <phinterval<UTC>[1]>
#> [1] {2000-10-01--2000-10-11}

# Partially missing endpoints become fully NA
partial_na <- interval(NA, as.Date("1999-08-02"))
as_phinterval(partial_na)
#> <phinterval<UTC>[1]>
#> [1] <NA>

# Datetime vectors become instantaneous intervals
as_phinterval(as.Date(c("2000-10-11", "2001-05-03")))
#> <phinterval<UTC>[2]>
#> [1] {2000-10-11--2000-10-11} {2001-05-03--2001-05-03}
```
