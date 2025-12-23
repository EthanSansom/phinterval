# Test if the object is a phinterval or interval

This function returns `TRUE` for `<phinterval>` and `<Interval>` vectors
and returns `FALSE` otherwise.

## Usage

``` r
is_phintish(x)
```

## Arguments

- x:

  An object to test.

## Value

`TRUE` if `x` is a `<phinterval>` or `<Interval>`, `FALSE` otherwise.

## Examples

``` r
is_phinterval(phinterval())
#> [1] TRUE
is_phinterval(interval())
#> [1] FALSE
is_phinterval(as.Date("2020-01-01"))
#> [1] FALSE
```
