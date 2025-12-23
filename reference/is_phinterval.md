# Test if the object is a phinterval

This function returns `TRUE` for `<phinterval>` vectors and returns
`FALSE` otherwise.

## Usage

``` r
is_phinterval(x)
```

## Arguments

- x:

  An object to test.

## Value

`TRUE` if `x` is a `<phinterval>`, `FALSE` otherwise.

## Examples

``` r
is_phinterval(phinterval())
#> [1] TRUE
is_phinterval(interval())
#> [1] FALSE
```
