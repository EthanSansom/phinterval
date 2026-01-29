# Test if the object is a recognized time zone

`is_recognized_tzone()` returns `TRUE` for strings that are recognized
IANA time zone names, and `FALSE` otherwise.

## Usage

``` r
is_recognized_tzone(x)
```

## Arguments

- x:

  An object to test.

## Value

`TRUE` if `x` is a recognized time zone, `FALSE` otherwise.

## Details

Recognized time zones are those listed in
[`tzdb::tzdb_names()`](https://tzdb.r-lib.org/reference/tzdb_names.html),
which provides an up-to-date copy of time zones from the IANA time zone
database.

`<phinterval>` vectors with an unrecognized time zone are formatted
using the `"UTC"` time zone with a warning.

## Examples

``` r
is_recognized_tzone("UTC")
#> [1] TRUE
is_recognized_tzone("America/New_York")
#> [1] TRUE
is_recognized_tzone("")
#> [1] TRUE
is_recognized_tzone("badzone")
#> [1] FALSE
is_recognized_tzone(10L)
#> [1] FALSE
```
