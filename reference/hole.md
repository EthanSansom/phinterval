# Create a hole phinterval

`hole()` creates a `<phinterval>` vector where each element is a hole
(an empty set of time spans).

## Usage

``` r
hole(n = 1L, tzone = "")
```

## Arguments

- n:

  `[integerish(1)]`

  The number of hole elements to create. Must be a positive whole
  number.

- tzone:

  `[character(1)]`

  A time zone to display the `<phinterval>` in. Defaults to `""`.

## Value

A `<phinterval>` vector of length `n` where each element is a `<hole>`.

## Details

A hole is a phinterval element with zero time spans, representing an
empty interval. Holes are useful as placeholders or for representing the
absence of time periods in interval algebra operations.

## Examples

``` r
# Create a single hole
hole()
#> <phinterval<local>[1]>
#> [1] <hole>

# Create multiple holes
hole(3)
#> <phinterval<local>[3]>
#> [1] <hole> <hole> <hole>

# Specify time zone
hole(tzone = "UTC")
#> <phinterval<UTC>[1]>
#> [1] <hole>

# Holes can be combined with other phintervals
jan <- phinterval(as.Date("2000-01-01"), as.Date("2000-02-01"))
c(jan, hole(), jan)
#> <phinterval<UTC>[3]>
#> [1] {2000-01-01--2000-02-01} <hole>                   {2000-01-01--2000-02-01}
```
