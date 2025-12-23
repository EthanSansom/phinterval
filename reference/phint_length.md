# Compute the length of a phinterval in seconds

Compute the length of a phinterval in seconds

## Usage

``` r
phint_length(phint)

# Default S3 method
phint_length(phint)

# S3 method for class 'Interval'
phint_length(phint)

# S3 method for class 'phinterval'
phint_length(phint)

phint_lengths(phint)

# Default S3 method
phint_lengths(phint)

# S3 method for class 'Interval'
phint_lengths(phint)

# S3 method for class 'phinterval'
phint_lengths(phint)
```

## Arguments

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

## Value

For `phint_length()`, a numeric vector the same length as `phint`.

For `phint_lengths()`, a list of numeric vectors the same length as
`phint`.

## Details

`phint_length()` calculates the total length of time spans within a
phinterval. Instantaneous and empty time spans are 0 seconds long.

`phint_lengths()` calculates the length in seconds of each time span in
a phinterval.

## Examples

``` r
monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
friday <- interval(as.Date("2025-11-14"), as.Date("2025-11-15"))

phint_length(monday)
#> [1] 86400
phint_length(phint_intersect(monday, friday))
#> [1] 0

# phint_length() sums the lengths of disjoint time spans
mon_and_fri <- phint_union(monday, friday)
phint_length(mon_and_fri) == phint_length(monday) + phint_length(friday)
#> [1] TRUE

# phint_lengths() returns the length of each disjoint time span
phint_lengths(mon_and_fri)
#> [[1]]
#> [1] 86400 86400
#> 
```
