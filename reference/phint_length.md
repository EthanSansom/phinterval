# Compute the length of a phinterval in seconds

`phint_length()` calculates the total length of all time spans within
each phinterval element in seconds. For phintervals with multiple
disjoint spans, the lengths are summed. Instantaneous intervals and
holes have length 0.

`phint_lengths()` returns the individual length in seconds of each time
span within each phinterval element.

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
