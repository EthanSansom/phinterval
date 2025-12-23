# Test whether two phintervals overlap

`phint_overlaps()` tests whether the i-th element of `phint1` overlaps
with the i-th element of `phint2`, returning a logical vector. Adjacent
intervals are considered overlapping. `phint1` and `phint2` are recycled
to their common length using vctrs-style recycling rules.

## Usage

``` r
phint_overlaps(phint1, phint2)
```

## Arguments

- phint1, phint2:

  `[phinterval / Interval]`

  A pair of `<phinterval>` or `<Interval>` vectors of the same length or
  of length 1. Inputs of length 1 are recycled.

## Value

A logical vector.

## Examples

``` r
monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
tuesday <- interval(as.Date("2025-11-11"), as.Date("2025-11-12"))
friday <- interval(as.Date("2025-11-14"), as.Date("2025-11-15"))
mon_and_fri <- phint_union(monday, friday)

phint_overlaps(c(monday, monday, friday), c(mon_and_fri, friday, NA))
#> [1]  TRUE FALSE    NA

# Adjacent intervals are considered overlapping
phint_overlaps(monday, tuesday)
#> [1] TRUE

# Holes are always considered non-overlapping
hole <- phinterval(interval())
phint_overlaps(c(hole, monday), c(hole, hole))
#> [1] FALSE FALSE
```
