# Test whether two phintervals overlap

`phint_overlaps()` tests whether the i-th element of `phint1` overlaps
with the i-th element of `phint2`, returning a logical vector. Adjacent
intervals (where one ends exactly when the other begins) are considered
overlapping. `phint1` and `phint2` are recycled to their common length
using vctrs-style recycling rules.

## Usage

``` r
phint_overlaps(phint1, phint2, bounds = c("[]", "()"))
```

## Arguments

- phint1, phint2:

  `[phinterval / Interval]`

  A pair of `<phinterval>` or `<Interval>` vectors. `phint1` and
  `phint2` are recycled to a common length using vctrs-style recycling.

- bounds:

  `["[]" / "()"]`

  Whether span endpoints are inclusive or exclusive:

  - `"[]"` (default): Closed intervals - both endpoints are included

  - `"()"`: Open intervals - both endpoints are excluded

  This affects adjacency and overlap detection. For example, with
  `bounds = "[]"`, the intervals `[1, 5]` and `[5, 10]` are considered
  adjacent (they share the endpoint 5), while with `bounds = "()"`,
  `(1, 5)` and `(5, 10)` are disjoint (neither includes 5).

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

# Adjacent intervals are considered overlapping by default
phint_overlaps(monday, tuesday)
#> [1] TRUE

# Use exclusive bounds to consider adjacent intervals as disjoint
phint_overlaps(monday, tuesday, bounds = "()")
#> [1] FALSE

# Holes never overlap with anything (including other holes)
hole <- hole()
phint_overlaps(c(hole, monday), c(hole, hole))
#> [1] FALSE FALSE
```
