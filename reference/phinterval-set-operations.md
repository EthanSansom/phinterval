# Vectorized set operations

These functions perform elementwise set operations on `<phinterval>`
vectors, treating each element as a set of non-overlapping intervals.
They return a new `<phinterval>` vector representing the result of the
corresponding set operation. All functions follow vctrs-style recycling
rules.

- `phint_complement()` returns all time spans *not covered* by `phint`.

- `phint_union()` returns the intervals that are within either `phint1`
  or `phint2`.

- `phint_intersect()` returns the intervals that are within both
  `phint1` and `phint2`.

- `phint_setdiff()` returns intervals in `phint1` that are not within
  `phint2`.

## Usage

``` r
phint_complement(phint)

phint_union(phint1, phint2)

phint_intersect(phint1, phint2, bounds = c("[]", "()"))

phint_setdiff(phint1, phint2)
```

## Arguments

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

- phint1, phint2:

  `[phinterval / Interval]`

  A pair of `<phinterval>` or `<Interval>` vectors. `phint1` and
  `phint2` are recycled to a common length using vctrs-style recycling.

- bounds:

  `["[]" / "()"]`

  For `phint_intersect()` only, whether span endpoints are inclusive or
  exclusive:

  - `"[]"` (default): Closed intervals - both endpoints are included

  - `"()"`: Open intervals - both endpoints are excluded

  This affects adjacency and overlap detection. For example, with
  `bounds = "[]"`, the intervals `[1, 5]` and `[5, 10]` are considered
  adjacent (they share the endpoint 5), while with `bounds = "()"`,
  `(1, 5)` and `(5, 10)` are disjoint (neither includes 5).

## Value

A `<phinterval>` vector.

## Examples

``` r
monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
tuesday <- interval(as.Date("2025-11-11"), as.Date("2025-11-12"))
friday <- interval(as.Date("2025-11-14"), as.Date("2025-11-15"))
jan_1_to_5 <- interval(as.Date("2000-01-01"), as.Date("2000-01-05"))
jan_3_to_9 <- interval(as.Date("2000-01-03"), as.Date("2000-01-09"))

# Complement
phint_complement(jan_1_to_5)
#> <phinterval<UTC>[1]>
#> [1] {-Inf--2000-01-01, 2000-01-05--Inf}

# The complement of a hole is an infinite span covering all time
hole <- hole()
phint_complement(hole)
#> <phinterval<local>[1]>
#> [1] {-Inf--Inf}

# Union
phint_union(c(monday, monday, monday), c(tuesday, friday, NA))
#> <phinterval<UTC>[3]>
#> [1] {2025-11-10--2025-11-12}                        
#> [2] {2025-11-10--2025-11-11, 2025-11-14--2025-11-15}
#> [3] <NA>                                            

# Elements of length 1 are recycled
phint_union(monday, c(tuesday, friday, NA))
#> <phinterval<UTC>[3]>
#> [1] {2025-11-10--2025-11-12}                        
#> [2] {2025-11-10--2025-11-11, 2025-11-14--2025-11-15}
#> [3] <NA>                                            

# Intersection
phint_intersect(jan_1_to_5, jan_3_to_9)
#> <phinterval<UTC>[1]>
#> [1] {2000-01-03--2000-01-05}

# The intersection of non-overlapping intervals is a hole
phint_intersect(monday, friday)
#> <phinterval<UTC>[1]>
#> [1] <hole>

# By default, the intersection of adjacent intervals is instantaneous
phint_intersect(monday, tuesday)
#> <phinterval<UTC>[1]>
#> [1] {2025-11-11--2025-11-11}

# Use bounds to set the intersection of adjacent intervals to a hole
phint_intersect(monday, tuesday, bounds = "()")
#> <phinterval<UTC>[1]>
#> [1] <hole>

# Set difference
phint_setdiff(jan_1_to_5, jan_3_to_9)
#> <phinterval<UTC>[1]>
#> [1] {2000-01-01--2000-01-03}
phint_setdiff(jan_3_to_9, jan_1_to_5)
#> <phinterval<UTC>[1]>
#> [1] {2000-01-05--2000-01-09}

# Instantaneous intervals do not affect the set difference
noon_monday <- as.POSIXct("2025-11-10 12:00:00")
phint_setdiff(monday, interval(noon_monday, noon_monday)) == monday
#> [1] TRUE
```
