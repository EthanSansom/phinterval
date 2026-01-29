# Get the gaps in a phinterval as time spans

`phint_invert()` returns the gaps within a phinterval as a
`<phinterval>` vector. For phintervals with multiple disjoint spans, the
gaps between those spans are returned. Contiguous time spans (e.g.,
[`lubridate::interval()`](https://lubridate.tidyverse.org/reference/interval.html)
vectors) have no gaps and are inverted to holes.

`phint_invert()` is similar to
[`phint_complement()`](https://ethansansom.github.io/phinterval/reference/phinterval-set-operations.md),
except that time occurring outside the extent of `phint` (before its
earliest start or after its latest end) is not included in the result.

## Usage

``` r
phint_invert(phint, hole_to = c("hole", "inf", "na"))
```

## Arguments

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

- hole_to:

  `["hole" / "inf" / "na"]`

  How to handle holes (empty phinterval elements):

  - `"hole"` (default): Holes remain as holes

  - `"inf"`: Return a span from `-Inf` to `Inf` (all time)

  - `"na"`: Return an `NA` phinterval

## Value

A `<phinterval>` vector the same length as `phint`.

## Examples

``` r
monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
friday <- interval(as.Date("2025-11-14"), as.Date("2025-11-15"))
sunday <- interval(as.Date("2025-11-16"), as.Date("2025-11-17"))

# Contiguous intervals have no gaps (inverted to holes)
phint_invert(monday)
#> <phinterval<UTC>[1]>
#> [1] <hole>

# Disjoint intervals: gaps between spans are returned
phint_invert(phint_squash(c(monday, friday, sunday)))
#> <phinterval<UTC>[1]>
#> [1] {2025-11-11--2025-11-14, 2025-11-15--2025-11-16}

# The gap between Monday and Friday is Tuesday through Thursday
tues_to_thurs <- interval(as.Date("2025-11-11"), as.Date("2025-11-14"))
phint_invert(phint_union(monday, friday)) == tues_to_thurs
#> [1] TRUE

# Invert vs complement: time before and after is excluded from invert
mon_and_fri <- phint_union(monday, friday)
phint_invert(mon_and_fri)
#> <phinterval<UTC>[1]>
#> [1] {2025-11-11--2025-11-14}
phint_complement(mon_and_fri)
#> <phinterval<UTC>[1]>
#> [1] {-Inf-[3]-Inf}

# How to invert holes
hole <- phint_intersect(monday, friday)
phint_invert(hole, hole_to = "hole")
#> <phinterval<UTC>[1]>
#> [1] <hole>
phint_invert(hole, hole_to = "inf")
#> <phinterval<UTC>[1]>
#> [1] {-Inf--Inf}
phint_invert(hole, hole_to = "na")
#> <phinterval<UTC>[1]>
#> [1] <NA>
```
