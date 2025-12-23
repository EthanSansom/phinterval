# Get the gaps in a phinterval as time spans

`phint_invert()` returns the gaps in a phinterval as a `<phinterval>`
vector. Contiguous time spans (e.g.
[`lubridate::interval()`](https://lubridate.tidyverse.org/reference/interval.html)
vectors) are inverted to `<hole>` time spans.

`phint_invert()` is similar to
[`phint_complement()`](https://ethansansom.github.io/phinterval/reference/phinterval-set-operations.md),
except that the time occurring outside the extent of `phint` (i.e.
before its earliest start or after its latest end) is not included in
the result.

## Usage

``` r
phint_invert(phint, hole_to = c("hole", "inf", "na"))

# Default S3 method
phint_invert(phint, hole_to = c("hole", "inf", "na"))

# S3 method for class 'Interval'
phint_invert(phint, hole_to = c("hole", "inf", "na"))

# S3 method for class 'phinterval'
phint_invert(phint, hole_to = c("hole", "inf", "na"))
```

## Arguments

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

- hole_to:

  `["hole" / "inf" / "na"]`

  What to turn `<hole>` (i.e. empty) time spans into.

  - If `hole_to = "hole"` (the default), `<hole>` spans remain as
    `<hole>` elements.

  - If `"inf"`, they are returned as a time span from `-Inf` to `Inf`.

  - If `"na"`, they are returned as a missing (`NA`) span.

## Value

A `<phinterval>` vector the same length as `phint`.

## Examples

``` r
monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
friday <- interval(as.Date("2025-11-14"), as.Date("2025-11-15"))
sunday <- interval(as.Date("2025-11-16"), as.Date("2025-11-17"))

# Contiguous intervals are inverted to holes,
# disjoint intervals to spans
phint_invert(monday)
#> <phinterval<UTC>[1]>
#> [1] <hole>
phint_invert(phint_squash(c(monday, friday, sunday)))
#> <phinterval<UTC>[1]>
#> [1] {2025-11-11--2025-11-14, 2025-11-15--2025-11-16}

tues_to_thurs <- interval(as.Date("2025-11-11"), as.Date("2025-11-14"))
phint_invert(phint_union(monday, friday)) == tues_to_thurs
#> [1] TRUE

# The time before `monday` and after `friday` is included
# in the complement, but not the inversion
mon_and_fri <- phint_union(monday, friday)
phint_invert(mon_and_fri)
#> <phinterval<UTC>[1]>
#> [1] {2025-11-11--2025-11-14}
phint_complement(mon_and_fri)
#> <phinterval<UTC>[1]>
#> [1] {-Inf--2025-11-10, 2025-11-11--2025-11-14, 2025-11-15--Inf}

# Specify how to invert empty time spans
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
