# Convert a phinterval into a list of intervals

`phint_to_spans()` decomposes each element of a phinterval into the set
of its contiguous time spans, returned as
[`lubridate::interval()`](https://lubridate.tidyverse.org/reference/interval.html)
vectors.

## Usage

``` r
phint_to_spans(phint, hole_to = c("empty", "na", "null"))
```

## Arguments

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

- hole_to:

  `["empty" / "na" / "null"]`

  What to turn `<hole>` (i.e. empty) time spans into. If
  `hole_to = "empty"` (the default), `<hole>` spans are returned as
  length 0 intervals. If `"na"`, they are returned as a missing (`NA`)
  interval. If `"null"`, they are returned as a `NULL` element.

## Value

A list of `<Interval>` vectors the same length as `phint`. If
`hole_to = "null"`, the list may contain `NULL` elements.

## Examples

``` r
monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
friday <- interval(as.Date("2025-11-14"), as.Date("2025-11-15"))
sunday <- interval(as.Date("2025-11-16"), as.Date("2025-11-17"))

mon_and_fri <- phint_union(monday, friday)
phint_to_spans(mon_and_fri)
#> [[1]]
#> [1] 2025-11-10 UTC--2025-11-11 UTC 2025-11-14 UTC--2025-11-15 UTC
#> 
phint_to_spans(sunday)
#> [[1]]
#> [1] 2025-11-16 UTC--2025-11-17 UTC
#> 

# Specify how to invert empty time spans
hole <- phint_intersect(monday, friday)
phint_to_spans(hole, hole_to = "empty")
#> [[1]]
#> <Interval[0]>
#> 
phint_to_spans(hole, hole_to = "na")
#> [[1]]
#> [1] NA--NA
#> 
phint_to_spans(hole, hole_to = "null")
#> [[1]]
#> NULL
#> 
```
