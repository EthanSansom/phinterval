# Create a new phinterval

`phinterval()` creates a new `<phinterval>` vector from a list of
[`lubridate::interval()`](https://lubridate.tidyverse.org/reference/interval.html)
vectors. A phinterval (think "potentially holey interval") is a span of
time which may contain gaps.

## Usage

``` r
phinterval(intervals = list(), tzone = NULL)
```

## Arguments

- intervals:

  `[list of Interval]`

  A list of
  [`lubridate::interval()`](https://lubridate.tidyverse.org/reference/interval.html)
  vectors. Each interval vector is merged into a set of non-overlapping
  and non-adjacent time spans. Empty (length 0) interval vectors are
  returned as an empty set of time spans (i.e. a `<hole>`).

- tzone:

  `[character(1)]`

  A recognized timezone to display the `<phinterval>` in. If `tzone` is
  `NULL` (the default), then the timezone of the first element of
  `intervals` is used.

## Value

A `<phinterval>` vector.

## Details

The `<phinterval>` class is designed as a generalization of the
[lubridate::Interval](https://lubridate.tidyverse.org/reference/Interval-class.html).
While an `<Interval>` element represents a single contiguous span
between two fixed times, a `<phinterval>` element can represent a time
span that may be empty, contiguous, or disjoint (i.e. containing gaps).
Each element of a `<phinterval>` is stored as a (possibly empty) set of
non-overlapping and non-abutting time spans.

## Examples

``` r
jan <- interval(as.Date("2000-01-01"), as.Date("2000-02-01"), tz = "UTC")
feb <- interval(as.Date("2000-02-01"), as.Date("2000-03-01"), tz = "UTC")
nov <- interval(as.Date("2000-11-01"), as.Date("2000-12-01"), tz = "UTC")

phinterval(list(jan, c(feb, nov)))
#> <phinterval<UTC>[2]>
#> [1] {2000-01-01--2000-02-01}                        
#> [2] {2000-02-01--2000-03-01, 2000-11-01--2000-12-01}
phinterval(list(jan), tzone = "EST")
#> <phinterval<EST>[1]>
#> [1] {1999-12-31 19:00:00--2000-01-31 19:00:00}
phinterval()
#> <phinterval<UTC>[0]>

# Empty (length 0) intervals can be used to create <hole> elements
phinterval(list(interval()))
#> <phinterval<UTC>[1]>
#> [1] <hole>

# Abutting or overlapping intervals are merged into one span
phinterval(list(c(jan, feb)))
#> <phinterval<UTC>[1]>
#> [1] {2000-01-01--2000-03-01}
```
