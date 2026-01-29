# Introduction to phinterval

``` r
library(phinterval)
library(lubridate, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
```

### Introduction

The phinterval package extends
[{lubridate}](https://lubridate.tidyverse.org/) to support disjoint
(“holey”) and empty time spans. It implements the `<phinterval>` vector
class, a generalization of the standard contiguous `<Interval>`, which
can represent:

- **Contiguous spans:** A contiguous interval bounded by a start and end
  point (e.g., the year 2025).
- **Empty spans:** A set containing no time points (e.g., the
  intersection of your life and Napoleon’s).
- **Disjoint spans:** A set of multiple time spans separated by gaps
  (e.g., the days you attended school, excluding weekends and holidays).

This package is designed to easily integrate into existing lubridate
workflows. Any `<Interval>` vector can be converted to an equivalent
`<phinterval>` vector using
[`as_phinterval()`](https://ethansansom.github.io/phinterval/reference/as_phinterval.md),
and all phinterval functions accept either `<Interval>` or
`<phinterval>` inputs.

### When Time Isn’t Continuous

Certain set operations on time spans naturally produce empty or disjoint
results, which are difficult to represent using a standard interval.
This section illustrates several such edge cases using the months of
January and November 2025, along with the full calendar year.

``` r
jan <- interval(ymd("2025-01-01"), ymd("2025-02-01"))
nov <- interval(ymd("2025-11-01"), ymd("2025-12-01"))
full_2025 <- interval(ymd("2025-01-01"), ymd("2026-01-01"))
```

#### Empty Intersections

Because January and November do not overlap, their intersection should
contain no time.

``` r
lubridate::intersect(jan, nov)
#> [1] NA--NA

phint_intersect(jan, nov)
#> <phinterval<UTC>[1]>
#> [1] <hole>
```

In lubridate this is resolved by coercing the intersection to `NA`,
while phinterval returns a `<hole>`, which explicitly represents an
empty span of time.

This distinction matters when performing downstream calculations. For
example, counting the number of days contained in both January and
November:

``` r
lubridate::intersect(jan, nov) / duration(days = 1)
#> [1] NA

phint_intersect(jan, nov) / duration(days = 1)
#> [1] 0
```

#### Punching Holes in Intervals

Next, consider subtracting the month of November from the full year of
2025.

``` r
try(lubridate::setdiff(full_2025, nov))
#> Error in setdiff.Interval(full_2025, nov) : 
#>   Cases 1 result in discontinuous intervals.

phint_setdiff(full_2025, nov)
#> <phinterval<UTC>[1]>
#> [1] {2025-01-01--2025-11-01, 2025-12-01--2026-01-01}
```

The result is two disjoint spans, January through October and December,
which can’t be represented by a single interval. As a result, lubridate
raises an error. In phinterval, the disjoint span is represented as a
single object with an explicit gap.

#### Unions of Non-Overlapping Spans

Similarly, the union of January and November contains a gap from
February to October.

``` r
lubridate::union(jan, nov)
#> [1] 2025-01-01 UTC--2025-12-01 UTC

phint_union(jan, nov)
#> <phinterval<UTC>[1]>
#> [1] {2025-01-01--2025-02-01, 2025-11-01--2025-12-01}
```

In this case lubridate returns the span from the beginning of January to
the end of November, implicitly filling in the gap. The two disjoint
months are represented explicitly using phinterval.

#### Subtracting an Interval from Itself

Finally, consider subtracting an interval from itself. Intuitively, this
should result in an empty time span.

``` r
lubridate::setdiff(jan, jan)
#> [1] 2025-01-01 UTC--2025-02-01 UTC

phint_setdiff(jan, jan)
#> <phinterval<UTC>[1]>
#> [1] <hole>
```

In this case, lubridate returns the original interval, while phinterval
returns a `<hole>`.

### Case Study: Employment History

The phinterval package is most useful when working with tabular data and
vectorized workflows. To illustrate this, we’ll consider an abridged
employment history for several characters from the television show
*Succession*.

``` r
jobs <- tribble(
  ~name,   ~job_title,             ~start,        ~end,
  "Greg",  "Mascot",               "2018-01-01",  "2018-06-03",
  "Greg",  "Executive Assistant",  "2018-06-10",  "2020-04-01",
  "Greg",  "Chief of Staff",       "2020-03-01",  "2020-11-28",
  "Tom",   "Chairman",             "2019-05-01",  "2020-11-10",
  "Tom",   "CEO",                  "2020-11-10",  "2020-12-31",
  "Shiv",  "Political Consultant", "2017-01-01",  "2019-04-01"
)
```

Suppose we know that Greg, Tom, and Shiv went on a group Christmas
vacation in December 2017.

``` r
vacation <- interval(ymd("2017-12-23"), ymd("2017-12-29"))
```

If we want to analyze only the time spent working, and exclude time on
vacation, we might try to subtract the vacation interval from each job
span. However, this approach breaks down when the vacation falls
strictly within a job interval, as it does for Shiv’s Political
Consultant role.

``` r
try(
  jobs |>
    mutate(
      span = interval(start, end),
      span = setdiff(span, vacation)
    ) |>
    select(name, job_title, span)
)
#> Error in mutate(jobs, span = interval(start, end), span = setdiff(span,  : 
#>   ℹ In argument: `span = setdiff(span, vacation)`.
#> Caused by error in `setdiff.Interval()`:
#> ! Cases 6 result in discontinuous intervals.
```

Handling this correctly is surprisingly involved. One option is to split
Shiv’s job into two rows (one pre-vacation and one post-vacation),
breaking the one-row-per-job structure of the data. Another is to
represent each job as a list of intervals, complicating downstream
analysis.

The main purpose of phinterval is to avoid these workarounds, by
providing drop-in replacements for lubridate interval functions. Because
phinterval functions accept either `<Interval>` or `<phinterval>`
inputs, existing code can typically be adapted by simply replacing a
lubridate function with its phinterval counterpart.

``` r
jobs |>
  mutate(
    span = interval(start, end),
    span = phint_setdiff(span, vacation)
  ) |>
  select(name, job_title, span)
#> # A tibble: 6 × 3
#>   name  job_title            span                       
#>   <chr> <chr>                <phint<UTC>>               
#> 1 Greg  Mascot               {2018-01-01--2018-06-03}   
#> 2 Greg  Executive Assistant  {2018-06-10--2020-04-01}   
#> 3 Greg  Chief of Staff       {2020-03-01--2020-11-28}   
#> 4 Tom   Chairman             {2019-05-01--2020-11-10}   
#> 5 Tom   CEO                  {2020-11-10--2020-12-31}   
#> 6 Shiv  Political Consultant {2017-01-01-[2]-2019-04-01}
```

#### Merging Intervals

Suppose we want to analyze only the total time each character spent
employed, without distinguishing between individual jobs. This can be
done using
[`phint_squash()`](https://ethansansom.github.io/phinterval/reference/squash.md),
which aggregates a vector of intervals into a minimal set of
non-overlapping spans within a scalar `<phinterval>`.

``` r
employment <- jobs |>
  mutate(span = interval(start, end)) |>
  group_by(name) |>
  summarize(employed = phint_squash(span))

employment
#> # A tibble: 3 × 2
#>   name  employed                   
#>   <chr> <phint<UTC>>               
#> 1 Greg  {2018-01-01-[2]-2020-11-28}
#> 2 Shiv  {2017-01-01--2019-04-01}   
#> 3 Tom   {2019-05-01--2020-12-31}
```

Notice that:

- *Greg* has multiple disjoint employment periods, which are preserved
  as separate spans within a single `<phinterval>` element.
- *Tom* held two back-to-back positions (Chairman followed by CEO),
  which
  [`phint_squash()`](https://ethansansom.github.io/phinterval/reference/squash.md)
  correctly merges into a single contiguous span.

The `by` argument of
[`phint_squash()`](https://ethansansom.github.io/phinterval/reference/squash.md)
and
[`datetime_squash()`](https://ethansansom.github.io/phinterval/reference/squash.md)
(which takes `start` and `end` times directly) can be used in place of
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
The example below is equivalent to the previous code but is usually
several times faster.

``` r
datetime_squash(
  start = ymd(jobs$start),
  end = ymd(jobs$end),
  by = jobs$name,
  keep_by = TRUE,
  order_by = TRUE
) |> as_tibble()
#> # A tibble: 3 × 2
#>   by    phint                      
#>   <chr> <phint<UTC>>               
#> 1 Greg  {2018-01-01-[2]-2020-11-28}
#> 2 Shiv  {2017-01-01--2019-04-01}   
#> 3 Tom   {2019-05-01--2020-12-31}
```

As in
[`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html),
the `by` argument can be a vector or data frame to support multiple
grouping columns.

To return the dataset to a one-row-per-span format, use
[`phint_unnest()`](https://ethansansom.github.io/phinterval/reference/phint_unnest.md),
which converts each `<phinterval>` element into separate rows:

``` r
employment |>
  reframe(phint_unnest(employed, key = name))
#> # A tibble: 4 × 3
#>   key   start               end                
#>   <chr> <dttm>              <dttm>             
#> 1 Greg  2018-01-01 00:00:00 2018-06-03 00:00:00
#> 2 Greg  2018-06-10 00:00:00 2020-11-28 00:00:00
#> 3 Shiv  2017-01-01 00:00:00 2019-04-01 00:00:00
#> 4 Tom   2019-05-01 00:00:00 2020-12-31 00:00:00
```

#### Finding Gaps

To analyze periods of unemployment, we need to identify the gaps between
employment intervals. The
[`phint_invert()`](https://ethansansom.github.io/phinterval/reference/phint_invert.md)
function returns the gaps between spans in a `<phinterval>`.

``` r
unemployment <- employment |>
  mutate(
    # Find the gaps between jobs
    unemployed = phint_invert(employed),
    
    # Calculate duration of unemployment
    days_unemployed = unemployed / ddays(1)
  ) |>
  select(name, unemployed, days_unemployed)

unemployment
#> # A tibble: 3 × 3
#>   name  unemployed               days_unemployed
#>   <chr> <phint<UTC>>                       <dbl>
#> 1 Greg  {2018-06-03--2018-06-10}               7
#> 2 Shiv  <hole>                                 0
#> 3 Tom   <hole>                                 0
```

Greg was unemployed for 7 days between his time as a Mascot and his role
as Executive Assistant. Tom and Shiv have no gaps within their
respective employment timelines, represented by a `<hole>`.

## Edge Cases and Gotchas

### Abutting Intervals and Intersection

Manipulating abutting intervals (intervals that share an endpoint) can
produce sometimes unexpected results. To demonstrate, consider the time
within a Monday and Tuesday in November 2025.

``` r
monday <- interval(ymd("2025-11-10"), ymd("2025-11-11"))
tuesday <- interval(ymd("2025-11-11"), ymd("2025-11-12"))
```

By default, intervals in `<phinterval>` and `<Interval>` vectors have
inclusive endpoints, meaning that midnight on Monday, November 11th,
2025 falls within both `monday` and `tuesday`:

``` r
midnight_monday <- ymd_hms("2025-11-11 00:00:00")
phint_within(midnight_monday, monday)
#> [1] TRUE
phint_within(midnight_monday, tuesday)
#> [1] TRUE
```

As a result, the intersection of `monday` and `tuesday` is an
instantaneous interval at `midnight_monday`.

``` r
phint_intersect(monday, tuesday) == midnight_monday
#>   size starts   ends 
#>  FALSE     NA     NA
```

Perhaps surprisingly, this also means that the intersection of `monday`
and its complement is not empty, but consists of the two endpoints of
`monday`.

``` r
not_monday <- phint_complement(monday)
not_monday
#> <phinterval<UTC>[1]>
#> [1] {-Inf--2025-11-10, 2025-11-11--Inf}

phint_intersect(monday, not_monday)
#> <phinterval<UTC>[1]>
#> [1] {2025-11-10--2025-11-10, 2025-11-11--2025-11-11}
```

The bounds argument in
[`phint_overlaps()`](https://ethansansom.github.io/phinterval/reference/phint_overlaps.md),
[`phint_within()`](https://ethansansom.github.io/phinterval/reference/phint_within.md),
and
[`phint_intersect()`](https://ethansansom.github.io/phinterval/reference/phinterval-set-operations.md)
controls this behavior. When `bounds = "()"`, endpoints are treated as
exclusive:

``` r
phint_overlaps(monday, tuesday, bounds = "()")
#> [1] FALSE
phint_intersect(monday, tuesday, bounds = "()")
#> <phinterval<UTC>[1]>
#> [1] <hole>
```

With exclusive endpoints, `monday` and `tuesday` no longer overlap, and
their intersection is empty.

An instantaneous interval `(point, point)` with open bounds is
mathematically undefined, but for convenience we allow these points to
exist. With `bounds = "()"`, instants on the endpoint of an interval are
outside of the interval, while instants in the middle of an interval are
considered to be within it:

``` r
monday_at_9AM <- as_phinterval(ymd_hms("2025-11-10 00:09:00"))
phint_within(monday_at_9AM, monday, bounds = "()")
#> [1] TRUE
phint_within(midnight_monday, monday, bounds = "()")
#> [1] FALSE
```

To consider instantaneous intervals as empty, use
[`phint_sift()`](https://ethansansom.github.io/phinterval/reference/phint_sift.md)
to remove all instants from an interval vector:

``` r
phint <- phint_squash(c(monday_at_9AM, tuesday))
phint
#> <phinterval<UTC>[1]>
#> [1] {2025-11-10 00:09:00--2025-11-10 00:09:00, 2025-11-11 00:00:00--2025-11-12 00:00:00}

phint_sift(phint)
#> <phinterval<UTC>[1]>
#> [1] {2025-11-11--2025-11-12}
```

### Instantaneous Intervals and Set Difference

Because phinterval elements are composed of non-overlapping,
non-adjacent spans, “punching” an instantaneous hole into an interval
using
[`phint_setdiff()`](https://ethansansom.github.io/phinterval/reference/phinterval-set-operations.md)
has no effect on the interval. While removing a single point from an
interval `[start, end]` would theoretically split it into
`[start, point)` and `(point, end]`, in practice these adjacent pieces
are immediately merged back together:

``` r
monday_noon <- as_phinterval(ymd_hms("2025-11-10 12:00:00"))
monday_lunch_break <- interval(
  ymd_hms("2025-11-10 12:00:00"), 
  ymd_hms("2025-11-10 13:00:00")
)

phint_setdiff(monday, monday_lunch_break) # Removes a non-zero interval
#> <phinterval<UTC>[1]>
#> [1] {2025-11-10 00:00:00--2025-11-10 12:00:00, 2025-11-10 13:00:00--2025-11-11 00:00:00}
phint_setdiff(monday, monday_noon)        # Instantaneous - no effect
#> <phinterval<UTC>[1]>
#> [1] {2025-11-10--2025-11-11}
```

To create gaps, you must remove an interval with non-zero duration.

### Time Zones

To ensure that any `<Interval>` vector can be represented as an
equivalent `<phinterval>` vector, the
[`phinterval()`](https://ethansansom.github.io/phinterval/reference/phinterval.md)
constructor accepts any time zone permitted by
[`interval()`](https://lubridate.tidyverse.org/reference/interval.html),
including unrecognized zones.

``` r
intvl <- interval(ymd("2020-01-01"), ymd("2020-01-02"), tzone = "nozone")
phint <- phinterval(ymd("2020-01-01"), ymd("2020-01-02"), tzone = "nozone")
intvl == phint
#> [1] TRUE
```

When a `<phinterval>` with an unrecognized time zone is formatted, its
time points are displayed using the UTC time zone:

``` r
print(phint)
#> <phinterval<nozone>[1]>
#> [1] {2020-01-01--2020-01-02}
```

The
[`is_recognized_tzone()`](https://ethansansom.github.io/phinterval/reference/is_recognized_tzone.md)
function can be used to check whether a time zone is recognized:

``` r
is_recognized_tzone("America/New_York")
#> [1] TRUE
is_recognized_tzone("nozone")
#> [1] FALSE
is_recognized_tzone(NA_character_)
#> [1] FALSE
```

Some datetime vectors, such as `<POSIXct>`, are allowed to have an `NA`
time zone. When converted to a `<phinterval>`, the missing time zone is
silently replaced with UTC:

``` r
na_zoned <- as.POSIXct("2021-01-01", tz = NA_character_)
as_phinterval(na_zoned)
#> <phinterval<UTC>[1]>
#> [1] {2021-01-01--2021-01-01}
```

Operations that combine two or more interval vectors, such as
[`phint_union()`](https://ethansansom.github.io/phinterval/reference/phinterval-set-operations.md),
use the time zone of the first argument. If the first argument’s time
zone is `""` (the user’s local time zone), the second argument’s time
zone is used instead.

``` r
int_est <- interval(ymd("2020-01-01"), ymd("2020-01-02"), tzone = "EST")
int_utc <- interval(ymd("2020-01-01"), ymd("2020-01-02"), tzone = "UTC")
int_lcl <- interval(ymd("2020-01-01"), ymd("2020-01-02"), tzone = "")

phint_union(int_est, int_utc)
#> <phinterval<EST>[1]>
#> [1] {2019-12-31 19:00:00--2020-01-01 19:00:00}
phint_union(int_utc, int_est)
#> <phinterval<UTC>[1]>
#> [1] {2020-01-01--2020-01-02}
phint_union(int_lcl, int_est)
#> <phinterval<EST>[1]>
#> [1] {2019-12-31 19:00:00--2020-01-01 19:00:00}
```
