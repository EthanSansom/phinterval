# Introduction to phinterval

``` r
library(phinterval)
library(lubridate, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
```

## Introduction

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

## When Time Isn’t Continuous

Certain set operations on time spans naturally produce empty or disjoint
results, which are difficult to represent using a standard interval.
This section illustrates several such edge cases using the months of
January and November 2025, along with the full calendar year.

``` r
jan <- interval(ymd("2025-01-01"), ymd("2025-01-01") + months(1))
nov <- interval(ymd("2025-11-01"), ymd("2025-11-01") + months(1))
full_2025 <- interval(ymd("2025-01-01"), ymd("2025-01-01") + years(1))
```

### Empty Intersections

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

### Punching Holes in Intervals

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

### Unions of Non-Overlapping Spans

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

### Subtracting an Interval from Itself

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

## Case Study: Employment History

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
analysis. The main purpose of phinterval is to avoid these workarounds,
by providing drop-in replacements for lubridate interval functions.
Because phinterval functions accept either `<Interval>` or
`<phinterval>` inputs, existing code can typically be adapted by simply
replacing a lubridate function with its phinterval counterpart.

``` r
jobs |>
  mutate(
    span = interval(start, end),
    span = phint_setdiff(span, vacation)
  ) |>
  select(name, job_title, span)
#> # A tibble: 6 × 3
#>   name  job_title                                                        span
#>   <chr> <chr>                                                    <phint<UTC>>
#> 1 Greg  Mascot                                       {2018-01-01--2018-06-03}
#> 2 Greg  Executive Assistant                          {2018-06-10--2020-04-01}
#> 3 Greg  Chief of Staff                               {2020-03-01--2020-11-28}
#> 4 Tom   Chairman                                     {2019-05-01--2020-11-10}
#> 5 Tom   CEO                                          {2020-11-10--2020-12-31}
#> 6 Shiv  Political Consultant {2017-01-01--2017-12-23, 2017-12-29--2019-04-01}
```

### Merging Intervals

Suppose we want to analyze only the total time each character spent
employed, without distinguishing between individual jobs. This can be
done using
[`phint_squash()`](https://ethansansom.github.io/phinterval/reference/phint_squash.md),
which aggregates a vector of intervals into a minimal set of
non-overlapping spans within a scalar `<phinterval>`.

``` r
employment <- jobs |>
  mutate(span = interval(start, end)) |>
  group_by(name) |>
  summarize(employed = phint_squash(span))

employment
#> # A tibble: 3 × 2
#>   name                                          employed
#>   <chr>                                     <phint<UTC>>
#> 1 Greg  {2018-01-01--2018-06-03, 2018-06-10--2020-11-28}
#> 2 Shiv                          {2017-01-01--2019-04-01}
#> 3 Tom                           {2019-05-01--2020-12-31}
```

Notice that:

- *Greg* has multiple disjoint employment periods, which are preserved
  as separate spans within a single `<phinterval>` element.
- *Tom* held two back-to-back positions (Chairman followed by CEO),
  which
  [`phint_squash()`](https://ethansansom.github.io/phinterval/reference/phint_squash.md)
  correctly merges into a single contiguous span.

To return the dataset to a one-row-per-span format, we can extract the
start and end points of each `<phinterval>` element using
[`phint_starts()`](https://ethansansom.github.io/phinterval/reference/phinterval-accessors.md)
and
[`phint_ends()`](https://ethansansom.github.io/phinterval/reference/phinterval-accessors.md).
These functions return lists of start and end dates, which can then be
unnested back into separate rows:

``` r
employment |>
  mutate(
    start = phint_starts(employed), 
    end = phint_ends(employed)
  ) |>
  unnest(cols = c(start, end)) |>
  select(-employed)
#> # A tibble: 4 × 3
#>   name  start               end                
#>   <chr> <dttm>              <dttm>             
#> 1 Greg  2018-01-01 00:00:00 2018-06-03 00:00:00
#> 2 Greg  2018-06-10 00:00:00 2020-11-28 00:00:00
#> 3 Shiv  2017-01-01 00:00:00 2019-04-01 00:00:00
#> 4 Tom   2019-05-01 00:00:00 2020-12-31 00:00:00
```

The resulting tidy format integrates well with
[{dplyr}](https://dplyr.tidyverse.org/) and lubridate workflows.

### Finding Gaps

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
#>   name                unemployed days_unemployed
#>   <chr>             <phint<UTC>>           <dbl>
#> 1 Greg  {2018-06-03--2018-06-10}               7
#> 2 Shiv                    <hole>               0
#> 3 Tom                     <hole>               0
```

Greg was unemployed for 7 days between his time as a Mascot and his role
as Executive Assistant. Tom and Shiv have no gaps within their
respective employment timelines, represented by a `<hole>`.
