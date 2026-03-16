# Keep or discard spans by duration

`phint_sift()` keeps or removes spans within each element of `phint`
based on their duration. At least one of `min_length` or `max_length`
must be non-`NULL`. Duration bounds are inclusive.

When `action = "keep"` (the default):

- `min_length` and `max_length`: keep spans where
  `min_length <= length <= max_length`.

- `min_length` only: keep spans where `length >= min_length`.

- `max_length` only: keep spans where `length <= max_length`.

When `action = "discard"`, these conditions are negated; spans
satisfying the condition are removed rather than kept. The function
[`phint_discard_instants()`](https://ethansansom.github.io/phinterval/reference/phint_discard_instants.md)
is a wrapper around
`phint_sift(phint, max_length = 0L, action = "discard")` for the common
case of removing spans which are 0-seconds in length (i.e.
`start == end`).

Span durations are computed in seconds, equivalent to
[`phint_lengths()`](https://ethansansom.github.io/phinterval/reference/phint_length.md).

## Usage

``` r
phint_sift(
  phint,
  min_length = NULL,
  max_length = NULL,
  action = c("keep", "discard")
)
```

## Arguments

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

- min_length:

  `[Duration / numeric / NULL]`

  The minimum span duration (inclusive). Must be recyclable with
  `phint`. If `NULL` (the default), no lower bound is applied.

- max_length:

  `[Duration / numeric / NULL]`

  The maximum span duration (inclusive). Must be recyclable with
  `phint`. If `NULL` (the default), no upper bound is applied.

- action:

  `["keep" / "discard"]`

  Whether to keep or discard spans satisfying the duration condition:

  - `"keep"` (default): Retain only spans satisfying the condition.

  - `"discard"`: Remove spans satisfying the condition.

## Value

A `<phinterval>` vector the same length as `phint`. Elements where all
spans are removed become
[`hole()`](https://ethansansom.github.io/phinterval/reference/hole.md)s.

## See also

- [`phint_discard_instants()`](https://ethansansom.github.io/phinterval/reference/phint_discard_instants.md)
  to remove zero-duration spans, equivalent to
  `phint_sift(phint, max_length = 0, action = "discard")`.

- [`phint_lengths()`](https://ethansansom.github.io/phinterval/reference/phint_length.md)
  to compute the duration of each span in seconds.

## Examples

``` r
one_day <- interval(as.Date("2025-10-10"), as.Date("2025-10-11"))
two_days <- interval(as.Date("2025-11-12"), as.Date("2025-11-14"))
three_days <- interval(as.Date("2025-12-14"), as.Date("2025-12-17"))

# Keep spans which are at most 2 days long
phint_sift(
  phint_squash(c(one_day, two_days, three_days)),
  min_length = duration(2, "days")
)
#> <phinterval<UTC>[1]>
#> [1] {2025-11-12--2025-11-14, 2025-12-14--2025-12-17}

# Keep spans which are at least 2 days long
phint_sift(
  phint_squash(c(one_day, two_days, three_days)),
  max_length = duration(2, "days")
)
#> <phinterval<UTC>[1]>
#> [1] {2025-10-10--2025-10-11, 2025-11-12--2025-11-14}

# Keep spans with duration in [1.5 days, 2 days]
phint_sift(
  phint_squash(c(one_day, two_days, three_days)),
  min_length = duration(1.5, "days"),
  max_length = duration(2, "days")
)
#> <phinterval<UTC>[1]>
#> [1] {2025-11-12--2025-11-14}

# Discard spans with duration in [1.5 days, 2 days]
phint_sift(
  phint_squash(c(one_day, two_days, three_days)),
  min_length = duration(1.5, "days"),
  max_length = duration(2, "days"),
  action = "keep"
)
#> <phinterval<UTC>[1]>
#> [1] {2025-11-12--2025-11-14}

# All spans removed results in a hole
phint_sift(one_day, max_length = duration(0, "days"))
#> <phinterval<UTC>[1]>
#> [1] <hole>

# Spans within a disjoint element are sifted independently
phint_sift(
  phint_squash(c(two_days, three_days)),
  max_length = duration(2, "days")
)
#> <phinterval<UTC>[1]>
#> [1] {2025-11-12--2025-11-14}

# min_length and max_length are vectorized
phint_sift(
  c(one_day, two_days),
  min_length = duration(c(0, 3), "days")
)
#> Error in funs_cpp$intvl(starts = lubridate::int_start(x), spans = lubridate::int_length(x),     ...): attempt to set index 2/2 in SET_VECTOR_ELT
```
