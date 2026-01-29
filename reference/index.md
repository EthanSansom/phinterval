# Package index

## Construction and Casting

- [`phinterval()`](https://ethansansom.github.io/phinterval/reference/phinterval.md)
  : Create a new phinterval
- [`hole()`](https://ethansansom.github.io/phinterval/reference/hole.md)
  : Create a hole phinterval
- [`as_phinterval()`](https://ethansansom.github.io/phinterval/reference/as_phinterval.md)
  : Convert an interval or datetime vector into a phinterval
- [`as_duration()`](https://ethansansom.github.io/phinterval/reference/as_duration.md)
  : Convert a phinterval to a duration

## Set Operations and Modifications

- [`phint_complement()`](https://ethansansom.github.io/phinterval/reference/phinterval-set-operations.md)
  [`phint_union()`](https://ethansansom.github.io/phinterval/reference/phinterval-set-operations.md)
  [`phint_intersect()`](https://ethansansom.github.io/phinterval/reference/phinterval-set-operations.md)
  [`phint_setdiff()`](https://ethansansom.github.io/phinterval/reference/phinterval-set-operations.md)
  : Vectorized set operations
- [`phint_invert()`](https://ethansansom.github.io/phinterval/reference/phint_invert.md)
  : Get the gaps in a phinterval as time spans
- [`phint_sift()`](https://ethansansom.github.io/phinterval/reference/phint_sift.md)
  : Remove instantaneous time spans from a phinterval
- [`phint_squash()`](https://ethansansom.github.io/phinterval/reference/squash.md)
  [`datetime_squash()`](https://ethansansom.github.io/phinterval/reference/squash.md)
  : Squash overlapping intervals into non-overlapping spans

## Relations and Predicates

- [`phint_overlaps()`](https://ethansansom.github.io/phinterval/reference/phint_overlaps.md)
  : Test whether two phintervals overlap
- [`phint_within()`](https://ethansansom.github.io/phinterval/reference/phint_within.md)
  : Test whether a datetime or phinterval is within another phinterval
- [`is_hole()`](https://ethansansom.github.io/phinterval/reference/is_hole.md)
  : Test for empty intervals
- [`is_phinterval()`](https://ethansansom.github.io/phinterval/reference/is_phinterval.md)
  : Test if the object is a phinterval
- [`is_phintish()`](https://ethansansom.github.io/phinterval/reference/is_phintish.md)
  : Test if the object is a phinterval or interval

## Accessors

- [`phint_unnest()`](https://ethansansom.github.io/phinterval/reference/phint_unnest.md)
  : Unnest a phinterval into a data frame
- [`phint_start()`](https://ethansansom.github.io/phinterval/reference/phinterval-accessors.md)
  [`phint_end()`](https://ethansansom.github.io/phinterval/reference/phinterval-accessors.md)
  [`phint_starts()`](https://ethansansom.github.io/phinterval/reference/phinterval-accessors.md)
  [`phint_ends()`](https://ethansansom.github.io/phinterval/reference/phinterval-accessors.md)
  : Accessors for the endpoints of a phinterval
- [`phint_length()`](https://ethansansom.github.io/phinterval/reference/phint_length.md)
  [`phint_lengths()`](https://ethansansom.github.io/phinterval/reference/phint_length.md)
  : Compute the length of a phinterval in seconds
- [`n_spans()`](https://ethansansom.github.io/phinterval/reference/n_spans.md)
  : Count the number of spans in a phinterval

## Time Zone

- [`is_recognized_tzone()`](https://ethansansom.github.io/phinterval/reference/is_recognized_tzone.md)
  : Test if the object is a recognized time zone

## Package Options

- [`phinterval_options`](https://ethansansom.github.io/phinterval/reference/phinterval_options.md)
  : Package options

## Re-Exports

- [`reexports`](https://ethansansom.github.io/phinterval/reference/reexports.md)
  [`interval`](https://ethansansom.github.io/phinterval/reference/reexports.md)
  [`duration`](https://ethansansom.github.io/phinterval/reference/reexports.md)
  [`POSIXct`](https://ethansansom.github.io/phinterval/reference/reexports.md)
  [`NA_POSIXct_`](https://ethansansom.github.io/phinterval/reference/reexports.md)
  [`tzdb_names`](https://ethansansom.github.io/phinterval/reference/reexports.md)
  [`tzdb_version`](https://ethansansom.github.io/phinterval/reference/reexports.md)
  : Objects exported from other packages
