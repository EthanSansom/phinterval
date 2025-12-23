# Package index

## Construction and Coercion

- [`phinterval()`](https://ethansansom.github.io/phinterval/reference/phinterval.md)
  : Create a new phinterval
- [`reexports`](https://ethansansom.github.io/phinterval/reference/reexports.md)
  [`interval`](https://ethansansom.github.io/phinterval/reference/reexports.md)
  [`duration`](https://ethansansom.github.io/phinterval/reference/reexports.md)
  : Objects exported from other packages
- [`as_phinterval()`](https://ethansansom.github.io/phinterval/reference/as_phinterval.md)
  : Convert an interval vector into a phinterval
- [`as_duration()`](https://ethansansom.github.io/phinterval/reference/as_duration.md)
  : Convert a phinterval to a duration

## Set Operations

- [`phint_complement()`](https://ethansansom.github.io/phinterval/reference/phinterval-set-operations.md)
  [`phint_union()`](https://ethansansom.github.io/phinterval/reference/phinterval-set-operations.md)
  [`phint_intersect()`](https://ethansansom.github.io/phinterval/reference/phinterval-set-operations.md)
  [`phint_setdiff()`](https://ethansansom.github.io/phinterval/reference/phinterval-set-operations.md)
  : Vectorized set operations

## Other Operations

- [`phint_squash()`](https://ethansansom.github.io/phinterval/reference/phint_squash.md)
  : Flatten a phinterval
- [`phint_invert()`](https://ethansansom.github.io/phinterval/reference/phint_invert.md)
  : Get the gaps in a phinterval as time spans
- [`phint_sift()`](https://ethansansom.github.io/phinterval/reference/phint_sift.md)
  : Remove instantaneous time spans from a phinterval

## Predicates and Comparisons

- [`phint_overlaps()`](https://ethansansom.github.io/phinterval/reference/phint_overlaps.md)
  : Test whether two phintervals overlap
- [`phint_within()`](https://ethansansom.github.io/phinterval/reference/phint_within.md)
  : Test whether a date, time, or phinterval is within another
  phinterval
- [`is_hole()`](https://ethansansom.github.io/phinterval/reference/is_hole.md)
  : Test for empty intervals
- [`n_spans()`](https://ethansansom.github.io/phinterval/reference/n_spans.md)
  : Count the number of spans in a phinterval
- [`is_phinterval()`](https://ethansansom.github.io/phinterval/reference/is_phinterval.md)
  : Test if the object is a phinterval
- [`is_phintish()`](https://ethansansom.github.io/phinterval/reference/is_phintish.md)
  : Test if the object is a phinterval or interval

## Accessors

- [`phint_start()`](https://ethansansom.github.io/phinterval/reference/phinterval-accessors.md)
  [`phint_end()`](https://ethansansom.github.io/phinterval/reference/phinterval-accessors.md)
  [`phint_starts()`](https://ethansansom.github.io/phinterval/reference/phinterval-accessors.md)
  [`phint_ends()`](https://ethansansom.github.io/phinterval/reference/phinterval-accessors.md)
  : Accessors for the endpoints of a phinterval
- [`phint_to_spans()`](https://ethansansom.github.io/phinterval/reference/phint_to_spans.md)
  : Convert a phinterval into a list of intervals
- [`phint_length()`](https://ethansansom.github.io/phinterval/reference/phint_length.md)
  [`phint_lengths()`](https://ethansansom.github.io/phinterval/reference/phint_length.md)
  : Compute the length of a phinterval in seconds

## Package Options

- [`phinterval_options`](https://ethansansom.github.io/phinterval/reference/phinterval_options.md)
  : Package options
