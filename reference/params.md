# Parameter Descriptions for phinterval Package

Default parameter descriptions which may be overridden in individual
functions.

## Arguments

- phint1, phint2:

  `[phinterval / Interval]`

  A pair of `<phinterval>` or `<Interval>` vectors. `phint1` and
  `phint2` are recycled to a common length using vctrs-style recycling.

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

- bounds:

  `["[]" / "()"]`

  Whether span endpoints are inclusive or exclusive:

  - `"[]"` (default): Closed intervals - both endpoints are included

  - `"()"`: Open intervals - both endpoints are excluded

  This affects adjacency and overlap detection. For example, with
  `bounds = "[]"`, the intervals `[1, 5]` and `[5, 10]` are considered
  adjacent (they share the endpoint 5), while with `bounds = "()"`,
  `(1, 5)` and `(5, 10)` are disjoint (neither includes 5).

## Value

The value `NULL`.
