# Changelog

## phinterval (development version)

### Features

- New
  [`phint_unoverlap()`](https://ethansansom.github.io/phinterval/reference/phint_unoverlap.md)
  resolves overlaps across elements of a `<phinterval>` vector by
  trimming each element against all preceding elements.

  - An optional `priority` argument groups elements by priority, so that
    lower-priority elements are trimmed against higher-priority elements
    instead of by row-order.

  - `priority_order` controls whether lower (`"asc"`) or higher
    (`"desc"`) priority values are processed first, or whether groups
    are processed in order of first appearance (`"appearance"`).

  - `within_priority` controls whether overlaps within the same priority
    group are resolved sequentially (`"sequential"`) or left as-is
    (`"keep"`).

  - `na_propagate` controls whether `NA` elements propagate to
    subsequent elements or lower-priority groups.

- New
  [`phint_has_overlaps()`](https://ethansansom.github.io/phinterval/reference/phinterval-overlap-predicates.md)
  returns a logical vector indicating which elements of a `<phinterval>`
  vector overlap with a preceding element. Accepts the same arguments as
  [`phint_unoverlap()`](https://ethansansom.github.io/phinterval/reference/phint_unoverlap.md).

  - New
    [`phint_any_overlaps()`](https://ethansansom.github.io/phinterval/reference/phinterval-overlap-predicates.md)
    is a fast scalar equivalent to
    `any(phint_has_overlaps(...), na.rm = TRUE)`.

- New
  [`phint_cumunion()`](https://ethansansom.github.io/phinterval/reference/phinterval-cumset-operations.md),
  [`phint_cumintersect()`](https://ethansansom.github.io/phinterval/reference/phinterval-cumset-operations.md),
  [`phint_symmetric_setdiff()`](https://ethansansom.github.io/phinterval/reference/phinterval-set-operations.md)
  to complete the family of set-operations.

  - [`phint_cumunion()`](https://ethansansom.github.io/phinterval/reference/phinterval-cumset-operations.md)
    and
    [`phint_cumintersect()`](https://ethansansom.github.io/phinterval/reference/phinterval-cumset-operations.md)
    take the cumulative union and intersection of a phinterval
    respectively.

  - [`phint_symmetric_setdiff()`](https://ethansansom.github.io/phinterval/reference/phinterval-set-operations.md)
    takes the symmetric set-difference of two phintervals.

- New
  [`phint_flatten()`](https://ethansansom.github.io/phinterval/reference/flatten.md)
  returns all spans or gaps within an entire phinterval vector as a
  vector of intervals.

  - New
    [`datetime_flatten()`](https://ethansansom.github.io/phinterval/reference/flatten.md)
    returns spans or gaps within a vector of spans defined by `start`
    and `end` points (see also
    [`datetime_squash()`](https://ethansansom.github.io/phinterval/reference/squash.md)).

- New
  [`is_span()`](https://ethansansom.github.io/phinterval/reference/phinterval-span-predicates.md)
  and
  [`is_disjoint()`](https://ethansansom.github.io/phinterval/reference/phinterval-span-predicates.md)
  to complement
  [`is_hole()`](https://ethansansom.github.io/phinterval/reference/phinterval-span-predicates.md).

  - [`is_span()`](https://ethansansom.github.io/phinterval/reference/phinterval-span-predicates.md)
    and
    [`is_disjoint()`](https://ethansansom.github.io/phinterval/reference/phinterval-span-predicates.md)
    test whether each element of a phinterval is contiguous or disjoint
    respectively.

- [`phint_sift()`](https://ethansansom.github.io/phinterval/reference/phint_sift.md)
  now keeps or discards spans with lengths in
  `[min_length, max_length]`, instead of discarding instantaneous spans
  only.

  - The previous version of
    [`phint_sift()`](https://ethansansom.github.io/phinterval/reference/phint_sift.md)
    was re-named to
    [`phint_discard_instants()`](https://ethansansom.github.io/phinterval/reference/phint_discard_instants.md).

### Bug fixes

- [`phint_setdiff()`](https://ethansansom.github.io/phinterval/reference/phinterval-set-operations.md)
  no longer returns a malformed phinterval when `phint2` contains
  instants ([\#3](https://github.com/EthanSansom/phinterval/issues/3)).

- [`phint_invert()`](https://ethansansom.github.io/phinterval/reference/phint_invert.md)
  no longer returns a malformed phinterval when `phint` contains
  instants ([\#9](https://github.com/EthanSansom/phinterval/issues/9)).

### Breaking changes

- [`phint_unnest()`](https://ethansansom.github.io/phinterval/reference/phint_unnest.md)
  now always returns a dataframe with columns `key`, `start`, `end`, and
  `size`, instead of optionally including a `size` column.

The `keep_size` argument has been removed from
[`phint_unnest()`](https://ethansansom.github.io/phinterval/reference/phint_unnest.md)
and the function signature has been revised:

    # Old Usage
    phint_unnest(phint, hole_to = c("drop", "na"), keep_size = FALSE, key = NULL)

    # New Usage
    phint_unnest(phint, key = NULL, hole_to = c("na", "drop"))

- [`phint_squash()`](https://ethansansom.github.io/phinterval/reference/squash.md)
  and
  [`datetime_squash()`](https://ethansansom.github.io/phinterval/reference/squash.md)
  have been split into two functions
  ([\#13](https://github.com/EthanSansom/phinterval/issues/13)).

  - [`phint_squash()`](https://ethansansom.github.io/phinterval/reference/squash.md)
    and
    [`datetime_squash()`](https://ethansansom.github.io/phinterval/reference/squash.md)
    now always squash intervals into a length-1 `<phinterval>`.

  - [`phint_squash_by()`](https://ethansansom.github.io/phinterval/reference/squash.md)
    and
    [`datetime_squash_by()`](https://ethansansom.github.io/phinterval/reference/squash.md)
    squash intervals within groups defined by the `by` argument.

  - The `order_by` argument of
    [`phint_squash_by()`](https://ethansansom.github.io/phinterval/reference/squash.md)
    and
    [`datetime_squash_by()`](https://ethansansom.github.io/phinterval/reference/squash.md)
    now defaults to `TRUE`, to match the behavior of
    [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

  - The `na.rm` argument of the squash functions has been renamed to
    `na_rm`
    ([\#19](https://github.com/EthanSansom/phinterval/issues/19)).

The return type of the `*_squash()` variants is now always a scalar
`<phinterval>` vector, meaning it is safe to use within
[`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html).
This includes cases where the `phint` argument of
[`phint_squash()`](https://ethansansom.github.io/phinterval/reference/squash.md)
or the `start, end` arguments of
[`datetime_squash()`](https://ethansansom.github.io/phinterval/reference/squash.md)
are empty as the `empty_to = "empty"` option has been removed from the
`empty_to` argument.

The return type of the `*_squash_by()` variants is now always a
[`tibble()`](https://tibble.tidyverse.org/reference/tibble.html) with
columns `by` and `phint`. Additionally, the `keep_by` argument has been
removed from these variants.

    # Old Usage
    phint_squash(
      phint,
      by = NULL,
      na.rm = TRUE,
      empty_to = c("hole", "na", "empty"),
      order_by = FALSE,
      keep_by = FALSE
    )

    # New Usage
    phint_squash(
      phint,
      na_rm = TRUE,
      empty_to = c("hole", "na")
    )

    phint_squash_by(
      phint,
      by,
      na_rm = TRUE,
      empty_to = c("hole", "na"),
      order_by = TRUE
    )

- [`phint_sift()`](https://ethansansom.github.io/phinterval/reference/phint_sift.md)
  now has additional arguments `min_length`, `max_length`, and `action`.

  - The previous version of
    [`phint_sift()`](https://ethansansom.github.io/phinterval/reference/phint_sift.md)
    (usage `phint_sift(phint)`) was re-named to
    [`phint_discard_instants()`](https://ethansansom.github.io/phinterval/reference/phint_discard_instants.md).

  - Calling `phint_sift(phint)` without additional arguments now raises
    an error.

## phinterval 1.0.0

CRAN release: 2026-02-03

- Initial CRAN submission.
