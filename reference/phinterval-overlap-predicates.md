# Test whether a phinterval vector has cross-element overlaps

`phint_has_overlaps()` returns a logical vector indicating which
elements of `phint` would be modified by
[`phint_unoverlap()`](https://ethansansom.github.io/phinterval/reference/phint_unoverlap.md),
i.e. elements which overlap with at least one preceding element (or
lower-priority group element). Blockers, i.e. preceding elements which
overlap with a following element, are not marked as overlapping.
`phint_any_overlaps()` is a fast scalar equivalent to
`any(phint_has_overlaps(...), na.rm = TRUE)`.

Both functions accept the same arguments as
[`phint_unoverlap()`](https://ethansansom.github.io/phinterval/reference/phint_unoverlap.md)
and use the same priority and within-priority resolution rules.

The following invariants hold:

    # phint_unoverlap() ensures that phint_has_overlaps() is FALSE
    phint <- phint_unoverlap(phint, ...)
    !phint_any_overlaps(phint, ...)

    # phint_unoverlap() does not alter non-overlapping elements
    overlapping <- phint_has_overlaps(phint, ...)
    all(phint[!overlapping] == phint_unoverlap(phint, ...)[!overlapping], na.rm = TRUE)

    # phint_any_overlaps() is equivalent to any(phint_has_overlaps(...))
    phint_any_overlaps(phint, ...) == any(phint_has_overlaps(phint, ...), na.rm = TRUE)

## Usage

``` r
phint_has_overlaps(
  phint,
  priority = NULL,
  priority_order = c("asc", "desc", "appearance"),
  within_priority = c("sequential", "keep"),
  na_propagate = FALSE
)

phint_any_overlaps(
  phint,
  priority = NULL,
  priority_order = c("asc", "desc", "appearance"),
  within_priority = c("sequential", "keep"),
  na_propagate = FALSE
)
```

## Arguments

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

- priority:

  `[vector / NULL]`

  An optional grouping vector defining priority groups. Earlier groups
  (per `priority_order`) are processed first and block later groups.
  Must be recyclable with `phint`. If `NULL` (the default), all elements
  are resolved considered to be within the same group.

  `priority` may be any vector in the vctrs sense. See
  [`vctrs::obj_is_vector()`](https://vctrs.r-lib.org/reference/vector-checks.html)
  for details.

- priority_order:

  `["asc" / "desc" / "appearance"]`

  How to order priority groups for processing:

  - `"asc"` (default): Lower values are processed first (priority 1
    before 2).

  - `"desc"`: Higher values are processed first (priority 9 before 2).

  - `"appearance"`: Groups are processed in order of first appearance in
    `priority`.

- within_priority:

  `["sequential" / "keep"]`

  How to handle overlaps within the same priority group:

  - `"sequential"` (default): Overlaps within a group are resolved by
    row order, so earlier elements block later elements within the same
    group.

  - `"keep"`: Overlaps within a group are preserved; only overlaps with
    higher-priority groups are removed.

- na_propagate:

  `[FALSE / TRUE]`

  Whether `NA` elements propagate to subsequent elements:

  - `FALSE` (default): `NA` elements are left as-is and do not affect
    subsequent results.

  - `TRUE`: An `NA` element causes all subsequent elements (or
    lower-priority group elements) to become `NA`.

## Value

`phint_has_overlaps()` returns a logical vector the same length as
`phint`:

- `TRUE`: the element overlaps with at least one preceding element and
  would be modified by
  [`phint_unoverlap()`](https://ethansansom.github.io/phinterval/reference/phint_unoverlap.md).

- `FALSE`: the element does not overlap with any preceding element and
  would not be affected by
  [`phint_unoverlap()`](https://ethansansom.github.io/phinterval/reference/phint_unoverlap.md).
  This includes `NA` elements when `na_propagate = FALSE`.

- `NA`: the element is itself `NA` or would become `NA` due to
  propagation. This is only applicable when `na_propagate = TRUE`.

`phint_any_overlaps()` returns a single `TRUE` or `FALSE`.

## See also

- [`phint_unoverlap()`](https://ethansansom.github.io/phinterval/reference/phint_unoverlap.md)
  to resolve the overlaps detected by this function.

## Examples

``` r
monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
tuesday <- interval(as.Date("2025-11-11"), as.Date("2025-11-12"))
wednesday <- interval(as.Date("2025-11-12"), as.Date("2025-11-13"))
mon_to_wed <- interval(as.Date("2025-11-10"), as.Date("2025-11-13"))
mon_to_tue <- interval(as.Date("2025-11-10"), as.Date("2025-11-12"))

# No overlaps: all FALSE
phint_has_overlaps(c(monday, tuesday, wednesday))
#> [1] FALSE FALSE FALSE
phint_any_overlaps(c(monday, tuesday, wednesday))
#> [1] FALSE

# Only the blocked element is TRUE, not the blocker
phint_has_overlaps(c(mon_to_wed, mon_to_tue))
#> [1] FALSE  TRUE
phint_any_overlaps(c(mon_to_wed, mon_to_tue))
#> [1] TRUE

# Non-overlapping elements are FALSE even when others overlap
phint_has_overlaps(c(mon_to_wed, mon_to_tue, wednesday))
#> [1] FALSE  TRUE  TRUE

# Priority-based: same rules as phint_unoverlap()
phint_has_overlaps(
  c(mon_to_wed, mon_to_tue, wednesday),
  priority = c(1, 2, 1)
)
#> [1] FALSE  TRUE  TRUE

# NA elements return NA
phint_has_overlaps(c(mon_to_wed, NA, wednesday))
#> [1] FALSE FALSE  TRUE

# na_propagate = TRUE: NA propagates forward
phint_has_overlaps(c(monday, NA, wednesday), na_propagate = TRUE)
#> [1] FALSE    NA    NA
phint_any_overlaps(c(monday, NA, wednesday), na_propagate = TRUE)
#> [1] FALSE
```
