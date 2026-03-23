# Resolve overlapping intervals sequentially or by priority

`phint_unoverlap()` removes overlaps across elements of a `<phinterval>`
vector by trimming each element against all preceding elements. The
result is a vector where no two elements share any time.

Without `priority`, each element is trimmed by the union of all previous
elements:

    result[i] = phint_setdiff(phint[i], phint_squash(phint[1:(i - 1)]))

With `priority`, elements are grouped and processed in `priority_order`.
Each element is trimmed by all elements from earlier priority groups.
Within a priority group, `within_priority` controls whether overlapping
elements within each group are kept as-is or trimmed.

## Usage

``` r
phint_unoverlap(
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

A `<phinterval>` vector the same length as `phint`, where no two
elements overlap.

## See also

- [`phint_has_overlaps()`](https://ethansansom.github.io/phinterval/reference/phinterval-overlap-predicates.md)
  to test whether a `<phinterval>` vector has cross-element overlaps.

## Examples

``` r
monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
tuesday <- interval(as.Date("2025-11-11"), as.Date("2025-11-12"))
wednesday <- interval(as.Date("2025-11-12"), as.Date("2025-11-13"))
mon_to_wed <- interval(as.Date("2025-11-10"), as.Date("2025-11-13"))
mon_to_tue <- interval(as.Date("2025-11-10"), as.Date("2025-11-12"))

# Sequential removal: each element is trimmed by all previous elements
phint_unoverlap(c(wednesday, mon_to_wed, mon_to_tue))
#> <phinterval<UTC>[3]>
#> [1] {2025-11-12--2025-11-13} {2025-11-10--2025-11-12} <hole>                  

# Priority-based: lower priority values are processed first
phint_unoverlap(
  c(mon_to_wed, mon_to_tue, wednesday),
  priority = c(1, 2, 1)
)
#> <phinterval<UTC>[3]>
#> [1] {2025-11-10--2025-11-13} <hole>                   <hole>                  

# within_priority = "keep": overlaps within a group are preserved
phint_unoverlap(
  c(mon_to_wed, mon_to_tue, wednesday),
  priority = c(1, 1, 2),
  within_priority = "keep"
)
#> <phinterval<UTC>[3]>
#> [1] {2025-11-10--2025-11-13} {2025-11-10--2025-11-12} <hole>                  

# priority_order = "desc": higher priority values are processed first
phint_unoverlap(
  c(mon_to_wed, mon_to_tue, wednesday),
  priority = c(1, 2, 1),
  priority_order = "desc"
)
#> <phinterval<UTC>[3]>
#> [1] {2025-11-12--2025-11-13} {2025-11-10--2025-11-12} <hole>                  

# NA elements are treated as ignored by default
phint_unoverlap(c(mon_to_wed, NA, wednesday))
#> <phinterval<UTC>[3]>
#> [1] {2025-11-10--2025-11-13} <NA>                     <hole>                  

# NA elements propagate forward with na_propagate = TRUE
phint_unoverlap(c(mon_to_wed, NA, wednesday), na_propagate = TRUE)
#> <phinterval<UTC>[3]>
#> [1] {2025-11-10--2025-11-13} <NA>                     <NA>                    
```
