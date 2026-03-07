# Cumulative set operations

These functions perform cumulative elementwise set operations on
`<phinterval>` vectors, treating each element as a set of
non-overlapping intervals. They return a new `<phinterval>` vector where
each element is the result of applying the corresponding set operation
across all preceding elements.

- `phint_cumunion()` returns the running union of all elements up to and
  including `phint[i]`.

- `phint_cumintersect()` returns the running intersection of all
  elements up to and including `phint[i]`.

## Usage

``` r
phint_cumunion(phint, na_propogate = FALSE)

phint_cumintersect(phint, na_propogate = FALSE, bounds = c("[]", "()"))
```

## Arguments

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

- na_propogate:

  `[FALSE / TRUE]`

  Whether `NA` values propagate forward (or backward, if
  `reverse = TRUE`) through the cumulative result:

  - `FALSE` (default): `NA` elements are treated as
    [`hole()`](https://ethansansom.github.io/phinterval/reference/hole.md)s
    and do not affect subsequent results.

  - `TRUE`: An `NA` element causes all subsequent elements to become
    `NA`.

- bounds:

  `["[]" / "()"]`

  For `phint_cumintersect()`, whether span endpoints are inclusive or
  exclusive:

  - `"[]"` (default): Closed intervals - both endpoints are included

  - `"()"`: Open intervals - both endpoints are excluded

  This affects adjacency and overlap detection. For example, with
  `bounds = "[]"`, the intervals `[1, 5]` and `[5, 10]` are considered
  adjacent (they share the endpoint 5), while with `bounds = "()"`,
  `(1, 5)` and `(5, 10)` are disjoint (neither includes 5).

## Value

A `<phinterval>` vector the same length as `phint`.

## See also

[`phint_union()`](https://ethansansom.github.io/phinterval/reference/phinterval-set-operations.md)
and
[`phint_intersect()`](https://ethansansom.github.io/phinterval/reference/phinterval-set-operations.md)
for the elementwise versions.

## Examples

``` r
monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
tuesday <- interval(as.Date("2025-11-11"), as.Date("2025-11-12"))
wednesday <- interval(as.Date("2025-11-12"), as.Date("2025-11-13"))
mon_to_wed <- interval(as.Date("2025-11-10"), as.Date("2025-11-13"))

# Cumulative union expands with each new element
phint_cumunion(c(monday, tuesday, wednesday))
#> <phinterval<UTC>[3]>
#> [1] {2025-11-10--2025-11-11} {2025-11-10--2025-11-12} {2025-11-10--2025-11-13}

# NA elements are treated as holes by default
phint_cumunion(c(monday, NA, wednesday))
#> <phinterval<UTC>[3]>
#> [1] {2025-11-10--2025-11-11}                        
#> [2] {2025-11-10--2025-11-11}                        
#> [3] {2025-11-10--2025-11-11, 2025-11-12--2025-11-13}

# NA elements propagate forward with na_propogate = TRUE
phint_cumunion(c(monday, NA, wednesday), na_propogate = TRUE)
#> <phinterval<UTC>[3]>
#> [1] {2025-11-10--2025-11-11} <NA>                     <NA>                    

# Cumulative intersection narrows with each new element
phint_cumintersect(c(mon_to_wed, monday, tuesday))
#> <phinterval<UTC>[3]>
#> [1] {2025-11-10--2025-11-13} {2025-11-10--2025-11-11} {2025-11-11--2025-11-11}

# Once the intersection becomes a hole, it remains a hole
phint_cumintersect(c(monday, tuesday, wednesday))
#> <phinterval<UTC>[3]>
#> [1] {2025-11-10--2025-11-11} {2025-11-11--2025-11-11} <hole>                  

# Bounds affect the intersection of adjacent intervals
phint_cumintersect(c(monday, tuesday), bounds = "[]")
#> <phinterval<UTC>[2]>
#> [1] {2025-11-10--2025-11-11} {2025-11-11--2025-11-11}
phint_cumintersect(c(monday, tuesday), bounds = "()")
#> <phinterval<UTC>[2]>
#> [1] {2025-11-10--2025-11-11} <hole>                  
```
