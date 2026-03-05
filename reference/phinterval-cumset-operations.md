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
phint_cumunion(phint, na_propogate = FALSE, reverse = FALSE)

phint_cumintersect(phint, na_propogate = FALSE, reverse = FALSE)
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

- reverse:

  `[FALSE / TRUE]`

  Whether to accumulate from right to left instead of left to right:

  - `FALSE` (default): Each element `result[i]` is the cumulative set
    operation over `phint[1:i]`.

  - `TRUE`: Each element `result[i]` is the cumulative set operation
    over `phint[i:length(phint)]`.

## Value

A `<phinterval>` vector the same length as `phint`.

## Examples

``` r
monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
tuesday <- interval(as.Date("2025-11-11"), as.Date("2025-11-12"))
wednesday <- interval(as.Date("2025-11-12"), as.Date("2025-11-13"))
mon_to_wed <- interval(as.Date("2025-11-10"), as.Date("2025-11-13"))
jan_1_to_5 <- interval(as.Date("2000-01-01"), as.Date("2000-01-05"))
jan_3_to_9 <- interval(as.Date("2000-01-03"), as.Date("2000-01-09"))
jan_6_to_9 <- interval(as.Date("2000-01-06"), as.Date("2000-01-09"))

# Cumulative union expands with each new element
phint_cumunion(c(monday, tuesday, wednesday))
#> <phinterval<UTC>[3]>
#> [1] {2025-11-10--2025-11-11} {2025-11-10--2025-11-12} {2025-11-10--2025-11-13}

# Accumulate from right to left
phint_cumunion(c(monday, tuesday, wednesday), reverse = TRUE)
#> <phinterval<UTC>[3]>
#> [1] {2025-11-10--2025-11-13} {2025-11-11--2025-11-13} {2025-11-12--2025-11-13}

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
phint_cumintersect(c(mon_to_wed, jan_1_to_5, jan_3_to_9))
#> <phinterval<UTC>[3]>
#> [1] {2025-11-10--2025-11-13} <hole>                   <hole>                  

# Once the intersection becomes a hole, it remains a hole
phint_cumintersect(c(monday, tuesday, wednesday))
#> <phinterval<UTC>[3]>
#> [1] {2025-11-10--2025-11-11} {2025-11-11--2025-11-11} <hole>                  

# Accumulate from right to left
phint_cumintersect(c(monday, tuesday, wednesday), reverse = TRUE)
#> <phinterval<UTC>[3]>
#> [1] <hole>                   {2025-11-12--2025-11-12} {2025-11-12--2025-11-13}

# NA elements are treated as holes by default
phint_cumintersect(c(jan_1_to_5, NA, jan_3_to_9))
#> <phinterval<UTC>[3]>
#> [1] {2000-01-01--2000-01-05} <hole>                   <hole>                  

# NA elements propagate forward with na_propogate = TRUE
phint_cumintersect(c(jan_1_to_5, NA, jan_3_to_9), na_propogate = TRUE)
#> <phinterval<UTC>[3]>
#> [1] {2000-01-01--2000-01-05} <NA>                     <NA>                    
```
