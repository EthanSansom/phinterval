# Flatten a phinterval

`phint_squash()` combines all elements of a `<phinterval>` vector into a
single set of non-overlapping, non-adjacent time spans, returned as a
scalar `<phinterval>` by default.

This is useful in conjunction with aggregation functions, e.g.
[`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html).

## Usage

``` r
phint_squash(phint, na.rm = TRUE, empty_to = c("na", "hole", "empty"))
```

## Arguments

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

- na.rm:

  `[TRUE / FALSE]`

  Whether to ignore `NA` elements when flattening a `<phinterval>`.

  If `na.rm = FALSE` and `phint` contains any `NA` elements, then a
  scalar `NA` `<phinterval>` will be returned.

- empty_to:

  What to return if the input `phint` is length 0.

  - If `empty_to = "na"` (the default), a length 1 `NA` `<phinterval>`
    is returned.

  - If `"hole"` a single `<hole>` is returned.

  - If `"empty"` a length 0 `<phinterval>` vector is returned.

## Value

A length 1 `<phinterval>` vector, with one exception.

If the input is an empty `<phinterval>` or `<Interval>` vector *and*
`empty_to = "empty"`, then the result is a length 0 `<phinterval>`
vector.

## Examples

``` r
jan_1_to_5   <- interval(as.Date("2000-01-01"), as.Date("2000-01-05"))
jan_3_to_9   <- interval(as.Date("2000-01-03"), as.Date("2000-01-09"))
jan_11_to_12 <- interval(as.Date("2000-01-11"), as.Date("2000-01-12"))

phint_squash(c(jan_1_to_5, jan_3_to_9, jan_11_to_12, NA))
#> <phinterval<UTC>[1]>
#> [1] {2000-01-01--2000-01-09, 2000-01-11--2000-01-12}

# Set `na.rm = FALSE` to propagate `NA` values
phint_squash(c(jan_1_to_5, jan_3_to_9, jan_11_to_12, NA), na.rm = FALSE)
#> <phinterval<UTC>[1]>
#> [1] <NA>

# `empty_to` determines the result of empty inputs
empty <- interval()
phint_squash(empty, empty_to = "na")
#> <phinterval<UTC>[1]>
#> [1] <NA>
phint_squash(empty, empty_to = "hole")
#> <phinterval<UTC>[1]>
#> [1] <hole>
phint_squash(empty, empty_to = "empty")
#> <phinterval<UTC>[0]>
```
