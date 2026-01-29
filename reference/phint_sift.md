# Remove instantaneous time spans from a phinterval

`phint_sift()` removes instantaneous spans (spans with 0 duration) from
phinterval elements. If all spans in an element are instantaneous, the
result is a hole.

## Usage

``` r
phint_sift(phint)
```

## Arguments

- phint:

  `[phinterval / Interval]`

  A `<phinterval>` or `<Interval>` vector.

## Value

A `<phinterval>` vector the same length as `phint`.

## Examples

``` r
y2020 <- interval(as.Date("2020-01-01"), as.Date("2021-01-01"))
y2021 <- interval(as.Date("2021-01-01"), as.Date("2022-01-01"))
y2022 <- interval(as.Date("2022-01-01"), as.Date("2023-01-01"))

# The intersection of two adjacent intervals is instantaneous
new_years_2021 <- phint_intersect(y2020, y2021)
new_years_2021
#> <phinterval<UTC>[1]>
#> [1] {2021-01-01--2021-01-01}
phint_sift(new_years_2021)
#> <phinterval<UTC>[1]>
#> [1] <hole>

# phint_sift() removes instants while keeping non-instantaneous spans
y2022_and_new_years <- phint_union(y2022, new_years_2021)
y2022_and_new_years
#> <phinterval<UTC>[1]>
#> [1] {2021-01-01--2021-01-01, 2022-01-01--2023-01-01}
phint_sift(y2022_and_new_years)
#> <phinterval<UTC>[1]>
#> [1] {2022-01-01--2023-01-01}
```
