
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phinterval

<!-- badges: start -->
<!-- badges: end -->

{phinterval} is a package for representing and manipulating time spans
that may contain gaps. It implements the `<phinterval>` (think
“potentially-holey-interval”) vector class, designed as an extension of
the [{lubridate}](https://lubridate.tidyverse.org/) `<Interval>`, to
represent continuous, disjoint, empty, and unknown spans of time.

Functionality for manipulating these spans includes:

- Performing set operations: union, intersection, difference, and
  complement.
- Merging overlapping or adjacent intervals into non-overlapping sets of
  time spans.
- Testing whether time spans, dates, or times fall within one another or
  overlap.

## Installation

You can install the development version of {phinterval} from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("EthanSansom/phinterval")
```
