
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phinterval

<!-- badges: start -->
<!-- badges: end -->

phinterval provides a `<phinterval>` (say “potentially-holey-interval”)
class for working with time intervals that potentially contain gaps such
as:

- An individual’s employment or educational history.

- A meeting schedule for the work day.

- All of the Monday’s in a month.

Sometimes it is useful to consider these holey time-spans as a single
observation rather than a collection of observations. phinterval
provides tools to work with such observations.

The implementation of, and conventions used by, this package borrow
heavily from the [lubridate](https://lubridate.tidyverse.org/)
`<interval>` class. The `<phinterval>` class is meant to be a
super-class of `<interval>`, meaning that the two classes are often
interchangeable.

See my [blog post](https://www.ethansansom.com/posts/2025_01_31_phinterval/) for
details of how the `<phinterval>` vector class can be used to answer common
questions about datetime intervals.

## Installation

⚠️ This package is still **under construction**. ⚠️

You can install the development version of phinterval from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EthanSansom/phinterval")
```
