# Changelog

## phinterval (development version)

## phinterval 1.0.0.9000

### Features:

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
  [`phint_flatten()`](https://ethansansom.github.io/phinterval/reference/phint_flatten.md)
  returns all spans or gaps within an entire phinterval vector as a
  vector of intervals.

### Bug fixes:

- [`phint_setdiff()`](https://ethansansom.github.io/phinterval/reference/phinterval-set-operations.md)
  no longer creates a malformed phinterval when `phint2` contains
  instants ([\#3](https://github.com/EthanSansom/phinterval/issues/3)).

## phinterval 1.0.0

CRAN release: 2026-02-03

- Initial CRAN submission.
