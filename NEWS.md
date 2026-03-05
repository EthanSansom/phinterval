# phinterval (development version)

# phinterval 1.0.0.9000

## Features:

* New `phint_cumunion()`, `phint_cumintersect()`, `phint_symmetric_setdiff()` to complete the family of set-operations.

  *  `phint_cumunion()` and `phint_cumintersect()` take the cumulative union and intersection of a phinterval respectively.
  
  * `phint_symmetric_setdiff()` takes the symmetric set-difference of two phintervals.
  
* New `phint_flatten()` returns all spans or gaps within an entire phinterval vector as a vector of intervals.

## Bug fixes:

* `phint_setdiff()` no longer creates a malformed phinterval when `phint2` contains instants (#3).

# phinterval 1.0.0

* Initial CRAN submission.
