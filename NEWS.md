# phinterval (development version)

## Features:

* New `phint_cumunion()`, `phint_cumintersect()`, `phint_symmetric_setdiff()` to complete the family of set-operations.

  *  `phint_cumunion()` and `phint_cumintersect()` take the cumulative union and intersection of a phinterval respectively.
  
  * `phint_symmetric_setdiff()` takes the symmetric set-difference of two phintervals.
  
* New `phint_flatten()` and `datetime_flatten()` return all spans or gaps within an entire phinterval vector or datetime spans as a vector of intervals.

## Bug fixes:

* `phint_setdiff()` no longer creates a malformed phinterval when `phint2` contains instants (#3).

* `phint_invert()` no longer creates a malformed phinterval when `phint` contains instants (#9).

## Breaking changes:

* `phint_unnest()` now always returns a dataframe with columns `key`, `start`, `end` and `size`.

The `keep_size` argument has been removed from `phint_unnest()` and the function
signature has been revised:

```
# Old Usage
phint_unnest(phint, hole_to = c("drop", "na"), keep_size = FALSE, key = NULL)

# New Usage
phint_unnest(phint, key = NULL, hole_to = c("na", "drop"))
```

# phinterval 1.0.0

* Initial CRAN submission.
