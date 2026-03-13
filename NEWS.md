# phinterval (development version)

## Features

* New `phint_cumunion()`, `phint_cumintersect()`, `phint_symmetric_setdiff()` to complete the family of set-operations.

  *  `phint_cumunion()` and `phint_cumintersect()` take the cumulative union and intersection of a phinterval respectively.
  
  * `phint_symmetric_setdiff()` takes the symmetric set-difference of two phintervals.
  
* New `phint_flatten()` returns all spans or gaps within an entire phinterval vector as a vector of intervals.

  * New `datetime_flatten()` returns spans or gaps within a vector of spans defined by `start` and `end` points (see also `datetime_squash()`).

* New `is_span()` and `is_disjoint()` to complement `is_hole()`.

  * `is_span()` and `is_disjoint()` test whether each element of a phinterval is contiguous or disjoint respectively.

## Bug fixes

* `phint_setdiff()` no longer returns a malformed phinterval when `phint2` contains instants (#3).

* `phint_invert()` no longer returns a malformed phinterval when `phint` contains instants (#9).

## Breaking changes

* `phint_unnest()` now always returns a dataframe with columns `key`, `start`, `end`, and `size`, instead of optionally including a `size` column.

The `keep_size` argument has been removed from `phint_unnest()` and the function signature has been revised:

```
# Old Usage
phint_unnest(phint, hole_to = c("drop", "na"), keep_size = FALSE, key = NULL)

# New Usage
phint_unnest(phint, key = NULL, hole_to = c("na", "drop"))
```

* `phint_squash()` and `datetime_squash()` have been split into two functions (#13).

  * `phint_squash()` and `datetime_squash()` now always squash intervals into a length-1 `<phinterval>`.
  
  * `phint_squash_by()` and `datetime_squash_by()` squash intervals within groups defined by the `by` argument.
  
  * The `order_by` argument of `phint_squash_by()` and `datetime_squash_by()` now defaults to `TRUE`, to match the behavior of `dplyr::group_by()`.
  
  * The `na.rm` argument of the squash functions has been renamed to `na_rm` (#19).
  
The return type of the `*_squash()` variants is now always a scalar `<phinterval>`
vector, meaning it is safe to use within `dplyr::summarize()`. This includes cases 
where the `phint` argument of `phint_squash()` or the `start, end` arguments of 
`datetime_squash()` are empty as the `empty_to = "empty"` option has been removed 
from the `empty_to` argument.

The return type of the `*_squash_by()` variants is now always a `tibble()` with 
columns `by` and `phint`. Additionally, the `keep_by` argument has been removed 
from these variants.

```
# Old Usage
phint_squash(
  phint,
  by = NULL,
  na.rm = TRUE,
  empty_to = c("hole", "na", "empty"),
  order_by = FALSE,
  keep_by = FALSE
)

# New Usage
phint_squash(
  phint,
  na_rm = TRUE,
  empty_to = c("hole", "na")
)

phint_squash_by(
  phint,
  by,
  na_rm = TRUE,
  empty_to = c("hole", "na"),
  order_by = TRUE
)
```

# phinterval 1.0.0

* Initial CRAN submission.
