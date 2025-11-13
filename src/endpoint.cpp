#include "endpoint.h"
#include <Rcpp.h>
using namespace Rcpp;

bool operator< (const Endpoint &a, const Endpoint &b) {
  if (a.value == b.value) {
    return a.is_start > b.is_start;
  }
  return a.value < b.value;
}

// Used as the sort order to determine whether an interval set `x` contains an
// interval set `y`. In the event of tied endpoint values, the starts of `x`
// should be first and ends of `x` should be last, such that `x` encloses `y`.
//
// Normal sort order    Instant y within x    Identical y within x
// x [  ]  -> [ { ] }   x [   ] -> [{}   ]    x [   ] -> [{   }]
// y   {  }             y {}                  y {   }
bool operator< (const BinaryEndpoint &a, const BinaryEndpoint &b) {
  if (a.value == b.value) {
    if (a.in_x && !b.in_x) {
      return a.is_start;
    } else if (!a.in_x && b.in_x) {
      return !b.is_start;
    } else {
      return a.is_start > b.is_start;
    }
  }
  return a.value < b.value;
}

// Used as the sort order when taking the set difference between an interval
// set `x` and interval set `y`. Ends of `x` should always come before endpoints
// on `y` and starts of `x` should always come before endpoints on `y`.
//
// Ends of x precede points on y         Starts of x follow points on y
// x [   ] -> [ { ]}  x [ ]   -> [ ]{ }  x [   ] -> {[ } ]  x   [ ] -> { }[ ]
// y   { }            y   { }            y { }              y { }
//
// Note that if both `x` and `y` are instantaneous intervals, this operation
// is no longer transitive, as the end of `x` must precede its start.
// x [] -> ]{}[
// y {}
bool lt_setdiff (const BinaryEndpoint &a, const BinaryEndpoint &b) {
  if (a.value == b.value) {
    if (!(a.in_x || b.in_x)) {
      stop("Internal error: unexpected instantaneous intervals.");
    }
    if (a.in_x && !b.in_x) {
      return !a.is_start;
    } else if (!a.in_x && b.in_x) {
      return b.is_start;
    } else {
      return a.is_start > b.is_start;
    }
  }
  return a.value < b.value;
}
