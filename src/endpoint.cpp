#include "endpoint.h"
#include <Rcpp.h>
using namespace Rcpp;

bool operator< (const Endpoint &a, const Endpoint &b) {
  if (a.value == b.value) {
    return a.is_start > b.is_start; // Starts < Ends if values are tied
  }
  return a.value < b.value;
}

// Used as the sort order to determine whether an interval set `x` is within an
// interval set `y`. Starts of `x` follow endpoints on `y` and ends of `x`
// precede them.
//
// Normal sort order    Instant x within y  Identical x within y
// x   [  ] -> { [ } ]  x []    -> {[]   }  x [   ] -> {[   ]}
// y {  }               y {   }             y {   }
//
// Abutting x and y
// x [ ]    -> [ ]{ }   x   [ ] -> { }[ ]
// y   {  }             y { }
//
// Note that if both `x` and `y` are instantaneous intervals, this operation
// is no longer transitive, as the end of `x` must precede its start.
//
// x [] -> ]{}[, but this contradicts `[` < `]`
// y {}
bool operator< (const BinaryEndpoint &a, const BinaryEndpoint &b) {
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
