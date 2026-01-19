#ifndef PHINTERVAL_TYPE_INTERVAL_H
#define PHINTERVAL_TYPE_INTERVAL_H

#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

struct IntvlView {
  const double start;
  const double end;
  const bool is_na;
};

// TODO: Look into receiving the start (POSIXct) and length (double) directly,
//       and doing the conversion upon `view()`.
//       - Call with `int_start()` and `int_length()` -> getters for @.Data
class IntvlVector {
public:
  const NumericVector& start;
  const NumericVector& end;
  const double* p_start;
  const double* p_end;
  R_xlen_t n;

  IntvlVector(
    const NumericVector& start_,
    const NumericVector& end_
  ) : start(start_), end(end_), p_start(REAL(start_)), p_end(REAL(end_)), n(start.size()) {}

  IntvlView view(R_xlen_t i) const {
    double start_i = p_start[i];
    double end_i = p_end[i];

    if (ISNAN(start_i) || ISNAN(end_i)) {
      return { NA_REAL, NA_REAL, true};
    }

    //
    if (end_i < start_i) {
      std::swap(start_i, end_i);
    }

    return { start_i, end_i, false };
  }
  R_xlen_t n_sets() const { return n; }
};

#endif
