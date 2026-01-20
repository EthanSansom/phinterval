#ifndef PHINTERVAL_TYPE_DATETIME_H
#define PHINTERVAL_TYPE_DATETIME_H

#include "type-helpers.h"
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

class DtimeVector;
using DtimeView = ScalarView;
using DtimeRecycled = Recycled<DtimeVector, DtimeView>;

class DtimeVector {
private:
  const DatetimeVector& start;
  const DatetimeVector& end;
  const double* p_start;
  const double* p_end;
  const R_xlen_t n;

public:
  DtimeVector(
    const DatetimeVector& start_,
    const DatetimeVector& end_
  ) : start(start_), end(end_), p_start(REAL(start_)), p_end(REAL(end_)), n(start.size()) {}

  DtimeRecycled as_recycled() const;
  DtimeView view(R_xlen_t i) const;
  R_xlen_t n_sets() const { return n; }
};

inline DtimeView DtimeVector::view(R_xlen_t i) const {
  double start = p_start[i];
  double end = p_end[i];

  if (ISNAN(start) || ISNAN(end)) {
    return ScalarView::na_view();
  }

  if (end < start) {
    return { end, start };
  }
  return { start, end };
}

inline DtimeRecycled DtimeVector::as_recycled() const {
  return DtimeRecycled{*this};
}

#endif
