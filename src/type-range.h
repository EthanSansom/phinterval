#ifndef PHINTERVAL_TYPE_RANGE_H
#define PHINTERVAL_TYPE_RANGE_H

#include "type-helpers.h"
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

class RangeVectorView;
using RangeView = ScalarView;
using RangeRecycled = Recycled<RangeVectorView, RangeView>;

class RangeVectorView {
private:
  const double* p_start;
  const double* p_end;
  const R_xlen_t n;

public:
  RangeVectorView(
    const DatetimeVector& start_,
    const DatetimeVector& end_
  ) : p_start(REAL(start_)), p_end(REAL(end_)), n(start_.size()) {}

  RangeRecycled as_recycled() const;
  RangeView view(R_xlen_t i) const;
  R_xlen_t n_sets() const { return n; }
};

inline RangeView RangeVectorView::view(R_xlen_t i) const {
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

inline RangeRecycled RangeVectorView::as_recycled() const {
  return RangeRecycled{*this};
}

#endif
