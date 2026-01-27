#ifndef PHINTERVAL_TYPE_INTERVAL_H
#define PHINTERVAL_TYPE_INTERVAL_H

#include "type-helpers.h"
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

class IntvlVectorView;
using IntvlView = ScalarView;
using IntvlRecycled = Recycled<IntvlVectorView, IntvlView>;

class IntvlVectorView {
private:
  const double* p_start;
  const double* p_span;
  const R_xlen_t n;

public:
  IntvlVectorView(
    const DatetimeVector& start_,
    const NumericVector& span_
  ) : p_start(REAL(start_)), p_span(REAL(span_)), n(start_.size()) {}

  IntvlRecycled as_recycled() const;
  IntvlView view(R_xlen_t i) const;
  R_xlen_t n_sets() const { return n; }
};

inline IntvlView IntvlVectorView::view(R_xlen_t i) const {
  double start = p_start[i];
  double span = p_span[i];

  if (ISNAN(start) || ISNAN(span)) {
    return ScalarView::na_view();
  }

  // When `start` and `span` are opposite-signed infinite values, adding them
  // results in `NaN`. Instead, manually setting to infinite range in this case.
  if (!R_FINITE(span) && !R_FINITE(start) && (span != start)) {
    return { R_NegInf, R_PosInf };
  }

  if (span < 0) {
    return { start + span, start };
  }
  return { start, start + span };
}

inline IntvlRecycled IntvlVectorView::as_recycled() const {
  return IntvlRecycled{*this};
}

#endif
