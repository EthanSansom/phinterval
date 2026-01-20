#ifndef PHINTERVAL_TYPE_INTERVAL_H
#define PHINTERVAL_TYPE_INTERVAL_H

#include "type-helpers.h"
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

class IntvlVector;
using IntvlView = ScalarView;
using IntvlRecycled = Recycled<IntvlVector, IntvlView>;

class IntvlVector {
private:
  const DatetimeVector& start;
  const NumericVector& span;
  const double* p_start;
  const double* p_span;
  const R_xlen_t n;

public:
  IntvlVector(
    const DatetimeVector& start_,
    const NumericVector& span_
  ) : start(start_), span(span_), p_start(REAL(start_)), p_span(REAL(span_)), n(start.size()) {}

  IntvlRecycled as_recycled() const;
  IntvlView view(R_xlen_t i) const;

  R_xlen_t n_sets() const { return n; }
};

IntvlView IntvlVector::view(R_xlen_t i) const {
  double start = p_start[i];
  double span = p_span[i];

  if (ISNAN(start) || ISNAN(span)) {
    return ScalarView::na_view();
  }

  if (span < 0) {
    return { start + span, start };
  }
  return { start, start + span };
}

inline IntvlRecycled IntvlVector::as_recycled() const {
  return IntvlRecycled{*this};
}

#endif
