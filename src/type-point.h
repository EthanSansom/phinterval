#ifndef PHINTERVAL_TYPE_POINT_H
#define PHINTERVAL_TYPE_POINT_H

#include "type-helpers.h"
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

class PointVectorView;
using PointView = ScalarView;
using PointRecycled = Recycled<PointVectorView, PointView>;

class PointVectorView {
private:
  const DatetimeVector& point;
  const double* p_point;
  const R_xlen_t n;

public:
  PointVectorView(const DatetimeVector& point_) :
    point(point_), p_point(REAL(point_)), n(point_.size()) {}

  PointRecycled as_recycled() const;
  PointView view(R_xlen_t i) const;
  R_xlen_t n_sets() const { return n; }
};

inline PointView PointVectorView::view(R_xlen_t i) const {
  double point = p_point[i];

  if (ISNAN(point)) {
    return ScalarView::na_view();
  }
  return { point, point };
}

inline PointRecycled PointVectorView::as_recycled() const {
  return PointRecycled{*this};
}

#endif
