#ifndef PHINTERVAL_TYPE_INTERVAL_H
#define PHINTERVAL_TYPE_INTERVAL_H

#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

struct IntvlView;
class IntvlRecycled;

// TODO: Modify to work with <Interval> vectors directly
// - Change initialize to use `DatetimeVector start`, `NumericVector span`
// - The only other change to make is normalizing within `view()`
// - From R, use `int_start(x)` and `int_length(x)` as accessors

class IntvlVector {
private:
  const NumericVector& start;
  const NumericVector& end;
  const double* p_start;
  const double* p_end;
  const R_xlen_t n;

public:
  IntvlVector(
    const NumericVector& start_,
    const NumericVector& end_
  ) : start(start_), end(end_), p_start(REAL(start_)), p_end(REAL(end_)), n(start.size()) {}

  IntvlRecycled as_recycled() const;
  IntvlView view(R_xlen_t i) const;
  R_xlen_t n_sets() const { return n; }
};

struct IntvlView {
  const int size;
  const double starts;
  const double ends;
  const bool is_na;

  double start(int) const { return starts; }
  double end(int) const { return ends; }
  bool is_empty() const { return false; }
  bool is_scalar() const { return !is_na; }
};

IntvlView IntvlVector::view(R_xlen_t i) const {
  double start = p_start[i];
  double end = p_end[i];

  if (ISNAN(start) || ISNAN(end)) {
    return { NA_INTEGER, NA_REAL, NA_REAL, true};
  }

  if (end < start) {
    return { 1, end, start, false };
  }
  return { 1, start, end, false };
}

class IntvlRecycled {
private:
  const IntvlView m_view;
  const int m_size;

public:
  IntvlRecycled(const IntvlVector& intvl)
    : m_view(intvl.view(0)),
      m_size(intvl.view(0).size)
  {
    if (intvl.n_sets() != 1) {
      stop("Attempted to recycle a IntvlVector of length %i.", intvl.n_sets());
    }
  }

  inline IntvlView view(R_xlen_t) const { return m_view; }
  inline R_xlen_t n_sets() const { return 1; }
  inline int size(R_xlen_t) const { return m_size; }
};

inline IntvlRecycled IntvlVector::as_recycled() const {
  return IntvlRecycled{*this};
}

#endif
