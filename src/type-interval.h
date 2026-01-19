#ifndef PHINTERVAL_TYPE_INTERVAL_H
#define PHINTERVAL_TYPE_INTERVAL_H

#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

struct IntvlView {
  const int size;
  const double starts;
  const double ends;
  const bool is_na;

  // `start(i)`, `end(i)` are only called when `i < size` (e.g. `i = 0`). This
  // interface allows `IntvlView` and `PhintView` to be used interchangeably.
  double start(int) const { return starts; }
  double end(int) const { return ends; }
  bool is_empty() const { return false; }
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
      return { NA_INTEGER, NA_REAL, NA_REAL, true};
    }

    if (end_i < start_i) {
      std::swap(start_i, end_i);
    }

    return { 1, start_i, end_i, false };
  }
  R_xlen_t n_sets() const { return n; }
};

class IntvlScalar {
private:
  const IntvlView m_view;
  const int m_size;

public:
  IntvlScalar(const IntvlVector& intvl) : m_view(intvl.view(0)), m_size(intvl.view(0).size) {
    if (intvl.n_sets() != 1) {
      stop("Attempted to initialize a IntvlScalar from a vector of length %i.", intvl.n_sets());
    }
  }

  // TODO: See if inline matters here and look elsewhere for inline opportunities
  inline IntvlView view(R_xlen_t) const { return m_view; }
  inline R_xlen_t n_sets() const { return 1; }
  inline int size(R_xlen_t) const { return m_size; }
};

#endif
