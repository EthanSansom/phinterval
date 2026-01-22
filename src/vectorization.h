#ifndef PHINTERVAL_VECTORIZATION_H
#define PHINTERVAL_VECTORIZATION_H

#include "type-helpers.h"
#include "type-interval.h"
#include "type-phinterval.h"
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

template <typename VectorX, typename VectorY, typename Op>
List phint_operate_impl(VectorX x, VectorY y, Op op, R_xlen_t n);

template <typename VectorX, typename VectorY, typename Op>
List phint_operate(const VectorX& x, const VectorY& y, Op op) {
  const R_xlen_t x_n = x.n_sets();
  const R_xlen_t y_n = y.n_sets();
  const R_xlen_t n = std::max(x_n, y_n);

  // Recyclability of `x` and `y` is checked in R
  if (x_n == 1) return phint_operate_impl(x.as_recycled(), y, op, n);
  if (y_n == 1) return phint_operate_impl(x, y.as_recycled(), op, n);
  return phint_operate_impl(x, y, op, n);
}

template <typename VectorX, typename VectorY, typename Op>
List phint_operate_impl(VectorX x, VectorY y, Op op, R_xlen_t n) {
  PhintBuffer buffer(n);

  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    auto x_view = x.view(i);
    auto y_view = y.view(i);

    if (x_view.is_na || y_view.is_na) {
      buffer.add_na_element();
      continue;
    }

    // Requires C++17, skips scalar check for span vectors (e.g. IntvlVector)
    if constexpr (is_scalar_view<decltype(x_view)> && is_scalar_view<decltype(y_view)>) {
      // Both vectors are scalar types (e.g. not a PhintVector)
      op.apply_to_span(x_view, y_view, buffer);
    } else {
      // One of the vectors is a PhintVector, check for scalar set at run-time
      if (x_view.is_scalar() && y_view.is_scalar()) {
        op.apply_to_span(x_view, y_view, buffer);
      } else {
        op.apply_to_set(x_view, y_view, buffer);
      }
    }
  }

  return buffer.get_results();
}

#endif
