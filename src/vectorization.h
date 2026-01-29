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

template <typename VectorX, typename VectorY, typename Rel>
LogicalVector phint_relate_impl(VectorX x, VectorY y, Rel rel, R_xlen_t n);

template <typename VectorX, typename Fn>
List phint_modify_impl(VectorX x, Fn fn, R_xlen_t n);

template <typename VectorX, typename VectorY, typename Op>
List phint_operate(const VectorX& x, const VectorY& y, Op op) {
  const R_xlen_t x_n = x.n_sets();
  const R_xlen_t y_n = y.n_sets();
  const R_xlen_t n = std::max(x_n, y_n);

  // Recyclability of `x` and `y` is checked in R
  if (x_n == 0 || y_n == 0) return phint_result_empty();
  if (x_n == 1) return phint_operate_impl(x.as_recycled(), y, op, n);
  if (y_n == 1) return phint_operate_impl(x, y.as_recycled(), op, n);
  return phint_operate_impl(x, y, op, n);
}

template <typename VectorX, typename VectorY, typename Rel>
LogicalVector phint_relate(const VectorX& x, const VectorY& y, Rel rel) {
  const R_xlen_t x_n = x.n_sets();
  const R_xlen_t y_n = y.n_sets();
  const R_xlen_t n = std::max(x_n, y_n);

  // Recyclability of `x` and `y` is checked in R
  if (x_n == 0 || y_n == 0) return LogicalVector(0);
  if (x_n == 1) return phint_relate_impl(x.as_recycled(), y, rel, n);
  if (y_n == 1) return phint_relate_impl(x, y.as_recycled(), rel, n);
  return phint_relate_impl(x, y, rel, n);
}

template <typename VectorX, typename Fn>
List phint_modify(const VectorX& x, Fn fn) {
  const R_xlen_t n = x.n_sets();
  if (n == 0) return phint_result_empty();
  return phint_modify_impl(x, fn, n);
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

    // Requires C++17, skips scalar check for span vectors (e.g. IntvlVectorView)
    if constexpr (is_scalar_view<decltype(x_view)> && is_scalar_view<decltype(y_view)>) {
      op.apply_to_span(x_view, y_view, buffer);
    } else {
      if (x_view.is_scalar() && y_view.is_scalar()) {
        op.apply_to_span(x_view, y_view, buffer);
      } else {
        op.apply_to_set(x_view, y_view, buffer);
      }
    }
  }

  return buffer.get_results();
}

template <typename VectorX, typename VectorY, typename Rel>
LogicalVector phint_relate_impl(VectorX x, VectorY y, Rel rel, R_xlen_t n) {
  LogicalVector out(n);
  int* p_out = LOGICAL(out);

  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    auto x_view = x.view(i);
    auto y_view = y.view(i);

    if (x_view.is_na || y_view.is_na) {
      p_out[i] = NA_LOGICAL;
      continue;
    }

    if constexpr (is_scalar_view<decltype(x_view)> && is_scalar_view<decltype(y_view)>) {
      p_out[i] = rel.apply_to_span(x_view, y_view);
    } else {
      if (x_view.is_scalar() && y_view.is_scalar()) {
        p_out[i] = rel.apply_to_span(x_view, y_view);
      } else {
        p_out[i] = rel.apply_to_set(x_view, y_view);
      }
    }
  }

  return out;
}

template <typename VectorX, typename Fn>
List phint_modify_impl(VectorX x, Fn fn, R_xlen_t n) {
  PhintBuffer buffer(n);

  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    auto x_view = x.view(i);

    if (x_view.is_na) {
      buffer.add_na_element();
      continue;
    }

    if constexpr (is_scalar_view<decltype(x_view)>) {
      fn.apply_to_span(x_view, buffer);
    } else {
      if (x_view.is_scalar()) {
        fn.apply_to_span(x_view, buffer);
      } else {
        fn.apply_to_set(x_view, buffer);
      }
    }
  }

  return buffer.get_results();
}

#endif
