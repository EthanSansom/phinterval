#ifndef PHINTERVAL_VECTORIZATION_H
#define PHINTERVAL_VECTORIZATION_H

#include "type-interval.h"
#include "type-phinterval.h"
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

template <typename T, typename U, typename Op>
List phint_operate_impl(T x, U y, Op op, R_xlen_t n);

template <typename T, typename U, typename Op>
List intvl_operate_impl(T x, U y, Op op, R_xlen_t n);

template <typename Op>
List phint_operate(const PhintVector& x, const PhintVector& y, Op op) {
  const R_xlen_t x_n = x.n_sets();
  const R_xlen_t y_n = y.n_sets();
  const R_xlen_t n = std::max(x_n, y_n);

  if (x_n == 1) return phint_operate_impl(PhintScalar{x}, y, op, n);
  if (y_n == 1) return phint_operate_impl(x, PhintScalar{y}, op, n);
  return phint_operate_impl(x, y, op, n);
}

template <typename Op>
List intvl_operate(const IntvlVector& x, const IntvlVector& y, Op op) {
  const R_xlen_t x_n = x.n_sets();
  const R_xlen_t y_n = y.n_sets();
  const R_xlen_t n = std::max(x_n, y_n);

  if (x_n == 1) return intvl_operate_impl(IntvlScalar{x}, y, op, n);
  if (y_n == 1) return intvl_operate_impl(x, IntvlScalar{y}, op, n);
  return intvl_operate_impl(x, y, op, n);
}

template <typename T, typename U, typename Op>
List phint_operate_impl(T x, U y, Op op, R_xlen_t n) {
  PhintBuffer buffer(n);

  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    int x_size = x.size(i);
    int y_size = y.size(i);

    if (x_size == NA_INTEGER || y_size == NA_INTEGER) {
      buffer.add_na_element();
    } else if (x_size == 1 && y_size == 1) {
      op.apply_to_span(x.view(i), y.view(i), buffer);
    } else {
      op.apply_to_set(x.view(i), y.view(i), buffer);
    }
  }

  return buffer.get_results();
}

template <typename T, typename U, typename Op>
List intvl_operate_impl(T x, U y, Op op, R_xlen_t n) {
  PhintBuffer buffer(n);

  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    IntvlView x_view = x.view(i);
    IntvlView y_view = y.view(i);

    if (x_view.is_na || y_view.is_na) {
      buffer.add_na_element();
    } else {
      op.apply_to_span(x_view, y_view, buffer);
    }
  }

  return buffer.get_results();
}

#endif
