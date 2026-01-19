#ifndef PHINTERVAL_VECTORIZATION_H
#define PHINTERVAL_VECTORIZATION_H

#include "type-interval.h"
#include "type-phinterval.h"
#include <Rcpp.h>
using namespace Rcpp;

template <typename Op>
List phint_operate(const PhintVector& x, const PhintVector& y, Op op) {
  const R_xlen_t x_n = x.n_sets();
  const R_xlen_t y_n = y.n_sets();

  if (y_n == 1) {
    const R_xlen_t n = x_n;
    PhintBuffer buffer(n);

    const PhintView y_view = y.view(0);
    const int y_size = y.size[0];
    const bool y_is_na = (y_size == NA_INTEGER);
    const bool y_is_span = (y_size == 1);

    for (R_xlen_t i = 0; i < n; i++) {
      if (!(i & 8191)) checkUserInterrupt();

      int x_size = x.size[i];

      if (y_is_na || x_size == NA_INTEGER) {
        buffer.add_na_element();
      } else if (y_is_span && x_size == 1) {
        op.apply_to_span(x.view(i), y_view, buffer);
      } else {
        op.apply_to_set(x.view(i), y_view, buffer);
      }
    }
    return buffer.get_results();
  }

  if (x_n == 1) {
    const R_xlen_t n = y_n;
    PhintBuffer buffer(n);

    const PhintView x_view = x.view(0);
    const int x_size = x.size[0];
    const bool x_is_na = (x_size == NA_INTEGER);
    const bool x_is_span = (x_size == 1);

    for (R_xlen_t i = 0; i < n; i++) {
      if (!(i & 8191)) checkUserInterrupt();

      int y_size = y.size[i];

      if (x_is_na || y_size == NA_INTEGER) {
        buffer.add_na_element();
      } else if (x_is_span && y_size == 1) {
        op.apply_to_span(x_view, y.view(i), buffer);
      } else {
        op.apply_to_set(x_view, y.view(i), buffer);
      }
    }
    return buffer.get_results();
  }

  const R_xlen_t n = x_n;
  PhintBuffer buffer(n);

  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    int x_size = x.size[i];
    int y_size = y.size[i];

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

template <typename Op>
List intvl_operate(const IntvlVector& x, const IntvlVector& y, Op op) {
  const R_xlen_t x_n = x.n_sets();
  const R_xlen_t y_n = y.n_sets();

  // if (xn == 1) return map_binary_op_recycled(y, x.view(0, 0), op);
  // if (yn == 1) return map_binary_op_recycled(x, y.view(0, 0), op);

  const R_xlen_t n = x_n; // Size compatibility is checked in R
  PhintBuffer buffer(n);

  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    IntvlView x_i = x.view(i);
    IntvlView y_i = y.view(i);

    if (x_i.is_na || y_i.is_na) {
      buffer.add_na_element();
    } else {
      op.apply_to_span(x_i, y_i, buffer);
    }
  }

  return buffer.get_results();
}

#endif
