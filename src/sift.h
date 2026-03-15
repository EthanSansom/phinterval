#ifndef PHINTERVAL_SIFT_H
#define PHINTERVAL_SIFT_H

#include "type-helpers.h"
#include "type-phinterval.h"
#include "type-interval.h"
#include <functional>

#include <Rcpp.h>
using namespace Rcpp;

template <bool HasMin, bool HasMax, bool KeepInBounds, typename VectorType>
List sift_impl(
  const VectorType& x,
  const NumericVector& min_length,
  const NumericVector& max_length
) {
  const R_xlen_t n = x.n_sets();
  if (n == 0) return phint_result_empty();

  auto keep_span = [](double start, double end, double min_length, double max_length) -> bool {
    // At least one of HasMin, HasMax is guaranteed to be true
    double length = end - start; // Length 10, [min_length = 5, max_length = 12]
    bool within_bounds;
    if constexpr (HasMin && HasMax) {
      within_bounds = (min_length <= length) && (length <= max_length);
    } else if constexpr (HasMin) {
      within_bounds = min_length <= length;
    } else {
      within_bounds = length <= max_length;
    }

    if constexpr (KeepInBounds) {
      return within_bounds;
    } else {
      return !within_bounds;
    }
  };

  auto recycler = [](const NumericVector& v) -> std::function<double(R_xlen_t)> {
    if (v.size() == 1) {
      return [&](R_xlen_t) { return v[0]; };
    } else {
      return [&](R_xlen_t i) { return v[i]; };
    }
  };
  auto get_min_length = recycler(min_length);
  auto get_max_length = recycler(max_length);

  PhintBuffer out(n);
  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    auto x_i = x.view(i);
    if (x_i.is_na) {
      out.add_na_element();
      continue;
    }
    if (x_i.is_empty()) {
      out.add_empty_element();
      continue;
    }

    double min_length_i = get_min_length(i);
    double max_length_i = get_max_length(i);

    if constexpr (is_scalar_view<decltype(x_i)>) {
      if (keep_span(x_i.start(0), x_i.end(0), min_length_i, max_length_i)) {
        out.add_scalar_element(x_i.start(0), x_i.end(0));
      }
      continue;
    }

    for (int k = 0; k < x_i.size; k++) {
      if (keep_span(x_i.start(k), x_i.end(k), min_length_i, max_length_i)) {
        out.add_span(x_i.start(k), x_i.end(k));
      }
    }
    out.finish_element();
  }

  return out.get_results();
}

template <typename VectorType>
List sift_dispatch(
    const VectorType& x,
    const String& action,
    Nullable<NumericVector> min_length_,
    Nullable<NumericVector> max_length_
) {
  // One of these is guaranteed to be non-NULL in R
  bool has_min = min_length_.isNotNull();
  bool has_max = max_length_.isNotNull();
  bool keep_in_bounds = (action == "keep");

  if (has_min && has_max) { // bounded []
    NumericVector min_length(min_length_);
    NumericVector max_length(max_length_);
    if (keep_in_bounds) {
      return sift_impl<true, true, true>(x, min_length, max_length);
    } else {
      return sift_impl<true, true, false>(x, min_length, max_length);
    }
  } else if (has_min) { // bounded below
    NumericVector min_length(min_length_);
    NumericVector max_length(1); // Dummy value, never used
    if (keep_in_bounds) {
      return sift_impl<true, false, true>(x, min_length, max_length);
    } else {
      return sift_impl<true, false, false>(x, min_length, max_length);
    }
  } else { // bounded above
    NumericVector min_length(1); // Dummy value, never used
    NumericVector max_length(max_length_);
    if (keep_in_bounds) {
      return sift_impl<false, true, true>(x, min_length, max_length);
    } else {
      return sift_impl<false, true, false>(x, min_length, max_length);
    }
  }
}

#endif
