#ifndef PHINTERVAL_SQUASH_H
#define PHINTERVAL_SQUASH_H

#include "type-helpers.h"
#include "type-range.h"
#include "type-interval.h"
#include "type-phinterval.h"
#include <algorithm>
#include <vector>
#include <numeric>
#include <Rcpp.h>
using namespace Rcpp;

// declarations ----------------------------------------------------------------

List squash_num_impl(const NumericVector& starts, const NumericVector& ends);

template <typename VectorType>
List squash_vec_impl(const VectorType& vec, bool na_rm, const String& empty_to);

template <typename VectorType>
List squash_vec_by_impl(const VectorType& vec, List group_locs, bool na_rm);

template <typename Buffer>
void squash_scalar(
    const std::vector<double>& starts,
    const std::vector<double>& ends,
    std::vector<size_t>& index_buffer,
    Buffer& out_buffer
);

// squash_vec_impl -------------------------------------------------------------

template <typename VectorType>
List squash_vec_impl(const VectorType& vec, bool na_rm, const String& empty_to) {
  const R_xlen_t n = vec.n_sets();
  if (n == 0) {
    if (empty_to == "hole") return phint_result_hole();
    return phint_result_na();
  }

  NumericVector starts_buffer = no_init(n);
  NumericVector ends_buffer = no_init(n);
  double* p_starts_buffer = REAL(starts_buffer);
  double* p_ends_buffer = REAL(ends_buffer);

  int n_non_na = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    auto view = vec.view(i);
    if (view.is_na) {
      if (na_rm) continue;
      return phint_result_na();
    }

    p_starts_buffer[n_non_na] = view.start(0);
    p_ends_buffer[n_non_na] = view.end(0);
    n_non_na++;
  }

  if (n_non_na == 0) {
    return phint_result_na();
  } else if (n_non_na < n) {
    return squash_num_impl(head(starts_buffer, n_non_na), head(ends_buffer, n_non_na));
  } else {
    return squash_num_impl(starts_buffer, ends_buffer);
  }
}

// squash_vec_by_impl ----------------------------------------------------------

template <typename VectorType>
List squash_vec_by_impl(
    const VectorType& vec,
    List group_locs,
    bool na_rm,
    const String& empty_to
) {
  if (vec.n_sets() == 0) {
    if (empty_to == "hole") return phint_result_empty();
    return phint_result_na();
  }

  R_xlen_t n_groups = group_locs.size();
  PhintBuffer out_buffer(n_groups);

  std::vector<double> group_starts;
  std::vector<double> group_ends;
  std::vector<size_t> index_buffer;

  group_starts.reserve(64);
  group_ends.reserve(64);
  index_buffer.reserve(64);

  for (R_xlen_t group = 0; group < n_groups; group++) {
    SEXP locs = VECTOR_ELT(group_locs, group);
    const int* p_locs = INTEGER(locs);
    R_xlen_t group_size = Rf_xlength(locs);

    bool group_is_na = true; // Flag as `true` until proven otherwise
    for (R_xlen_t j = 0; j < group_size; j++) {
      int i = p_locs[j] - 1;

      auto view = vec.view(i);
      if (view.is_na) {
        if (na_rm) continue; // If `na_rm = true`, skip NA elements
        group_is_na = true;  // Otherwise, set the entire group to NA
        break;
      }
      group_is_na = false;

      if (view.is_empty()) continue;

      // Requires C++17, skips loop for scalar span types
      if constexpr (is_scalar_view<decltype(view)>) {
        group_starts.push_back(view.starts);
        group_ends.push_back(view.ends);
      } else {
        for (int k = 0; k < view.size; k++) {
          group_starts.push_back(view.start(k));
          group_ends.push_back(view.end(k));
        }
      }
    }

    if (group_is_na) {
      out_buffer.add_na_element();
    } else {
      squash_scalar(group_starts, group_ends, index_buffer, out_buffer);
    }
    group_starts.clear();
    group_ends.clear();
  }

  return out_buffer.get_results();
}

// squash_scalar ---------------------------------------------------------------

template <typename Buffer>
void squash_scalar(
    const std::vector<double>& starts,
    const std::vector<double>& ends,
    std::vector<size_t>& index_buffer,
    Buffer& out_buffer
) {
  if (starts.empty()) {
    out_buffer.add_hole_element();
    return;
  }

  const size_t n_spans = starts.size();
  index_buffer.resize(n_spans);
  std::iota(index_buffer.begin(), index_buffer.end(), 0);

  std::sort(index_buffer.begin(), index_buffer.end(), [&](size_t i, size_t j) {
    if (starts[i] != starts[j]) return starts[i] < starts[j];
    return ends[i] < ends[j];
  });

  size_t index = index_buffer[0];
  double current_start = starts[index];
  double current_max_end = ends[index];

  for (size_t i = 1; i < n_spans; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    index = index_buffer[i];
    double next_start = starts[index];
    double next_end = ends[index];

    if (next_start <= current_max_end) {
      current_max_end = std::max(current_max_end, next_end);
    } else {
      out_buffer.add_span(current_start, current_max_end);
      current_start = next_start;
      current_max_end = next_end;
    }
  }

  out_buffer.add_span(current_start, current_max_end);
  out_buffer.finish_element();
}

#endif
