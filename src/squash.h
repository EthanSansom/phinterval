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
List squash_vec_impl(const VectorType& vec, bool na_rm);

template <typename VectorType>
List squash_by_impl(const VectorType& vec, List group_locs, bool na_rm);

template <typename Buffer>
void squash_scalar(
    const std::vector<double>& starts,
    const std::vector<double>& ends,
    std::vector<size_t>& index_buffer,
    Buffer& buffer
);

// squash_vec_impl -------------------------------------------------------------

template <typename VectorType>
List squash_vec_impl(const VectorType& vec, bool na_rm) {
  const R_xlen_t n = vec.n_sets();
  if (n == 0) {
    return phint_result_hole();
  }

  NumericVector norm_starts = no_init(n);
  NumericVector norm_ends = no_init(n);
  double* p_norm_starts = REAL(norm_starts);
  double* p_norm_ends = REAL(norm_ends);

  int n_non_na = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    auto view = vec.view(i);

    if (view.is_na) {
      if (na_rm) continue;
      return phint_result_na();
    }

    p_norm_starts[n_non_na] = view.start(0);
    p_norm_ends[n_non_na] = view.end(0);
    n_non_na++;
  }

  if (n_non_na == 0) {
    return phint_result_na();
  } else if (n_non_na < n) {
    return squash_num_impl(head(norm_starts, n_non_na), head(norm_ends, n_non_na));
  } else {
    return squash_num_impl(norm_starts, norm_ends);
  }
}

// squash_by_impl --------------------------------------------------------------

template <typename VectorType>
List squash_by_impl(const VectorType& vec, List group_locs, bool na_rm) {
  R_xlen_t n_groups = group_locs.size();
  PhintBuffer buffer(n_groups);

  std::vector<double> temp_group_starts;
  std::vector<double> temp_group_ends;
  std::vector<size_t> span_indices;

  temp_group_starts.reserve(64);
  temp_group_ends.reserve(64);
  span_indices.reserve(64);

  for (R_xlen_t group = 0; group < n_groups; group++) {
    SEXP locs = VECTOR_ELT(group_locs, group);
    const int* p_locs = INTEGER(locs);
    R_xlen_t group_size = Rf_xlength(locs);

    bool group_is_na = true;

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

      // Requires C++17, skips loop for scalar span types (e.g. not PhintVectorView)
      if constexpr (is_scalar_view<decltype(view)>) {
        temp_group_starts.push_back(view.starts);
        temp_group_ends.push_back(view.ends);
      } else {
        for (int k = 0; k < view.size; k++) {
          temp_group_starts.push_back(view.start(k));
          temp_group_ends.push_back(view.end(k));
        }
      }
    }

    // Add the squashed group spans to the buffer
    if (group_is_na) {
      buffer.add_na_element();
    } else {
      squash_scalar(temp_group_starts, temp_group_ends, span_indices, buffer);
    }

    temp_group_starts.clear();
    temp_group_ends.clear();
  }

  return buffer.get_results();
}

// squash_scalar ---------------------------------------------------------------

template <typename Buffer>
void squash_scalar(
    const std::vector<double>& starts,
    const std::vector<double>& ends,
    std::vector<size_t>& index_buffer,
    Buffer& buffer
) {
  if (starts.empty()) {
    buffer.add_empty_element();
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
      buffer.add_span(current_start, current_max_end);
      current_start = next_start;
      current_max_end = next_end;
    }
  }

  buffer.add_span(current_start, current_max_end);
  buffer.finish_element();
}

#endif
