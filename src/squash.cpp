#include "type-helpers.h"
#include "type-range.h"
#include "type-interval.h"
#include "type-phinterval.h"
#include <algorithm>
#include <numeric>
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

// squash ----------------------------------------------------------------------

List squash_num_impl(const NumericVector& starts, const NumericVector& ends);

template <typename VectorType>
List squash_vec_impl(const VectorType& vec, bool na_rm);

// [[Rcpp::export]]
List phint_squash_cpp(IntegerVector size, List starts, List ends, bool na_rm) {
  const R_xlen_t n = size.size();
  if (n == 0) {
    return phint_result_hole();
  }
  const int* p_size = INTEGER(size);

  // First pass: count the total number of spans and test for NA values
  bool all_na = true;
  R_xlen_t total_spans = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    int size_i = p_size[i];
    if (size_i == NA_INTEGER) {
      if (na_rm) continue;
      return phint_result_na();
    }
    all_na = false;
    total_spans += size_i;
  }

  if (total_spans == 0) {
    if (all_na) {
      return phint_result_na();
    }
    return phint_result_hole();
  }

  // Allocate vectors to unlist into
  NumericVector starts_buffer = no_init(total_spans);
  NumericVector ends_buffer = no_init(total_spans);
  double* p_starts_buffer = REAL(starts_buffer);
  double* p_ends_buffer = REAL(ends_buffer);

  // Second pass: unlist, copy all non-NA elements into the buffers
  R_xlen_t offset = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    int size_i = p_size[i];
    if (size_i == 0 || size_i == NA_INTEGER) continue;

    SEXP starts_i = VECTOR_ELT(starts, i);
    SEXP ends_i = VECTOR_ELT(ends, i);
    const double* p_starts_i = REAL(starts_i);
    const double* p_ends_i = REAL(ends_i);

    // Note size_i > 0
    std::memcpy(p_starts_buffer + offset, p_starts_i, size_i * sizeof(double));
    std::memcpy(p_ends_buffer + offset, p_ends_i, size_i * sizeof(double));
    offset += size_i;
  }

  // Squash the unlisted spans
  return squash_num_impl(starts_buffer, ends_buffer);
}

// [[Rcpp::export]]
List intvl_squash_cpp(DatetimeVector starts, NumericVector spans, bool na_rm) {
  IntvlVectorView vec { starts, spans };
  return squash_vec_impl(vec, na_rm);
}

// [[Rcpp::export]]
List range_squash_cpp(DatetimeVector starts, DatetimeVector ends, bool na_rm) {
  RangeVectorView vec { starts, ends };
  return squash_vec_impl(vec, na_rm);
}

List squash_num_impl(const NumericVector& starts, const NumericVector& ends) {
  const R_xlen_t n = starts.size();
  if (n == 0) {
    return phint_result_hole();
  }

  const double* p_starts = REAL(starts);
  const double* p_ends = REAL(ends);

  std::vector<size_t> span_indices(n);
  std::iota(span_indices.begin(), span_indices.end(), 0);
  std::sort(span_indices.begin(), span_indices.end(), [&](size_t i, size_t j) {
    double start_i = p_starts[i], start_j = p_starts[j];
    if (start_i != start_j) return start_i < start_j;
    return p_ends[i] < p_ends[j];
  });

  NumericVector out_starts = no_init(n);
  NumericVector out_ends = no_init(n);
  double* p_out_starts = REAL(out_starts);
  double* p_out_ends = REAL(out_ends);

  R_xlen_t out_index = 0;
  size_t span_index = span_indices[0];
  double current_start = p_starts[span_index];
  double current_max_end = p_ends[span_index];

  for (R_xlen_t i = 1; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    span_index = span_indices[i];
    double next_start = p_starts[span_index];
    double next_end = p_ends[span_index];

    if (next_start <= current_max_end) {
      current_max_end = std::max(current_max_end, next_end);
    } else {
      p_out_starts[out_index] = current_start;
      p_out_ends[out_index] = current_max_end;
      out_index++;
      current_start = next_start;
      current_max_end = next_end;
    }
  }

  p_out_starts[out_index] = current_start;
  p_out_ends[out_index] = current_max_end;
  out_index++;

  return List::create(
    Named("size") = IntegerVector::create(out_index),
    Named("starts") = List::create(head(out_starts, out_index)),
    Named("ends") = List::create(head(out_ends, out_index))
  );
}

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

// squash by -------------------------------------------------------------------

template <typename VectorType>
List squash_by_impl(const VectorType& vec, List group_locs, bool na_rm);

// [[Rcpp::export]]
List phint_squash_by_cpp(
    IntegerVector size,
    List starts,
    List ends,
    List group_locs,
    bool na_rm
) {
  PhintVectorView vec { size, starts, ends };
  return squash_by_impl(vec, group_locs, na_rm);
}

// [[Rcpp::export]]
List intvl_squash_by_cpp(
    DatetimeVector starts,
    NumericVector spans,
    List group_locs,
    bool na_rm
) {
  IntvlVectorView vec { starts, spans };
  return squash_by_impl(vec, group_locs, na_rm);
}

// [[Rcpp::export]]
List range_squash_by_cpp(
    DatetimeVector starts,
    DatetimeVector ends,
    List group_locs,
    bool na_rm
) {
  RangeVectorView vec { starts, ends };
  return squash_by_impl(vec, group_locs, na_rm);
}

void squash_scalar(
    const std::vector<double>& starts,
    const std::vector<double>& ends,
    std::vector<size_t>& index_buffer,
    PhintBuffer& buffer
);

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

void squash_scalar(
    const std::vector<double>& starts,
    const std::vector<double>& ends,
    std::vector<size_t>& index_buffer,
    PhintBuffer& buffer
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
