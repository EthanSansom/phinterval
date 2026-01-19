#include "type-phinterval.h"
#include <algorithm>
#include <numeric>
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

// TODO: Neither `phint_squash_cpp()` nor `intvl_squash_cpp()` handle empty inputs
// - Confirm when/if empty inputs are handled in R

// squash ----------------------------------------------------------------------

// [[Rcpp::export]]
List phint_squash_cpp(NumericVector starts, NumericVector ends) {
  const R_xlen_t n = starts.size();
  const double* p_starts = REAL(starts);
  const double* p_ends = REAL(ends);

  std::vector<size_t> span_indices(n);
  std::iota(span_indices.begin(), span_indices.end(), 0);

  std::sort(span_indices.begin(), span_indices.end(), [&](size_t i, size_t j) {
    double start_i = p_starts[i];
    double start_j = p_starts[j];

    if (start_i != start_j) return start_i < start_j;
    return p_ends[i] < p_ends[j];
  });

  // The maximum number of spans is `n`, the case where no spans are merged
  NumericVector out_starts = no_init(n);
  NumericVector out_ends = no_init(n);
  double* p_out_starts = REAL(out_starts);
  double* p_out_ends = REAL(out_ends);
  R_xlen_t out_index = 0;

  int span_index = span_indices[0];
  double current_start = p_starts[span_index];
  double current_max_end = p_ends[span_index];

  for (R_xlen_t i = 1; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    span_index = span_indices[i];
    double next_start = p_starts[span_index];
    double next_end = p_ends[span_index];

    if (next_start <= current_max_end) {
      if (next_end > current_max_end) {
        current_max_end = next_end;
      }
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

// [[Rcpp::export]]
List intvl_squash_cpp(DatetimeVector starts, NumericVector spans, bool na_rm) {
  const R_xlen_t n = starts.size();

  NumericVector norm_starts = no_init(n);
  NumericVector norm_ends = no_init(n);
  double* p_norm_starts = REAL(norm_starts);
  double* p_norm_ends = REAL(norm_ends);

  const double* p_starts = REAL(starts);
  const double* p_spans = REAL(spans);

  int n_non_na = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    double start = p_starts[i];
    double span = p_spans[i];

    if (ISNAN(start) || ISNAN(span)) {
      if (na_rm) continue;
      return List::create(
        Named("size") = IntegerVector::create(NA_INTEGER),
        Named("starts") = List::create(R_NilValue),
        Named("ends") = List::create(R_NilValue)
      );
    }

    // TODO: Benchmark `std::min/max()` approach vs. `if (span < 0)` approach
    double end = start + span;
    p_norm_starts[n_non_na] = std::min(start, end);
    p_norm_ends[n_non_na] = std::max(start, end);
    n_non_na++;
  }

  if (na_rm) {
    return phint_squash_cpp(head(norm_starts, n_non_na), head(norm_ends, n_non_na));
  }
  // If `!na_rm` and we're here, then every element was non-NA
  return phint_squash_cpp(norm_starts, norm_ends);
}

// squash by -------------------------------------------------------------------

// [[Rcpp::export]]
List phint_squash_by_cpp(
    IntegerVector size,
    List starts,
    List ends,
    List group_locs,
    bool na_rm
) {
  R_xlen_t n_groups = group_locs.size();
  const int* p_size = INTEGER(size);
  PhintBuffer buffer(n_groups);

  std::vector<double> temp_group_starts;
  std::vector<double> temp_group_ends;
  std::vector<size_t> span_indices;

  temp_group_starts.reserve(64);
  temp_group_ends.reserve(64);
  span_indices.reserve(64);

  for (R_xlen_t group = 0; group < n_groups; group++) {
    // 1. Collect the starts and ends from every span set in this group
    SEXP locs = VECTOR_ELT(group_locs, group);
    const int* p_locs = INTEGER(locs);

    R_xlen_t group_size = Rf_xlength(locs);
    bool group_is_na = true;

    for (R_xlen_t j = 0; j < group_size; j++) {
      int i = p_locs[j] - 1; // Adjust for R 1-based indexing
      int size = p_size[i];

      if (size == NA_INTEGER) {
        if (na_rm) continue; // If `na_rm`, skip `NA` elements
        group_is_na = true;  // Otherwise, set the entire group to `NA`
        break;
      }

      group_is_na = false;
      if (size == 0) continue;

      SEXP starts_i = VECTOR_ELT(starts, i);
      SEXP ends_i = VECTOR_ELT(ends, i);
      const double* p_starts_i = REAL(starts_i);
      const double* p_ends_i = REAL(ends_i);

      for (int k = 0; k < size; k++) {
        temp_group_starts.push_back(p_starts_i[k]);
        temp_group_ends.push_back(p_ends_i[k]);
      }
    }

    if (group_is_na) {
      buffer.add_na_element();
      temp_group_starts.clear();
      temp_group_ends.clear();
      continue;
    }

    if (temp_group_starts.empty()) {
      buffer.add_empty_element();
      continue; // Don't need to `clear()` empty starts/ends
    }

    // 2. Perform the usual squash logic, only on the spans in this group
    size_t group_span_count = temp_group_starts.size();
    span_indices.resize(group_span_count);
    std::iota(span_indices.begin(), span_indices.end(), 0);

    std::sort(span_indices.begin(), span_indices.end(), [&](size_t i, size_t j) {
      if (temp_group_starts[i] != temp_group_starts[j]) {
        return temp_group_starts[i] < temp_group_starts[j];
      }
      return temp_group_ends[i] < temp_group_ends[j];
    });

    size_t span_index = span_indices[0];
    double current_start = temp_group_starts[span_index];
    double current_max_end = temp_group_ends[span_index];

    for (size_t i = 1; i < group_span_count; i++) {
      span_index = span_indices[i];
      double next_start = temp_group_starts[span_index];
      double next_end = temp_group_ends[span_index];

      if (next_start <= current_max_end) {
        if (next_end > current_max_end) {
          current_max_end = next_end;
        }
      } else {
        buffer.add_span(current_start, current_max_end);
        current_start = next_start;
        current_max_end = next_end;
      }
    }
    buffer.add_span(current_start, current_max_end);

    // 3. Finalize the squash group set and re-set the temporary starts/ends
    buffer.finish_element();
    temp_group_starts.clear();
    temp_group_ends.clear();
  }

  return buffer.get_results();
}
