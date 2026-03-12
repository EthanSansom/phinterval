#include "squash.h"
#include "type-helpers.h"
#include "type-range.h"
#include "type-interval.h"
#include "type-phinterval.h"
#include <algorithm>
#include <vector>
#include <numeric>
#include <Rcpp.h>
using namespace Rcpp;

// squash ----------------------------------------------------------------------

// [[Rcpp::export]]
List phint_squash_cpp(
    IntegerVector size,
    List starts,
    List ends,
    bool na_rm,
    String empty_to
) {
  const R_xlen_t n = size.size();
  if (n == 0) {
    if (empty_to == "hole") return phint_result_hole();
    return phint_result_na();
  }
  if (n == 1) {
    return List::create(
      Named("size") = size,
      Named("starts") = starts,
      Named("ends") = ends
    );
  }

  // First pass: count the total number of spans and test for NA values
  const int* p_size = INTEGER(size);
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

  // We either only hit holes or only hit NA values
  if (total_spans == 0) {
    if (all_na) return phint_result_na();
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

    // Note size_i > 0, `memcpy(dest, src, 0)` can be unsafe on some machines:
    // https://github.com/r-lib/vctrs/blob/94cea16b1ed3939aaa59c58dda75eedc75d6d075/src/rlang/c-utils.h#L180
    std::memcpy(p_starts_buffer + offset, p_starts_i, size_i * sizeof(double));
    std::memcpy(p_ends_buffer + offset, p_ends_i, size_i * sizeof(double));
    offset += size_i;
  }

  // Squash the unlisted spans
  return squash_num_impl(starts_buffer, ends_buffer);
}

// [[Rcpp::export]]
List intvl_squash_cpp(DatetimeVector starts, NumericVector spans, bool na_rm, String empty_to) {
  IntvlVectorView vec { starts, spans };
  return squash_vec_impl(vec, na_rm, empty_to);
}

// [[Rcpp::export]]
List range_squash_cpp(DatetimeVector starts, DatetimeVector ends, bool na_rm, String empty_to) {
  RangeVectorView vec { starts, ends };
  return squash_vec_impl(vec, na_rm, empty_to);
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

  // TODO: Potentially std::vector<numeric>. Benchmark, we might not need to
  // reserve the entire size `n`. We can just `Rcpp::wrap()` for the output.
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

// squash by -------------------------------------------------------------------

// [[Rcpp::export]]
List phint_squash_by_cpp(
    IntegerVector size,
    List starts,
    List ends,
    List group_locs,
    bool na_rm,
    String empty_to
) {
  PhintVectorView vec { size, starts, ends };
  return squash_vec_by_impl(vec, group_locs, na_rm, empty_to);
}

// [[Rcpp::export]]
List intvl_squash_by_cpp(
    DatetimeVector starts,
    NumericVector spans,
    List group_locs,
    bool na_rm,
    String empty_to
) {
  IntvlVectorView vec { starts, spans };
  return squash_vec_by_impl(vec, group_locs, na_rm, empty_to);
}

// [[Rcpp::export]]
List range_squash_by_cpp(
    DatetimeVector starts,
    DatetimeVector ends,
    List group_locs,
    bool na_rm,
    String empty_to
) {
  RangeVectorView vec { starts, ends };
  return squash_vec_by_impl(vec, group_locs, na_rm, empty_to);
}
