#include "type-phinterval.h"
#include "type-interval.h"
#include "type-helpers.h"
#include <Rcpp.h>
using namespace Rcpp;

template <bool DropHoles, typename VectorType>
DataFrame phint_unnest_impl(const VectorType& vec, String tzone);

// [[Rcpp::export]]
DataFrame phint_unnest_cpp(
    IntegerVector size,
    List starts,
    List ends,
    String tzone,
    String hole_to
) {
  PhintVectorView vec { size, starts, ends };
  if (hole_to == "drop") {
    return phint_unnest_impl<true>(vec, tzone);
  } else {
    return phint_unnest_impl<false>(vec, tzone);
  }
}

// [[Rcpp::export]]
DataFrame intvl_unnest_cpp(
    DatetimeVector starts,
    NumericVector spans,
    String tzone,
    String hole_to
) {
  IntvlVectorView vec { starts, spans };
  if (hole_to == "drop") {
    return phint_unnest_impl<true>(vec, tzone);
  } else {
    return phint_unnest_impl<false>(vec, tzone);
  }
}

template <bool DropHoles, typename VectorType>
DataFrame phint_unnest_impl(const VectorType& vec, String tzone) {
  const R_xlen_t n = vec.n_sets();

  // Count the number of rows in the output
  R_xlen_t total_rows = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    auto view = vec.view(i);

    if (view.is_na) {
      total_rows++;
      continue;
    }

    if (view.is_hole()) {
      if constexpr (DropHoles) {
        continue;
      } else {
        total_rows++;
      }
      continue;
    }

    total_rows += view.size;
  }

  NumericVector out_key = no_init(total_rows);
  NumericVector out_starts = no_init(total_rows);
  NumericVector out_ends = no_init(total_rows);
  double* p_out_key = REAL(out_key);
  double* p_out_starts = REAL(out_starts);
  double* p_out_ends = REAL(out_ends);

  IntegerVector out_size = no_init(total_rows);
  int* p_out_size = INTEGER(out_size);

  R_xlen_t row = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    auto view = vec.view(i);
    double key = static_cast<double>(i) + 1.0;

    if (view.is_na) {
      p_out_key[row] = key;
      p_out_starts[row] = NA_REAL;
      p_out_ends[row] = NA_REAL;
      p_out_size[row] = NA_INTEGER;
      row++;
      continue;
    }

    if (view.is_hole()) {
      if constexpr (DropHoles) {
        continue;
      } else {
        p_out_key[row] = key;
        p_out_starts[row] = NA_REAL;
        p_out_ends[row] = NA_REAL;
        p_out_size[row] = 0;
        row++;
      }
      continue;
    }

    // Requires C++17, skips loop for scalar span types (e.g. not PhintVectorView)
    if constexpr (is_scalar_view<decltype(view)>) {
      p_out_key[row] = key;
      p_out_starts[row] = view.start(0);
      p_out_ends[row] = view.end(0);
      p_out_size[row] = 1;
      row++;
    } else {
      for (int k = 0; k < view.size; k++) {
        p_out_key[row] = key;
        p_out_starts[row] = view.start(k);
        p_out_ends[row] = view.end(k);
        p_out_size[row] = view.size;
        row++;
      }
    }
  }

  DatetimeVector dt_starts(out_starts);
  DatetimeVector dt_ends(out_ends);
  dt_starts.attr("tzone") = tzone;
  dt_ends.attr("tzone") = tzone;

  return DataFrame::create(
    Named("key") = out_key,
    Named("start") = dt_starts,
    Named("end") = dt_ends,
    Named("size") = out_size
  );
}
