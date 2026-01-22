#include "type-phinterval.h"
#include "type-interval.h"
#include "type-helpers.h"
#include <Rcpp.h>
using namespace Rcpp;

template <bool KeepSize, bool DropHoles, typename VectorType>
DataFrame phint_unnest_impl(const VectorType& vec, String tzone);

// [[Rcpp::export]]
DataFrame phint_unnest_cpp(
    IntegerVector size,
    List starts,
    List ends,
    String tzone,
    String hole_to,
    bool keep_size
) {
  PhintVector vec { size, starts, ends };
  bool drop_holes = (hole_to == "drop");

  if (keep_size && drop_holes) {
    return phint_unnest_impl<true, true>(vec, tzone);
  } else if (keep_size && !drop_holes) {
    return phint_unnest_impl<true, false>(vec, tzone);
  } else if (!keep_size && drop_holes) {
    return phint_unnest_impl<false, true>(vec, tzone);
  } else {
    return phint_unnest_impl<false, false>(vec, tzone);
  }
}

// [[Rcpp::export]]
DataFrame intvl_unnest_cpp(
    DatetimeVector starts,
    NumericVector spans,
    String tzone,
    String hole_to,
    bool keep_size
) {
  IntvlVector vec { starts, spans };
  bool drop_holes = (hole_to == "drop");

  if (keep_size && drop_holes) {
    return phint_unnest_impl<true, true>(vec, tzone);
  } else if (keep_size && !drop_holes) {
    return phint_unnest_impl<true, false>(vec, tzone);
  } else if (!keep_size && drop_holes) {
    return phint_unnest_impl<false, true>(vec, tzone);
  } else {
    return phint_unnest_impl<false, false>(vec, tzone);
  }
}

template <bool KeepSize, bool DropHoles, typename VectorType>
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

    if (view.is_empty()) {
      if constexpr (DropHoles) {
        continue;
      } else {
        total_rows++;
      }
      continue;
    }

    total_rows += view.size;
  }

  // Allocate exact size needed, `out_key` is numeric to support longer vectors
  NumericVector out_key = no_init(total_rows);
  NumericVector out_starts = no_init(total_rows);
  NumericVector out_ends = no_init(total_rows);
  double* p_out_key = REAL(out_key);
  double* p_out_starts = REAL(out_starts);
  double* p_out_ends = REAL(out_ends);

  IntegerVector out_size = KeepSize ? no_init(total_rows) : IntegerVector();
  int* p_out_size = KeepSize ? INTEGER(out_size) : nullptr;

  R_xlen_t row = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    auto view = vec.view(i);
    double key = static_cast<double>(i) + 1.0;

    if (view.is_na) {
      p_out_key[row] = key;
      p_out_starts[row] = NA_REAL;
      p_out_ends[row] = NA_REAL;
      if constexpr (KeepSize) {
        p_out_size[row] = NA_INTEGER;
      }
      row++;
      continue;
    }

    if (view.is_empty()) {
      if constexpr (DropHoles) {
        continue;
      } else {
        p_out_key[row] = key;
        p_out_starts[row] = NA_REAL;
        p_out_ends[row] = NA_REAL;
        if constexpr (KeepSize) {
          p_out_size[row] = 0;
        }
        row++;
      }
      continue;
    }

    // Requires C++17, skips loop for scalar span types (e.g. not PhintVector)
    if constexpr (is_scalar_view<decltype(view)>) {
      p_out_key[row] = key;
      p_out_starts[row] = view.start(0);
      p_out_ends[row] = view.end(0);
      if constexpr (KeepSize) {
        p_out_size[row] = 1;
      }
      row++;
    } else {
      for (int k = 0; k < view.size; k++) {
        p_out_key[row] = key;
        p_out_starts[row] = view.start(k);
        p_out_ends[row] = view.end(k);
        if constexpr (KeepSize) {
          p_out_size[row] = view.size;
        }
        row++;
      }
    }
  }

  DatetimeVector dt_starts(out_starts);
  DatetimeVector dt_ends(out_ends);
  dt_starts.attr("tzone") = tzone;
  dt_ends.attr("tzone") = tzone;

  if constexpr (KeepSize) {
    return DataFrame::create(
      Named("key") = out_key,
      Named("start") = dt_starts,
      Named("end") = dt_ends,
      Named("size") = out_size
    );
  } else {
    return DataFrame::create(
      Named("key") = out_key,
      Named("start") = dt_starts,
      Named("end") = dt_ends
    );
  }
}
