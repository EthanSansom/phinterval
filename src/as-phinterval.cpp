#include "type-interval.h"
#include "type-datetime.h"
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

template <typename VectorType>
List as_phint_impl(const VectorType& vec);

// [[Rcpp::export]]
List as_phint_intvl_cpp(DatetimeVector starts, NumericVector spans) {
  IntvlVector vec { starts, spans };
  return as_phint_impl(vec);
}

// [[Rcpp::export]]
List as_phint_datetime_cpp(DatetimeVector starts, DatetimeVector ends) {
  DtimeVector vec { starts, ends };
  return as_phint_impl(vec);
}

template <typename VectorType>
List as_phint_impl(const VectorType& vec) {
  const R_xlen_t n = vec.n_sets();

  IntegerVector out_size = no_init(n);
  List out_starts = no_init(n);
  List out_ends = no_init(n);
  int* p_out_size = INTEGER(out_size);

  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    auto view = vec.view(i);

    if (view.is_na) {
      p_out_size[i] = NA_INTEGER;
      SET_VECTOR_ELT(out_starts, i, R_NilValue);
      SET_VECTOR_ELT(out_ends, i, R_NilValue);
      continue;
    }

    p_out_size[i] = 1;
    SET_VECTOR_ELT(out_starts, i, Rf_ScalarReal(view.start(0)));
    SET_VECTOR_ELT(out_ends, i, Rf_ScalarReal(view.end(0)));
  }

  return List::create(
    Named("size") = out_size,
    Named("starts") = out_starts,
    Named("ends") = out_ends
  );
}
