#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List as_phint_intvl_cpp(DatetimeVector starts, NumericVector spans) {
  const R_xlen_t n = starts.size();

  IntegerVector out_size = no_init(n);
  List out_starts = no_init(n);
  List out_ends = no_init(n);

  const double* p_starts = REAL(starts);
  const double* p_spans = REAL(spans);
  int* p_out_size = INTEGER(out_size);

  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    double start = p_starts[i];
    double span = p_spans[i];

    if (ISNAN(start) || ISNAN(span)) {
      p_out_size[i] = NA_INTEGER;
      SET_VECTOR_ELT(out_starts, i, R_NilValue);
      SET_VECTOR_ELT(out_ends, i, R_NilValue);
      continue;
    }

    p_out_size[i] = 1;
    double end = start + span;
    SET_VECTOR_ELT(out_starts, i, Rf_ScalarReal(std::min(start, end)));
    SET_VECTOR_ELT(out_ends, i, Rf_ScalarReal(std::max(start, end)));
  }

  return List::create(
    Named("size") = out_size,
    Named("starts") = out_starts,
    Named("ends") = out_ends
  );
}
