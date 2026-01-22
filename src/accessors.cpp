#include "type-interval.h"
#include "type-phinterval.h"
#include <Rcpp.h>
using namespace Rcpp;

// scalar start/end ------------------------------------------------------------

// [[Rcpp::export]]
DatetimeVector phint_start_cpp(IntegerVector size, List starts, List ends, String tzone) {
  const R_xlen_t n = size.size();
  const int* p_size = INTEGER(size);
  NumericVector out = no_init(n);
  double* p_out = REAL(out);

  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    int size_i = p_size[i];

    if (size_i == NA_INTEGER || size_i == 0) {
      p_out[i] = NA_REAL;
      continue;
    }

    SEXP starts_i = VECTOR_ELT(starts, i);
    const double* p_starts_i = REAL(starts_i);
    p_out[i] = p_starts_i[0];
  }

  DatetimeVector result(out);
  result.attr("tzone") = tzone;
  return result;
}

// [[Rcpp::export]]
DatetimeVector phint_end_cpp(IntegerVector size, List starts, List ends, String tzone) {
  const R_xlen_t n = size.size();
  const int* p_size = INTEGER(size);
  NumericVector out = no_init(n);
  double* p_out = REAL(out);

  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    int size_i = p_size[i];

    if (size_i == NA_INTEGER || size_i == 0) {
      p_out[i] = NA_REAL;
      continue;
    }

    SEXP ends_i = VECTOR_ELT(ends, i);
    const double* p_ends_i = REAL(ends_i);
    p_out[i] = p_ends_i[size_i - 1]; // Adjust for 0-based index
  }

  DatetimeVector result(out);
  result.attr("tzone") = tzone;
  return result;
}

template <typename VectorType>
DatetimeVector scalar_start_impl(const VectorType& vec, String tzone);

template <typename VectorType>
DatetimeVector scalar_end_impl(const VectorType& vec, String tzone);

// [[Rcpp::export]]
DatetimeVector intvl_start_cpp(DatetimeVector starts, NumericVector spans, String tzone) {
  IntvlVector vec { starts, spans };
  return scalar_start_impl(vec, tzone);
}

// [[Rcpp::export]]
DatetimeVector intvl_end_cpp(DatetimeVector starts, NumericVector spans, String tzone) {
  IntvlVector vec { starts, spans };
  return scalar_end_impl(vec, tzone);
}

template <typename VectorType>
DatetimeVector scalar_start_impl(const VectorType& vec, String tzone) {
  const R_xlen_t n = vec.n_sets();
  NumericVector out = no_init(n);
  double* p_out = REAL(out);

  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    auto view = vec.view(i);
    p_out[i] = view.is_na ? NA_REAL : view.start(0);
  }

  DatetimeVector result(out);
  result.attr("tzone") = tzone;
  return result;
}

template <typename VectorType>
DatetimeVector scalar_end_impl(const VectorType& vec, String tzone) {
  const R_xlen_t n = vec.n_sets();
  NumericVector out = no_init(n);
  double* p_out = REAL(out);

  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    auto view = vec.view(i);
    p_out[i] = view.is_na ? NA_REAL : view.end(0);
  }

  DatetimeVector result(out);
  result.attr("tzone") = tzone;
  return result;
}

// list starts/ends ------------------------------------------------------------

// [[Rcpp::export]]
List phint_starts_cpp(IntegerVector size, List starts, List ends, String tzone) {
  const R_xlen_t n = size.size();
  const int* p_size = INTEGER(size);
  List out = no_init(n);

  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    int size_i = p_size[i];

    if (size_i == NA_INTEGER) {
      SET_VECTOR_ELT(out, i, Rf_ScalarReal(NA_REAL));
      continue;
    }

    SEXP starts_i = VECTOR_ELT(starts, i);
    DatetimeVector dt(clone(starts_i));
    dt.attr("tzone") = tzone;
    SET_VECTOR_ELT(out, i, dt);
  }

  return out;
}

// [[Rcpp::export]]
List phint_ends_cpp(IntegerVector size, List starts, List ends, String tzone) {
  const R_xlen_t n = size.size();
  const int* p_size = INTEGER(size);
  List out = no_init(n);

  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    int size_i = p_size[i];

    if (size_i == NA_INTEGER) {
      SET_VECTOR_ELT(out, i, Rf_ScalarReal(NA_REAL));
      continue;
    }

    SEXP ends_i = VECTOR_ELT(ends, i);
    DatetimeVector dt(clone(ends_i));
    dt.attr("tzone") = tzone;
    SET_VECTOR_ELT(out, i, dt);
  }

  return out;
}

// length ----------------------------------------------------------------------

// [[Rcpp::export]]
NumericVector phint_length_cpp(IntegerVector size, List starts, List ends) {
  const R_xlen_t n = size.size();
  const int* p_size = INTEGER(size);
  NumericVector out = no_init(n);
  double* p_out = REAL(out);

  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    int size_i = p_size[i];

    if (size_i == NA_INTEGER) {
      p_out[i] = NA_REAL;
      continue;
    }
    if (size_i == 0) {
      p_out[i] = 0.0;
      continue;
    }

    SEXP starts_i = VECTOR_ELT(starts, i);
    SEXP ends_i = VECTOR_ELT(ends, i);
    const double* p_starts_i = REAL(starts_i);
    const double* p_ends_i = REAL(ends_i);

    double total_length = 0.0;
    for (int k = 0; k < size_i; k++) {
      total_length += p_ends_i[k] - p_starts_i[k];
    }

    p_out[i] = total_length;
  }

  return out;
}

// [[Rcpp::export]]
List phint_lengths_cpp(IntegerVector size, List starts, List ends) {
  const R_xlen_t n = size.size();
  const int* p_size = INTEGER(size);
  List out = no_init(n);

  for (R_xlen_t i = 0; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    int size_i = p_size[i];

    if (size_i == NA_INTEGER) {
      SET_VECTOR_ELT(out, i, Rf_ScalarReal(NA_REAL));
      continue;
    }
    if (size_i == 0) {
      SET_VECTOR_ELT(out, i, Rf_ScalarReal(0.0));
      continue;
    }

    SEXP starts_i = VECTOR_ELT(starts, i);
    SEXP ends_i = VECTOR_ELT(ends, i);
    const double* p_starts_i = REAL(starts_i);
    const double* p_ends_i = REAL(ends_i);

    NumericVector lengths_i = no_init(size_i);
    double* p_lengths_i = REAL(lengths_i);

    for (int k = 0; k < size_i; k++) {
      p_lengths_i[k] = p_ends_i[k] - p_starts_i[k];
    }

    SET_VECTOR_ELT(out, i, lengths_i);
  }

  return out;
}
