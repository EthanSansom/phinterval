#include "accessors.h"
#include "utils.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cpp_interval_sets_start(const List& x) {
  int n = x.size();
  NumericVector out(n);
  for (int i { 0 }; i < n; ++i) {
    out[i] = interval_set_start(x[i]);
  }
  return out;
}

// [[Rcpp::export]]
List cpp_interval_sets_starts(const List& x) {
  int n = x.size();
  List out(n);
  for (int i { 0 }; i < n; ++i) {
    out[i] = interval_set_starts(x[i]);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector cpp_interval_sets_end(const List& x) {
  int n = x.size();
  NumericVector out(n);
  for (int i { 0 }; i < n; ++i) {
    out[i] = interval_set_end(x[i]);
  }
  return out;
}

// [[Rcpp::export]]
List cpp_interval_sets_ends(const List& x) {
  int n = x.size();
  List out(n);
  for (int i { 0 }; i < n; ++i) {
    out[i] = interval_set_ends(x[i]);
  }
  return out;
}

double interval_set_start(const Nullable<NumericMatrix> x_) {
  if (x_.isNull()) return NA_REAL;
  NumericMatrix x (x_);
  return (x == NA_INTERVAL || !x.nrow()) ? NA_REAL : x[0];
}

NumericVector interval_set_starts(const Nullable<NumericMatrix> x_) {
  if (x_.isNull()) return NumericVector(1, NA_REAL);
  NumericMatrix x (x_);
  return (x == NA_INTERVAL || !x.nrow()) ? NumericVector(1, NA_REAL) : x.column(0);
}

double interval_set_end(const Nullable<NumericMatrix> x_) {
  if (x_.isNull()) return NA_REAL;
  NumericMatrix x (x_);
  return (x == NA_INTERVAL || !x.nrow()) ? NA_REAL : x[x.size() - 1];
}

NumericVector interval_set_ends(const Nullable<NumericMatrix> x_) {
  if (x_.isNull()) return NumericVector(1, NA_REAL);
  NumericMatrix x (x_);
  return (x == NA_INTERVAL || !x.nrow()) ? NumericVector(1, NA_REAL) : x.column(1);
}
