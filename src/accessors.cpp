#include "accessors.h"
#include "utils.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cpp_interval_sets_start(const List& x) {
  int n = x.size();
  NumericVector out(n);
  for (int i { 0 }; i < n; ++i) {
    if (i % INTERRUPT_N == 0) {
      checkUserInterrupt();
    }
    if (x[i] == NA_INTERVAL) {
      out[i] = NA_REAL;
    } else {
      out[i] = interval_set_start(x[i]);
    }
  }
  return out;
}

// [[Rcpp::export]]
List cpp_interval_sets_starts(const List& x) {
  int n = x.size();
  List out(n);
  for (int i { 0 }; i < n; ++i) {
    if (i % INTERRUPT_N == 0) {
      checkUserInterrupt();
    }
    if (x[i] == NA_INTERVAL) {
      out[i] = NA_REAL;
    } else {
      out[i] = interval_set_starts(x[i]);
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericVector cpp_interval_sets_end(const List& x) {
  int n = x.size();
  NumericVector out(n);
  for (int i { 0 }; i < n; ++i) {
    if (i % INTERRUPT_N == 0) {
      checkUserInterrupt();
    }
    if (x[i] == NA_INTERVAL) {
      out[i] = NA_REAL;
    } else {
      out[i] = interval_set_end(x[i]);
    }
  }
  return out;
}

// [[Rcpp::export]]
List cpp_interval_sets_ends(const List& x) {
  int n = x.size();
  List out(n);
  for (int i { 0 }; i < n; ++i) {
    if (i % INTERRUPT_N == 0) {
      checkUserInterrupt();
    }
    if (x[i] == NA_INTERVAL) {
      out[i] = NA_REAL;
    } else {
      out[i] = interval_set_ends(x[i]);
    }
  }
  return out;
}

double interval_set_start(const NumericMatrix x) {
  return x.nrow() ? x[0] : NA_REAL;
}

NumericVector interval_set_starts(const NumericMatrix x) {
  return x.column(0);
}

double interval_set_end(const NumericMatrix x) {
  return x.nrow() ? x[x.size() - 1] : NA_REAL;
}

NumericVector interval_set_ends(const NumericMatrix x) {
  return x.column(1);
}
