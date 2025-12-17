#include "within.h"
#include "utils.h"
#include "endpoint.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector cpp_interval_sets_within(const List& x, const List& y) {
  int n = x.size();
  LogicalVector out(n);
  for (int i { 0 }; i < n; ++i) {
    if (x[i] == NA_INTERVAL || y[i] == NA_INTERVAL) {
      out[i] = NA_LOGICAL;
    } else {
      out[i] = interval_set_within(x[i], y[i]);
    }
  }
  return out;
}

int interval_set_within(NumericMatrix x, NumericMatrix y) {
  int nx { x.nrow() };
  int ny { y.nrow() };
  if (nx == 0 || ny == 0) return false;

  int j = 0;
  for (int i = 0; i < nx; ++i) {
    double x_start = x(i, 0);
    double x_end   = x(i, 1);

    while (j < ny && y(j, 1) < x_start) ++j;

    if (j == ny) return false;

    double y_start = y(j, 0);
    double y_end = y(j, 1);
    if (x_start < y_start || x_end > y_end) return false;
  }

  return true;
}

// [[Rcpp::export]]
LogicalVector cpp_interval_sets_contains(const List& x, NumericVector t) {
  int n = x.size();
  LogicalVector out(n);
  for (int i { 0 }; i < n; ++i) {
    if (x[i] == NA_INTERVAL || ISNAN(t[i])) {
      out[i] = NA_LOGICAL;
    } else {
      out[i] = interval_set_contains(x[i], t[i]);
    }
  }
  return out;
}

int interval_set_contains(NumericMatrix x, double t) {
  int n { x.nrow() };
  if (n == 0 || t > x(n - 1, 1)) return false;

  int i { 0 };
  while (t > x(i, 1)) ++i;

  return x(i, 0) <= t;
}
