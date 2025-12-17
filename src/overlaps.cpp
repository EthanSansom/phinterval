#include "overlaps.h"
#include "utils.h"
#include "endpoint.h"
#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector cpp_interval_sets_overlaps(const List& x, const List& y) {
  int n = x.size();
  LogicalVector out(n);
  for (int i { 0 }; i < n; ++i) {
    if (x[i] == NA_INTERVAL || y[i] == NA_INTERVAL) {
      out[i] = NA_LOGICAL;
    } else {
      out[i] = interval_set_overlaps(x[i], y[i]);
    }
  }
  return out;
}

int interval_set_overlaps(NumericMatrix x, NumericMatrix y) {
  int nx = x.nrow();
  int ny = y.nrow();
  if (nx == 0 || ny == 0) return false;

  int i { 0 }, j { 0 };
  while (i < nx && j < ny) {
    double start = std::max(x(i, 0), y(j, 0));
    double end = std::min(x(i, 1), y(j, 1));

    if (start <= end) return true;

    if (x(i, 1) < y(j, 1)) {
      i++;
    } else {
      j++;
    }
  }

  return false;
}
