#include "intersect.h"
#include "utils.h"
#include <Rcpp.h>
#include <algorithm>
#include <vector>
using namespace Rcpp;

// [[Rcpp::export]]
List cpp_intersect_interval_sets(const List& x, const List& y) {
  int n = x.size();
  List out(n);
  for (int i { 0 }; i < n; ++i) {
    if (x[i] == NA_INTERVAL || y[i] == NA_INTERVAL) {
      out[i] = NA_INTERVAL;
    } else {
      out[i] = intersect_interval_set(x[i], y[i]);
    }
  }
  return out;
}

NumericMatrix intersect_interval_set(NumericMatrix x, NumericMatrix y) {
  int nx = x.nrow();
  int ny = y.nrow();

  if (nx == 0 || ny == 0) return empty_interval();

  std::vector<double> starts;
  std::vector<double> ends;
  starts.reserve(std::min(nx, ny));
  ends.reserve(std::min(nx, ny));

  int i { 0 }, j { 0 };
  while (i < nx && j < ny) {
    double start = std::max(x(i, 0), y(j, 0));
    double end = std::min(x(i, 1), y(j, 1));

    if (start <= end) {
      starts.push_back(start);
      ends.push_back(end);
    }

    if (x(i, 1) < y(j, 1)) {
      i++;
    } else {
      j++;
    }
  }

  int n = starts.size();
  NumericMatrix out(n, 2);
  for (int i = 0; i < n; ++i) {
    out(i, 0) = starts[i];
    out(i, 1) = ends[i];
  }

  return out;
}
