#include "intersect.h"
#include "utils.h"
#include <Rcpp.h>
#include <algorithm>
#include <vector>
using namespace Rcpp;

// [[Rcpp::export]]
List cpp_intersect_interval_sets(const List& x, const List& y) {
  return binary_interval_set_op(x, y, intersect_interval_set);
}

NumericMatrix intersect_interval_set(NumericMatrix x, NumericMatrix y) {
  int nx = x.nrow();
  int ny = y.nrow();
  if (nx == 0 || ny == 0) return empty_interval();

  std::vector<double> starts, ends;
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

  return new_matrix(starts, ends);
}
