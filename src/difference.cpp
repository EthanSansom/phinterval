#include "difference.h"
#include "utils.h"
#include "endpoint.h"
#include <Rcpp.h>
#include <algorithm>
#include <vector>
using namespace Rcpp;

// [[Rcpp::export]]
List cpp_setdiff_interval_sets(const List& x, const List& y) {
  int n = x.size();
  List out(n);
  for (int i { 0 }; i < n; ++i) {
    if (x[i] == NA_INTERVAL || y[i] == NA_INTERVAL) {
      out[i] = NA_INTERVAL;
    } else {
      out[i] = setdiff_interval_set(x[i], y[i]);
    }
  }
  return out;
}

NumericMatrix setdiff_interval_set(NumericMatrix x, NumericMatrix y) {
  int nx = x.nrow();
  int ny = y.nrow();
  if (nx == 0 || ny == 0) return x;

  std::vector<double> starts;
  std::vector<double> ends;
  starts.reserve(nx);
  ends.reserve(nx);

  int j { 0 };
  for (int i { 0 }; i < nx; ++i) {
    double cur_start = x(i, 0);
    double cur_end = x(i, 1);

    // Skip intervals in y which appear before the current x interval
    while (j < ny && y(j, 1) <= cur_start) ++j;

    // Punch holes out of the current x interval using y intervals, until we're
    // past the current x interval
    double s = cur_start;
    while (j < ny && y(j, 0) < cur_end) {
      double y_start = y(j, 0);
      double y_end = y(j, 1);

      if (y_start == y_end) {
        ++j;
        continue;
      }

      if (s < y_start) {
        starts.push_back(s);
        ends.push_back(std::min(cur_end, y_start));
      }
      s = std::max(s, y_end);

      if (s >= cur_end) break; // We're past the end of the current x interval
      ++j;
    }

    // Add the final portion of the x interval after holes have been punched
    if (s < cur_end) {
      starts.push_back(s);
      ends.push_back(cur_end);
    }
  }

  int n = starts.size();
  NumericMatrix out(n, 2);
  for (int i { 0 }; i < n; ++i) {
    out(i, 0) = starts[i];
    out(i, 1) = ends[i];
  }

  return out;
}
