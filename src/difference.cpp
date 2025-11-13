#include "difference.h"
#include "utils.h"
#include "endpoint.h"
#include <Rcpp.h>
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
  int nx { x.nrow() };
  int ny { y.nrow() };
  if (nx == 0 || ny == 0) return x;

  BinaryEndpoints endpoints;
  endpoints.reserve((nx + ny) * 2);

  for (int i { 0 }; i < nx; ++i) {
    endpoints.push_back(BinaryEndpoint { true, true, x[i] });
    endpoints.push_back(BinaryEndpoint { false, true, x[i + nx] });
  }
  for (int i { 0 }; i < ny; ++i) {
    // Instantaneous intervals in `y` are discarded, as they don't impact
    // the set difference.
    if (y[i] == y[i + ny]) {
      continue;
    }
    endpoints.push_back(BinaryEndpoint { true, false, y[i] });
    endpoints.push_back(BinaryEndpoint { false, false, y[i + ny] });
  }

  std::sort(endpoints.begin(), endpoints.end(), lt_setdiff);
  return setdiff(endpoints);
}

NumericMatrix setdiff(const BinaryEndpoints& endpoints) {
  std::vector<double> starts, ends;
  starts.reserve(endpoints.size() / 2);
  ends.reserve(endpoints.size() / 2);

  int count { 0 }; // TODO: Remove

  bool within_x { false };
  bool within_y { false };
  for (const BinaryEndpoint& endpoint : endpoints) {
    if (endpoint.in_x) {
      within_x = endpoint.is_start;

      // Entering or exiting `x` while within `y` -> not in set difference
      if (within_y) continue;

      if (within_x) {
        // Entering `x` while not within `y` -> start of an interval
        starts.push_back(endpoint.value);
      } else {
        // Exiting `x` while not within `y`  -> end of an interval
        ends.push_back(endpoint.value);
      }
    } else {
      within_y = endpoint.is_start;

      // Entering or exiting `y` while not within `x` -> not in set difference
      if (!within_x) continue;

      if (within_y) {
        // Entering `y` while within `x` -> end of an interval
        ends.push_back(endpoint.value);
      } else {
        // Exiting `y` while within `x`  -> start of an interval
        starts.push_back(endpoint.value);
      }
    }
  }

  int n = starts.size();
  if (n != ends.size()) {
    stop("Internal error: number of starts does not match number of ends.");
  }
  NumericMatrix out(n, 2);
  std::copy(starts.begin(), starts.end(), out.begin());
  std::copy(ends.begin(), ends.end(), out.begin() + n);

  return out;
}
