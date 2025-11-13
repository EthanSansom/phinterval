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

  BinaryEndpoints endpoints;
  endpoints.reserve((nx + ny) * 2);

  for (int i { 0 }; i < nx; ++i) {
    endpoints.push_back(BinaryEndpoint { true, true, x[i] });
    endpoints.push_back(BinaryEndpoint { false, true, x[i + nx] });
  }
  for (int i { 0 }; i < ny; ++i) {
    // We don't consider anything to be within an instant (even itself)
    // if (y[i] == y[i + ny]) {
    //  continue;
    // }
    endpoints.push_back(BinaryEndpoint { true, false, y[i] });
    endpoints.push_back(BinaryEndpoint { false, false, y[i + ny] });
  }

  std::sort(endpoints.begin(), endpoints.end());
  return within(endpoints);
}

bool within(const BinaryEndpoints& endpoints) {
  bool within_y { false };
  for (const BinaryEndpoint& endpoint : endpoints) {
    if (endpoint.in_x) {
      // We've encountered an endpoint of `x` while not within `y`
      if (!within_y) return false;
    } else {
      // Encountering a start from `y` indicates we've entered a `y` interval
      // and an end that we've exited.
      within_y = endpoint.is_start;
    }
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

  // `x` is an empty interval of `t` is outside of the maximum start/end of `x`
  if (n == 0 || t < x[0] || t > x[n * 2 - 1]) return false;

  for (int i { 0 }; i < n; ++i) {
    if (x[i] <= t && t <= x[i + n]) return true;
  }
  return false;
}
