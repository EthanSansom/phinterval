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
    endpoints.push_back(BinaryEndpoint { true, false, y[i] });
    endpoints.push_back(BinaryEndpoint { false, false, y[i + ny] });
  }

  std::sort(endpoints.begin(), endpoints.end());
  return within(endpoints);
}

bool within(const BinaryEndpoints& endpoints) {
  bool in_x_interval { false };
  for (const BinaryEndpoint& endpoint : endpoints) {
    if (endpoint.in_x) {
      // Encountering a start from `x` indicates that we've entered an `x`
      // interval and an end indicates that we've exited an `x` interval.
      in_x_interval = endpoint.is_start;
    } else {
      // Encountering any endpoint from `y` while we're not within an `x`
      // interval indicates that `y` is not within `x`.
      if (!in_x_interval) return false;
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
      return NA_LOGICAL;
    } else {
      out[i] = interval_set_contains(x[i], t[i]);
    }
  }
  return out;
}

int interval_set_contains(NumericMatrix x, double t) {
  int n { x.nrow() };
  if (n == 0) return false;

  // Anticipating that most interval sets will contain < 3 intervals, so
  // solving these cases immediately.
  switch(n) {
  case 1: return (x[1] <= t && t <= x[2]);
  case 2: return (x[1] <= t && t <= x[2]) || (x[3] <= t && t <= x[4]);
  }

  // Binary search for the nearest interval start less than or equal to `t`
  int l { 0 };
  int r { n };
  int m { l + ((r - l) / 2) };
  while(l < r) {
    if (x[m] < t) {
      l = m + 1;
    } else {
      r = m;
    }
    m = l + ((r - l) / 2);
  }

  return x[m] <= t && t <= x[m + n];
}
