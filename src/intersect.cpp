#include "intersect.h"
#include "utils.h"
#include "endpoint.h"
#include <Rcpp.h>
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
  int nx { x.nrow() };
  int ny { y.nrow() };
  if (nx == 0 || ny == 0) return empty_interval();

  Endpoints endpoints;
  endpoints.reserve((nx + ny) * 2);

  for (int i { 0 }; i < nx; ++i) {
    endpoints.push_back(Endpoint { true, x[i] });
    endpoints.push_back(Endpoint { false, x[i + nx] });
  }
  for (int i { 0 }; i < ny; ++i) {
    endpoints.push_back(Endpoint { true, y[i] });
    endpoints.push_back(Endpoint { false, y[i + ny] });
  }

  std::sort(endpoints.begin(), endpoints.end());
  return intersect(endpoints);
}

// At most two intervals will be intersecting, so we don't need to worry about
// intersections of 3+ intervals.
NumericMatrix intersect(const Endpoints& endpoints) {
  std::vector<double> starts, ends;
  starts.reserve(endpoints.size() / 2);
  ends.reserve(endpoints.size() / 2);

  int score { 0 };
  for (const Endpoint& endpoint : endpoints) {
    if (endpoint.is_start) {
      ++score;
      // We've entered an intersection
      if (score == 2) {
        starts.push_back(endpoint.value);
      }
    } else {
      --score;
      // We've exited an intersection
      if (score == 1) {
        ends.push_back(endpoint.value);
      }
    }
  }

  int n = starts.size();
  NumericMatrix out(n, 2);
  std::copy(starts.begin(), starts.end(), out.begin());
  std::copy(ends.begin(), ends.end(), out.begin() + n);

  return out;
}
