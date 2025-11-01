#include "overlaps.h"
#include "utils.h"
#include "endpoint.h"
#include <Rcpp.h>
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
  int nx { x.nrow() };
  int ny { y.nrow() };
  if (nx == 0 || ny == 0) return false;
  if (ISNA(x[0]) || ISNA(y[0])) return NA_LOGICAL;

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
  return overlaps(endpoints);
}

bool overlaps(const Endpoints& endpoints) {
  int score { 0 };
  for (const Endpoint& endpoint : endpoints) {
    if (endpoint.is_start) {
      ++score;
    } else {
      --score;
    }
    if (score > 1) {
      return true;
    }
  }
  return false;
}
