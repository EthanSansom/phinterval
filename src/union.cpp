#include "union.h"
#include "squash.h"
#include "utils.h"
#include "endpoint.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List cpp_union_interval_sets(const List& x, const List& y) {
  int n = x.size();
  List out(n);
  for (int i { 0 }; i < n; ++i) {
    out[i] = union_interval_set(x[i], y[i]);
  }
  return out;
}

NumericMatrix union_interval_set(NumericMatrix x, NumericMatrix y) {
  int nx { x.nrow() };
  int ny { y.nrow() };
  if (nx == 0) return y;
  if (ny == 0) return x;
  if (is_na_interval(x)) return x;
  if (is_na_interval(y)) return y;

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
  return squash(endpoints);
}
