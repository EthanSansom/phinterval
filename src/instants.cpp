#include "instants.h"
#include "utils.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List cpp_interval_sets_remove_instants(const List& x) {
  int n = x.size();
  List out(n);
  for (int i { 0 }; i < n; ++i) {
    if (x[i] == NA_INTERVAL) {
      out[i] = NA_INTERVAL;
    } else {
      out[i] = interval_set_remove_instants(x[i]);
    }
  }
  return out;
}

NumericMatrix interval_set_remove_instants(NumericMatrix x) {
  int n { x.nrow() };
  if (n == 0) return x;

  std::vector<int> keep {};
  keep.reserve(n);
  for (int i { 0 }; i < n; ++i) {
    if (x[i] != x[i + n]) keep.push_back(i);
  }

  int n_keep = keep.size();
  if (n_keep == n) return x;

  NumericMatrix out(n_keep, 2);
  for (int i { 0 }; i < n_keep; ++i) {
    out[i] = x[keep[i]];
    out[i + n_keep] = x[keep[i] + n];
  }
  return out;
}
