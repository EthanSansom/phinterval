#include "squash.h"
#include "endpoint.h"
#include "utils.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix cpp_squash_interval_set(const NumericMatrix x) {
  int n { x.nrow() };
  if (n <= 1) return x;

  Endpoints endpoints;
  endpoints.reserve(n * 2);

  for (int i { 0 }; i < n; ++i) {
    endpoints.push_back(Endpoint { true, x[i] });
    endpoints.push_back(Endpoint { false, x[i + n] });
  }

  std::sort(endpoints.begin(), endpoints.end());
  return squash(endpoints);
}

NumericMatrix squash(const Endpoints& endpoints) {
  // There are at most `endpoints.size() / 2` intervals in the case where every
  // interval is disjoint.
  std::vector<double> starts, ends;
  starts.reserve(endpoints.size() / 2);
  ends.reserve(endpoints.size() / 2);

  int score { 0 };
  for (const Endpoint& endpoint : endpoints) {
    if (endpoint.is_start) {
      // We're at the first start of a union
      if (score == 0) {
        starts.push_back(endpoint.value);
      }
      ++score;
    } else {
      // We're at the last end of a union
      if (score == 1) {
        ends.push_back(endpoint.value);
      }
      --score;
    }
  }

  int n = starts.size();
  NumericMatrix out(n, 2);
  std::copy(starts.begin(), starts.end(), out.begin());
  std::copy(ends.begin(), ends.end(), out.begin() + n);

  return out;
}
