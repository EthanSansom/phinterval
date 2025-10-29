#include "squash.h"
#include "endpoint.h"
#include "utils.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix cpp_squash_lubridate_interval(
    const DatetimeVector& starts,
    const NumericVector& spans,
    bool na_rm
) {
  int n = starts.size();

  Endpoints endpoints;
  endpoints.reserve(n * 2);
  double start;
  double span;
  for (int i { 0 }; i < n; ++i) {
    start = starts[i];
    span = spans[i];
    if (na_rm && (ISNAN(start) || ISNAN(span))) continue;
    if (!na_rm && (ISNAN(start) || ISNAN(span))) return na_interval();

    if (span < 0) {
      endpoints.push_back(Endpoint { true, start + span });
      endpoints.push_back(Endpoint { false, start });
    } else {
      endpoints.push_back(Endpoint { true, start });
      endpoints.push_back(Endpoint { false, start + span });
    }
  }

  // Case where `na_rm` and we've only encountered NA values
  if (endpoints.empty()) return na_interval();

  std::sort(endpoints.begin(), endpoints.end());
  return squash(endpoints);
}

// [[Rcpp::export]]
List lubridate_interval_to_interval_sets(
    const DatetimeVector& starts,
    const NumericVector& spans
  ) {
  int n = spans.size();
  List out(n);

  NumericMatrix element(1, 2);
  double start;
  double span;
  for (int i { 0 }; i < n; ++i) {
    start = starts[i];
    span = spans[i];
    if (ISNAN(start) || ISNAN(span)) {
      element(0, 0) = NA_REAL;
      element(0, 1) = NA_REAL;
    } else if (span < 0) {
      element(0, 0) = start + span;
      element(0, 1) = start;
    } else {
      element(0, 0) = start;
      element(0, 1) = span + start;
    }
    out[i] = clone(element);
  }

  return out;
}
