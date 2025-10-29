#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List interval_to_interval_sets(const DatetimeVector starts, const NumericVector spans) {
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
