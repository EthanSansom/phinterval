#ifndef PHINTERVAL_UTILS_H_
#define PHINTERVAL_UTILS_H_

#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

#define NA_INTERVAL R_NilValue
#define INTERRUPT_N 8192

NumericMatrix empty_interval();
NumericMatrix infinite_interval();
NumericMatrix new_matrix(
    const std::vector<double>& starts,
    const std::vector<double>& ends
);

template <typename F>
List binary_interval_set_op(const List& x, const List& y, F op) {
  int n = x.size();
  List out(n);
  for (int i { 0 }; i < n; ++i) {
    if (i % INTERRUPT_N == 0) {
      checkUserInterrupt();
    }
    if (x[i] == NA_INTERVAL || y[i] == NA_INTERVAL) {
      out[i] = NA_INTERVAL;
    } else {
      out[i] = op(x[i], y[i]);
    }
  }
  return out;
}

template <typename F>
List unary_interval_set_op(const List& x, F op) {
  int n = x.size();
  List out(n);
  for (int i { 0 }; i < n; ++i) {
    if (i % INTERRUPT_N == 0) {
      checkUserInterrupt();
    }
    if (x[i] == NA_INTERVAL) {
      out[i] = NA_INTERVAL;
    } else {
      out[i] = op(x[i]);
    }
  }
  return out;
}

#endif
