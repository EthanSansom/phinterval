#ifndef PHINTERVAL_UTILS_H_
#define PHINTERVAL_UTILS_H_

#include <Rcpp.h>
using namespace Rcpp;

#define NA_INTERVAL R_NilValue

template <typename F>
List pairwise_interval_set_op(const List& x, const List& y, F op) {
  int nx = x.size();
  int ny = y.size();

  // Recycle x to the length of y
  if (nx == 1) {
    List out(ny);
    if (x[0] == NA_INTERVAL) {
      std::fill(out.begin(), out.end(), NA_INTERVAL);
      return out;
    }

    NumericMatrix x0 = x[0];
    for (int i = 0; i < ny; ++i) {
      if (y[i] == NA_INTERVAL) {
        out[i] = NA_INTERVAL;
      } else {
        out[i] = op(x0, y[i]);
      }
    }
    return out;
  }

  // Recycle y to the length of x
  if (ny == 1) {
    List out(nx);
    if (y[0] == NA_INTERVAL) {
      std::fill(out.begin(), out.end(), NA_INTERVAL);
      return out;
    }
    NumericMatrix y0 = y[0];
    for (int i = 0; i < nx; ++i) {
      if (x[i] == NA_INTERVAL) {
        out[i] = NA_INTERVAL;
      } else {
        out[i] = op(x[i], y0);
      }
    }
    return out;
  }

  List out(nx);
  for (int i { 0 }; i < nx; ++i) {
    if (x[i] == NA_INTERVAL || y[i] == NA_INTERVAL) {
      out[i] = NA_INTERVAL;
    } else {
      out[i] = op(x[i], y[i]);
    }
  }
  return out;
}

NumericMatrix empty_interval();
NumericMatrix infinite_interval();

bool is_empty_interval(const NumericMatrix x);

#endif
