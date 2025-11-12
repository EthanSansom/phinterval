#include "utils.h"
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

// TODO: Finalize these (note, hole > phinterval)
// TODO: Create a function used as the backend for xtfrm(), which returns an
//       integer with the same sort order as the phinterval. Maybe don't allow
//       <, <=, >, >=?

bool interval_set_lt(NumericMatrix x, NumericMatrix y) {
  int nx { x.nrow() };
  int ny { y.nrow() };

  // Empty intervals are sorted last
  if (nx == 0) return false;
  if (ny == 0) return true;

  // Compare starts, ends, and ties are broken using the number of intervals
  bool x_shorter { nx < ny };
  int n { x_shorter ? nx : ny };
  for (int i = 0; i < n; ++i) {
    if (x[i] == y[i] || x[i + nx] == y[i + ny]) {
      continue;
    }
    return x[i] < y[i] || x[i + nx] < y[i + ny];
  }
  return x_shorter;
}

bool interval_set_leq(NumericMatrix x, NumericMatrix y) {
  int nx { x.nrow() };
  int ny { y.nrow() };

  // Compare starts, ends, and ties are broken using the number of intervals
  bool x_shorter { nx < ny };
  int n { x_shorter ? nx : ny };
  for (int i = 0; i < n; ++i) {
    if (x[i] == y[i] || x[i + nx] == y[i + ny]) {
      continue;
    }
    return x[i] < y[i] || x[i + nx] < y[i + ny];
  }
  return nx == ny || (nx > 0 && x_shorter);
}

bool interval_set_geq(NumericMatrix x, NumericMatrix y) {
  int nx { x.nrow() };
  int ny { y.nrow() };

  // Compare starts, ends, and ties are broken using the number of intervals
  bool x_longer { nx > ny };
  int n { x_longer ? ny : nx };
  for (int i = 0; i < n; ++i) {
    if (x[i] == y[i] || x[i + nx] == y[i + ny]) {
      continue;
    }
    return x[i] > y[i] || x[i + nx] > y[i + ny];
  }
  return nx == ny || nx == 0 || x_longer;
}

// [[Rcpp::export]]
LogicalVector cpp_interval_sets_lt(const List& x, const List& y) {
  int n = x.size();
  LogicalVector out(n);
  for (int i = 0; i < n; ++i) {
    if (x[i] == NA_INTERVAL || y[i] == NA_INTERVAL) {
      out[i] = NA_LOGICAL;
    } else {
      out[i] = interval_set_lt(x[i], y[i]);
    }
  }
  return out;
}

// [[Rcpp::export]]
LogicalVector cpp_interval_sets_leq(const List& x, const List& y) {
  int n = x.size();
  LogicalVector out(n);
  for (int i = 0; i < n; ++i) {
    if (x[i] == NA_INTERVAL || y[i] == NA_INTERVAL) {
      out[i] = NA_LOGICAL;
    } else {
      out[i] = interval_set_leq(x[i], y[i]);
    }
  }
  return out;
}

// [[Rcpp::export]]
LogicalVector cpp_interval_sets_geq(const List& x, const List& y) {
  int n = x.size();
  LogicalVector out(n);
  for (int i = 0; i < n; ++i) {
    if (x[i] == NA_INTERVAL || y[i] == NA_INTERVAL) {
      out[i] = NA_LOGICAL;
    } else {
      out[i] = interval_set_geq(x[i], y[i]);
    }
  }
  return out;
}
