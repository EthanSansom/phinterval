#include "complement.h"
#include "instants.h"
#include "endpoint.h"
#include "utils.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List cpp_complement_interval_sets(const List& x) {
  int n = x.size();
  List out(n);
  for (int i { 0 }; i < n; ++i) {
    if (x[i] == NA_INTERVAL) {
      out[i] = NA_INTERVAL;
    } else {
      out[i] = complement_interval_set(x[i]);
    }
  }
  return out;
}

// [[Rcpp::export]]
List cpp_invert_interval_sets(const List& x) {
  int n = x.size();
  List out(n);
  for (int i { 0 }; i < n; ++i) {
    if (x[i] == NA_INTERVAL) {
      out[i] = NA_INTERVAL;
    } else {
      out[i] = invert_interval_set(x[i]);
    }
  }
  return out;
}

// We can take the complement by populating the output matrix with endpoints of
// `x` shifted such that starts become ends and vice-versa. Some care must be
// taken to handle infinite endpoints.
//
// Bounded                Unbounded (Left)         Unbounded (Both)
// [a, b] -> [-Inf, a]    [-Inf, b] -> [b, c]      [-Inf, b] -> [b, c]
// [c, d]    [b,    c]    [c,    d]    [d, Inf]    [c,  Inf]
//           [d,  Inf]
NumericMatrix complement_interval_set(NumericMatrix x) {
  // Instants create abutting intervals, which we don't allow. Removing instants
  // creates the desired complement.
  //
  // Bad:  [a, a] -> [-Inf, a], [a, Inf]
  // Good: [] -> [-Inf, Inf]
  NumericMatrix xs { interval_set_remove_instants(x) };
  int n { xs.nrow() };
  if (n == 0) return infinite_interval();

  bool left_open { xs[0] == R_NegInf || xs[0] == R_PosInf };
  bool right_open { xs[xs.size() - 1] == R_NegInf || xs[xs.size() - 1] == R_PosInf };
  int n_out { n + 1 - left_open - right_open };

  // These conditions were found by trial and error
  NumericMatrix out(n_out, 2);
  for (int i { 0 }; i < n; ++i) {
    if (!(right_open && i >= n - 1)) out[i + 1 - left_open] = xs[i + n];
    if (!(left_open && i <= 0))      out[n_out + i - left_open] = xs[i];
  }
  if (!left_open) out[0] = R_NegInf;
  if (!right_open) out[out.size() - 1] = R_PosInf;

  return out;
}

NumericMatrix invert_interval_set(NumericMatrix x) {
  int n { x.nrow() };
  if (n == 0) return x;

  int n_out { n - 1 };
  NumericMatrix out(n_out, 2);
  for (int i { 0 }; i < n_out; ++i) {
    out[i] = x[n + i];
    out[i + n_out] = x[i + 1];
  }

  return out;
}
