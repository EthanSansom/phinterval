#include "compliment.h"
#include "endpoint.h"
#include "utils.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List cpp_compliment_interval_sets(const List& x) {
  int n = x.size();
  List out(n);
  for (int i { 0 }; i < n; ++i) {
    out[i] = compliment_interval_set(x[i]);
  }
  return out;
}

// We can take the compliment by populating the output matrix with endpoints of
// `x` shifted such that starts become ends and vice-versa. Some care must be
// taken to handle infinite endpoints.
//
// Bounded                Unbounded (Left)         Unbounded (Both)
// [a, b] -> [-Inf, a]    [-Inf, b] -> [b, c]      [-Inf, b] -> [b, c]
// [c, d]    [b,    c]    [c,    d]    [d, Inf]    [c,  Inf]
//           [d,  Inf]
NumericMatrix compliment_interval_set(NumericMatrix x) {
  int n { x.nrow() };
  if (n == 0) return infinite_interval();
  if (ISNA(x[0])) return x;

  bool left_open { x[0] == R_NegInf || x[0] == R_PosInf };
  bool right_open { x[x.size() - 1] == R_NegInf || x[x.size() - 1] == R_PosInf };
  int n_out { n + 1 - left_open - right_open };

  // These conditions were found by trial and error
  NumericMatrix out(n_out, 2);
  for (int i { 0 }; i < n; ++i) {
    if (!(right_open && i >= n - 1)) out[i + 1 - left_open] = x[i + n];
    if (!(left_open && i <= 0))      out[n_out + i - left_open] = x[i];
  }
  if (!left_open) out[0] = R_NegInf;
  if (!right_open) out[out.size() - 1] = R_PosInf;

  return out;
}
