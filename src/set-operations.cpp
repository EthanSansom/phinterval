#include "operators.h"
#include "type-interval.h"
#include "type-phinterval.h"
#include "vectorization.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List phint_intersect_cpp(
    IntegerVector x_size, List x_starts, List x_ends,
    IntegerVector y_size, List y_starts, List y_ends
) {
  const PhintVector x = { x_size, x_starts, x_ends };
  const PhintVector y = { y_size, y_starts, y_ends };
  const Intersect intersect;
  return phint_operate(x, y, intersect);
}

// [[Rcpp::export]]
List intvl_intersect_cpp(
    NumericVector x_starts, NumericVector x_ends,
    NumericVector y_starts, NumericVector y_ends
) {
  const IntvlVector x = { x_starts, x_ends };
  const IntvlVector y = { y_starts, y_ends };
  const Intersect intersect;
  return intvl_operate(x, y, intersect);
}
