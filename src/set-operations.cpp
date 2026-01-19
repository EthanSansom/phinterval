#include "operators.h"
#include "type-interval.h"
#include "type-phinterval.h"
#include "vectorization.h"
#include <Rcpp.h>
using namespace Rcpp;

// intersect -------------------------------------------------------------------

// [[Rcpp::export]]
List phint_phint_intersect_cpp(
    IntegerVector x_size, List x_starts, List x_ends,
    IntegerVector y_size, List y_starts, List y_ends
) {
  return phint_operate(
    PhintVector{x_size, x_starts, x_ends},
    PhintVector{y_size, y_starts, y_ends},
    Intersect{}
  );
}

// [[Rcpp::export]]
List phint_intvl_intersect_cpp(
    IntegerVector x_size, List x_starts, List x_ends,
    NumericVector y_starts, NumericVector y_ends
) {
  return phint_operate(
    PhintVector{x_size, x_starts, x_ends},
    IntvlVector{y_starts, y_ends},
    Intersect{}
  );
}

// [[Rcpp::export]]
List intvl_phint_intersect_cpp(
    NumericVector x_starts, NumericVector x_ends,
    IntegerVector y_size, List y_starts, List y_ends
) {
  return phint_operate(
    IntvlVector{x_starts, x_ends},
    PhintVector{y_size, y_starts, y_ends},
    Intersect{}
  );
}

// [[Rcpp::export]]
List intvl_intvl_intersect_cpp(
    NumericVector x_starts, NumericVector x_ends,
    NumericVector y_starts, NumericVector y_ends
) {
  return phint_operate(
    IntvlVector{x_starts, x_ends},
    IntvlVector{y_starts, y_ends},
    Intersect{}
  );
}

// union -----------------------------------------------------------------------

// setdiff ---------------------------------------------------------------------

// symmetric setdiff -----------------------------------------------------------
