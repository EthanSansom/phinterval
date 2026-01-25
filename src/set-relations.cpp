#include "fun-relations.h"
#include "type-interval.h"
#include "type-phinterval.h"
#include "type-point.h"
#include "vectorization.h"
#include <Rcpp.h>
using namespace Rcpp;

// within ----------------------------------------------------------------------

// [[Rcpp::export]]
LogicalVector phint_phint_within_cpp(
    IntegerVector x_size, List x_starts, List x_ends,
    IntegerVector y_size, List y_starts, List y_ends
) {
  return phint_relate(
    PhintVector{x_size, x_starts, x_ends},
    PhintVector{y_size, y_starts, y_ends},
    Within{}
  );
}

// [[Rcpp::export]]
LogicalVector phint_intvl_within_cpp(
    IntegerVector x_size, List x_starts, List x_ends,
    DatetimeVector y_starts, NumericVector y_spans
) {
  return phint_relate(
    PhintVector{x_size, x_starts, x_ends},
    IntvlVector{y_starts, y_spans},
    Within{}
  );
}

// [[Rcpp::export]]
LogicalVector intvl_phint_within_cpp(
    DatetimeVector x_starts, NumericVector x_spans,
    IntegerVector y_size, List y_starts, List y_ends
) {
  return phint_relate(
    IntvlVector{x_starts, x_spans},
    PhintVector{y_size, y_starts, y_ends},
    Within{}
  );
}

// [[Rcpp::export]]
LogicalVector intvl_intvl_within_cpp(
    DatetimeVector x_starts, NumericVector x_spans,
    DatetimeVector y_starts, NumericVector y_spans
) {
  return phint_relate(
    IntvlVector{x_starts, x_spans},
    IntvlVector{y_starts, y_spans},
    Within{}
  );
}

// [[Rcpp::export]]
LogicalVector point_phint_within_cpp(
    DatetimeVector x_points,
    IntegerVector y_size, List y_starts, List y_ends
) {
  return phint_relate(
    PointVector{x_points},
    PhintVector{y_size, y_starts, y_ends},
    Within{}
  );
}

// [[Rcpp::export]]
LogicalVector point_intvl_within_cpp(
    DatetimeVector x_points,
    DatetimeVector y_starts, NumericVector y_spans
) {
  return phint_relate(
    PointVector{x_points},
    IntvlVector{y_starts, y_spans},
    Within{}
  );
}

// overlaps --------------------------------------------------------------------

// [[Rcpp::export]]
LogicalVector phint_phint_overlaps_cpp(
    IntegerVector x_size, List x_starts, List x_ends,
    IntegerVector y_size, List y_starts, List y_ends
) {
  return phint_relate(
    PhintVector{x_size, x_starts, x_ends},
    PhintVector{y_size, y_starts, y_ends},
    Overlaps{}
  );
}

// [[Rcpp::export]]
LogicalVector phint_intvl_overlaps_cpp(
    IntegerVector x_size, List x_starts, List x_ends,
    DatetimeVector y_starts, NumericVector y_spans
) {
  return phint_relate(
    PhintVector{x_size, x_starts, x_ends},
    IntvlVector{y_starts, y_spans},
    Overlaps{}
  );
}

// [[Rcpp::export]]
LogicalVector intvl_phint_overlaps_cpp(
    DatetimeVector x_starts, NumericVector x_spans,
    IntegerVector y_size, List y_starts, List y_ends
) {
  return phint_relate(
    IntvlVector{x_starts, x_spans},
    PhintVector{y_size, y_starts, y_ends},
    Overlaps{}
  );
}

// [[Rcpp::export]]
LogicalVector intvl_intvl_overlaps_cpp(
    DatetimeVector x_starts, NumericVector x_spans,
    DatetimeVector y_starts, NumericVector y_spans
) {
  return phint_relate(
    IntvlVector{x_starts, x_spans},
    IntvlVector{y_starts, y_spans},
    Overlaps{}
  );
}
