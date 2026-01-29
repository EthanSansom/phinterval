#include "fun-operators.h"
#include "type-interval.h"
#include "type-phinterval.h"
#include "vectorization.h"
#include <Rcpp.h>
using namespace Rcpp;

// intersect -------------------------------------------------------------------

// [[Rcpp::export]]
List phint_phint_intersect_cpp(
    IntegerVector x_size, List x_starts, List x_ends,
    IntegerVector y_size, List y_starts, List y_ends,
    String bounds
) {
  PhintVectorView x {x_size, x_starts, x_ends};
  PhintVectorView y {y_size, y_starts, y_ends};
  if (bounds == "[]") {
    return phint_operate(x, y, Intersect<true>{});
  } else {
    return phint_operate(x, y, Intersect<false>{});
  }
}

// [[Rcpp::export]]
List phint_intvl_intersect_cpp(
    IntegerVector x_size, List x_starts, List x_ends,
    DatetimeVector y_starts, NumericVector y_spans,
    String bounds
) {
  PhintVectorView x {x_size, x_starts, x_ends};
  IntvlVectorView y {y_starts, y_spans};
  if (bounds == "[]") {
    return phint_operate(x, y, Intersect<true>{});
  } else {
    return phint_operate(x, y, Intersect<false>{});
  }
}

// [[Rcpp::export]]
List intvl_phint_intersect_cpp(
    DatetimeVector x_starts, NumericVector x_spans,
    IntegerVector y_size, List y_starts, List y_ends,
    String bounds
) {
  IntvlVectorView x {x_starts, x_spans};
  PhintVectorView y {y_size, y_starts, y_ends};
  if (bounds == "[]") {
    return phint_operate(x, y, Intersect<true>{});
  } else {
    return phint_operate(x, y, Intersect<false>{});
  }
}

// [[Rcpp::export]]
List intvl_intvl_intersect_cpp(
    DatetimeVector x_starts, NumericVector x_spans,
    DatetimeVector y_starts, NumericVector y_spans,
    String bounds
) {
  IntvlVectorView x {x_starts, x_spans};
  IntvlVectorView y {y_starts, y_spans};
  if (bounds == "[]") {
    return phint_operate(x, y, Intersect<true>{});
  } else {
    return phint_operate(x, y, Intersect<false>{});
  }
}

// union -----------------------------------------------------------------------

// [[Rcpp::export]]
List phint_phint_union_cpp(
    IntegerVector x_size, List x_starts, List x_ends,
    IntegerVector y_size, List y_starts, List y_ends
) {
  return phint_operate(
    PhintVectorView{x_size, x_starts, x_ends},
    PhintVectorView{y_size, y_starts, y_ends},
    Union{}
  );
}

// [[Rcpp::export]]
List phint_intvl_union_cpp(
    IntegerVector x_size, List x_starts, List x_ends,
    DatetimeVector y_starts, NumericVector y_spans
) {
  return phint_operate(
    PhintVectorView{x_size, x_starts, x_ends},
    IntvlVectorView{y_starts, y_spans},
    Union{}
  );
}

// [[Rcpp::export]]
List intvl_phint_union_cpp(
    DatetimeVector x_starts, NumericVector x_spans,
    IntegerVector y_size, List y_starts, List y_ends
) {
  return phint_operate(
    IntvlVectorView{x_starts, x_spans},
    PhintVectorView{y_size, y_starts, y_ends},
    Union{}
  );
}

// [[Rcpp::export]]
List intvl_intvl_union_cpp(
    DatetimeVector x_starts, NumericVector x_spans,
    DatetimeVector y_starts, NumericVector y_spans
) {
  return phint_operate(
    IntvlVectorView{x_starts, x_spans},
    IntvlVectorView{y_starts, y_spans},
    Union{}
  );
}

// setdiff ---------------------------------------------------------------------

// [[Rcpp::export]]
List phint_phint_setdiff_cpp(
    IntegerVector x_size, List x_starts, List x_ends,
    IntegerVector y_size, List y_starts, List y_ends
) {
  return phint_operate(
    PhintVectorView{x_size, x_starts, x_ends},
    PhintVectorView{y_size, y_starts, y_ends},
    Setdiff{}
  );
}

// [[Rcpp::export]]
List phint_intvl_setdiff_cpp(
    IntegerVector x_size, List x_starts, List x_ends,
    DatetimeVector y_starts, NumericVector y_spans
) {
  return phint_operate(
    PhintVectorView{x_size, x_starts, x_ends},
    IntvlVectorView{y_starts, y_spans},
    Setdiff{}
  );
}

// [[Rcpp::export]]
List intvl_phint_setdiff_cpp(
    DatetimeVector x_starts, NumericVector x_spans,
    IntegerVector y_size, List y_starts, List y_ends
) {
  return phint_operate(
    IntvlVectorView{x_starts, x_spans},
    PhintVectorView{y_size, y_starts, y_ends},
    Setdiff{}
  );
}

// [[Rcpp::export]]
List intvl_intvl_setdiff_cpp(
    DatetimeVector x_starts, NumericVector x_spans,
    DatetimeVector y_starts, NumericVector y_spans
) {
  return phint_operate(
    IntvlVectorView{x_starts, x_spans},
    IntvlVectorView{y_starts, y_spans},
    Setdiff{}
  );
}
