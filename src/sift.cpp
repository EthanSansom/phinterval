#include "type-helpers.h"
#include "type-phinterval.h"
#include "type-interval.h"
#include "sift.h"

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List phint_sift_cpp(
  IntegerVector size,
  List starts,
  List ends,
  String action,
  Nullable<NumericVector> min_length,
  Nullable<NumericVector> max_length
) {
  PhintVectorView vec {size, starts, ends};
  return sift_dispatch(vec, action, min_length, max_length);
}

// [[Rcpp::export]]
List intvl_sift_cpp(
  DatetimeVector starts,
  NumericVector spans,
  String action,
  Nullable<NumericVector> min_length,
  Nullable<NumericVector> max_length
) {
  IntvlVectorView vec { starts, spans };
  return sift_dispatch(vec, action, min_length, max_length);
}
