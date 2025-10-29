#include "utils.h"
#include <Rcpp.h>
using namespace Rcpp;

NumericMatrix empty_interval() {
  NumericMatrix matrix(1, 2);
  return matrix;
}

NumericMatrix na_interval() {
  NumericMatrix matrix(1, 2);
  matrix(0, 0) = NA_REAL;
  matrix(0, 1) = NA_REAL;
  return matrix;
}

NumericMatrix infinite_interval() {
  NumericMatrix matrix(1, 2);
  matrix(0, 0) = R_NegInf;
  matrix(0, 1) = R_PosInf;
  return matrix;
}

bool is_na_interval(const NumericMatrix x) {
  return ISNA(x[0]);
}
