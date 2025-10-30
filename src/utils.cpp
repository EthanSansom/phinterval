#include "utils.h"
#include <Rcpp.h>
using namespace Rcpp;

NumericMatrix empty_interval() {
  NumericMatrix matrix(0, 2);
  return matrix;
}

NumericMatrix infinite_interval() {
  NumericMatrix matrix(1, 2);
  matrix(0, 0) = R_NegInf;
  matrix(0, 1) = R_PosInf;
  return matrix;
}

bool is_empty_interval(NumericMatrix x) {
  return x.nrow();
}
