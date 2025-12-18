#include "utils.h"
#include <Rcpp.h>
#include <vector>
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

NumericMatrix new_matrix(
    const std::vector<double>& starts,
    const std::vector<double>& ends
) {
  int n = starts.size();
  NumericMatrix matrix(n, 2);
  for (int i = 0; i < n; ++i) {
    matrix(i, 0) = starts[i];
    matrix(i, 1) = ends[i];
  }
  return matrix;
}
