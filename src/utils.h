#ifndef PHINTERVAL_UTILS_H_
#define PHINTERVAL_UTILS_H_

#include <Rcpp.h>
using namespace Rcpp;

NumericMatrix empty_interval();
NumericMatrix na_interval();
NumericMatrix infinite_interval();

bool is_na_interval(NumericMatrix x);

#endif
