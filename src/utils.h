#ifndef PHINTERVAL_UTILS_H_
#define PHINTERVAL_UTILS_H_

#include <Rcpp.h>
using namespace Rcpp;

#define NA_INTERVAL R_NilValue

NumericMatrix empty_interval();
NumericMatrix infinite_interval();

#endif
