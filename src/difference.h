#ifndef PHINTERVAL_DIFFERENCE_H_
#define PHINTERVAL_DIFFERENCE_H_

#include "endpoint.h"
#include <Rcpp.h>
using namespace Rcpp;

List cpp_setdiff_interval_sets(const List& x, const List& y);
NumericMatrix setdiff_interval_set(NumericMatrix x, NumericMatrix y);
NumericMatrix setdiff(const BinaryEndpoints& endpoints);

#endif
