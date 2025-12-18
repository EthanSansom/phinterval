#ifndef PHINTERVAL_INTERSECT_H_
#define PHINTERVAL_INTERSECT_H_

#include <Rcpp.h>
using namespace Rcpp;

List cpp_intersect_interval_sets(const List& x, const List& y);
NumericMatrix intersect_interval_set(NumericMatrix x, NumericMatrix y);

#endif
