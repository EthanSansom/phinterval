#ifndef PHINTERVAL_UNION_H_
#define PHINTERVAL_UNION_H_

#include <Rcpp.h>
using namespace Rcpp;

List cpp_union_interval_sets(const List& x, const List& y);
NumericMatrix union_interval_set(NumericMatrix x, NumericMatrix y);

#endif
