#ifndef PHINTERVAL_COMPLEMENT_H_
#define PHINTERVAL_COMPLEMENT_H_

#include <Rcpp.h>
using namespace Rcpp;

List cpp_complement_interval_sets(const List& x);
List cpp_invert_interval_sets(const List& x);
NumericMatrix complement_interval_set(NumericMatrix x);
NumericMatrix invert_interval_set(NumericMatrix x);

#endif
