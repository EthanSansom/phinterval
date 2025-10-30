#ifndef PHINTERVAL_COMPLIMENT_H_
#define PHINTERVAL_COMPLIMENT_H_

#include <Rcpp.h>
using namespace Rcpp;

List cpp_compliment_interval_sets(const List& x);
NumericMatrix compliment_interval_set(NumericMatrix x);

#endif
