#ifndef PHINTERVAL_ACCESSORS_H_
#define PHINTERVAL_ACCESSORS_H_

#include <Rcpp.h>
using namespace Rcpp;

NumericVector cpp_interval_sets_start(const List& x);
List cpp_interval_sets_starts(const List& x);
NumericVector cpp_interval_sets_end(const List& x);
List cpp_interval_sets_ends(const List& x);

double interval_set_start(const NumericMatrix x);
NumericVector interval_set_starts(const NumericMatrix x);
double interval_set_end(const NumericMatrix x);
NumericVector interval_set_ends(const NumericMatrix x);

#endif
