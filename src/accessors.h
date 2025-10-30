#ifndef ACCESSORS_COMPLIMENT_H_
#define ACCESSORS_COMPLIMENT_H_

#include <Rcpp.h>
using namespace Rcpp;

NumericVector cpp_interval_sets_start(const List& x);
List cpp_interval_sets_starts(const List& x);
NumericVector cpp_interval_sets_end(const List& x);
List cpp_interval_sets_ends(const List& x);

double interval_set_start(const Nullable<NumericMatrix> x_);
NumericVector interval_set_starts(const Nullable<NumericMatrix> x_);
double interval_set_end(const Nullable<NumericMatrix> x_);
NumericVector interval_set_ends(const Nullable<NumericMatrix> x_);

#endif
