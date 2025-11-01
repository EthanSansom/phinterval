#ifndef PHINTERVAL_INSTANTS_H_
#define PHINTERVAL_INSTANTS_H_

#include <Rcpp.h>
using namespace Rcpp;

List cpp_interval_sets_remove_instants(const List& x);
NumericMatrix interval_set_remove_instants(NumericMatrix x);

#endif
