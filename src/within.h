#ifndef PHINTERVAL_WITHIN_H_
#define PHINTERVAL_WITHIN_H_

#include "endpoint.h"
#include <Rcpp.h>
using namespace Rcpp;

LogicalVector cpp_interval_sets_within(const List& x, const List& y);
int interval_set_within(NumericMatrix x, NumericMatrix y);
bool within(const BinaryEndpoints& endpoints);

LogicalVector cpp_interval_sets_contains(const List& x, const List& y);
int interval_set_contains(NumericMatrix x, double t);

#endif
