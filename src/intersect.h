#ifndef PHINTERVAL_INTERSECT_H_
#define PHINTERVAL_INTERSECT_H_

#include "endpoint.h"
#include <Rcpp.h>
using namespace Rcpp;

List cpp_intersect_interval_sets(const List& x, const List& y);
NumericMatrix intersect_interval_set(NumericMatrix x, NumericMatrix y);
NumericMatrix intersect(const Endpoints& endpoints);

#endif
