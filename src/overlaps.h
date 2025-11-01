#ifndef PHINTERVAL_OVERLAPS_H_
#define PHINTERVAL_OVERLAPS_H_

#include "endpoint.h"
#include <Rcpp.h>
using namespace Rcpp;

LogicalVector cpp_interval_sets_overlaps(const List& x, const List& y);
int interval_set_overlaps(NumericMatrix x, NumericMatrix y);
bool overlaps(const Endpoints& endpoints);

#endif
