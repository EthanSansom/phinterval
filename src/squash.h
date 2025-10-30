#ifndef PHINTERVAL_SQUASH_H_
#define PHINTERVAL_SQUASH_H_

#include "endpoint.h"
#include <Rcpp.h>
using namespace Rcpp;

NumericMatrix cpp_squash_interval_set(NumericMatrix x);
NumericMatrix squash(const Endpoints& endpoints);

#endif
