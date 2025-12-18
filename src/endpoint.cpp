#include "endpoint.h"
#include <Rcpp.h>
using namespace Rcpp;

bool operator< (const Endpoint &a, const Endpoint &b) {
  if (a.value == b.value) {
    return a.is_start > b.is_start; // Starts < Ends if values are tied
  }
  return a.value < b.value;
}
