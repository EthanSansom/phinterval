#include "fun-modifiers.h"
#include "type-interval.h"
#include "type-phinterval.h"
#include "vectorization.h"
#include <Rcpp.h>
using namespace Rcpp;

// sift ------------------------------------------------------------------------

// [[Rcpp::export]]
List phint_sift_cpp(IntegerVector size, List starts, List ends) {
  return phint_modify(PhintVectorView{size, starts, ends}, Sift{});
}

// [[Rcpp::export]]
List intvl_sift_cpp(DatetimeVector starts, NumericVector spans) {
  return phint_modify(IntvlVectorView{starts, spans}, Sift{});
}

// complement ------------------------------------------------------------------

// [[Rcpp::export]]
List phint_complement_cpp(IntegerVector size, List starts, List ends) {
  return phint_modify(PhintVectorView{size, starts, ends}, Complement{});
}

// [[Rcpp::export]]
List intvl_complement_cpp(DatetimeVector starts, NumericVector spans) {
  return phint_modify(IntvlVectorView{starts, spans}, Complement{});
}

// invert ----------------------------------------------------------------------

// [[Rcpp::export]]
List phint_invert_cpp(IntegerVector size, List starts, List ends, String hole_to) {
  PhintVectorView vec {size, starts, ends};
  if (hole_to == "hole") {
    return phint_modify(vec, Invert<HoleTo::Hole>{});
  } else if (hole_to == "inf") {
    return phint_modify(vec, Invert<HoleTo::Inf>{});
  } else {
    return phint_modify(vec, Invert<HoleTo::NA>{});
  }
}

// [[Rcpp::export]]
List intvl_invert_cpp(DatetimeVector starts, NumericVector spans, String hole_to) {
  // `hole_to` doesn't impact <Interval> vectors
  return phint_modify(IntvlVectorView{starts, spans}, Invert<HoleTo::Hole>{});
}
