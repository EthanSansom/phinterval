#include "operators.h"
#include "type-interval.h"
#include "type-phinterval.h"
#include "vectorization.h"
#include <Rcpp.h>
using namespace Rcpp;

#define PHINT_X_ARGS IntegerVector x_size, List x_starts, List x_ends
#define PHINT_Y_ARGS IntegerVector y_size, List y_starts, List y_ends
#define INTVL_X_ARGS NumericVector x_starts, NumericVector x_ends
#define INTVL_Y_ARGS NumericVector y_starts, NumericVector y_ends

#define PHINT_X_INIT const PhintVector x { x_size, x_starts, x_ends }
#define PHINT_Y_INIT const PhintVector y { y_size, y_starts, y_ends }
#define INTVL_X_INIT const IntvlVector x { x_starts, x_ends }
#define INTVL_Y_INIT const IntvlVector y { y_starts, y_ends }

// All binary operations are generated for every combination of
// PhintVector and IntvlVector.
#define PHINT_BINARY_OP(f_name, x_args, y_args, x_init, y_init, op) \
  List f_name(x_args, y_args) {                                     \
      x_init;                                                       \
      y_init;                                                       \
      return phint_operate(x, y, op{});                             \
  }                                                                 \

// [[Rcpp::export]]
PHINT_BINARY_OP(
  phint_phint_intersect_cpp,
  PHINT_X_ARGS, PHINT_Y_ARGS, PHINT_X_INIT, PHINT_Y_INIT, Intersect
)

// [[Rcpp::export]]
PHINT_BINARY_OP(
  phint_intvl_intersect_cpp,
  PHINT_X_ARGS, INTVL_Y_ARGS, PHINT_X_INIT, INTVL_Y_INIT, Intersect
)

// [[Rcpp::export]]
PHINT_BINARY_OP(
  intvl_phint_intersect_cpp,
  INTVL_X_ARGS, PHINT_Y_ARGS, INTVL_X_INIT, PHINT_Y_INIT, Intersect
)

// [[Rcpp::export]]
PHINT_BINARY_OP(
  intvl_intvl_intersect_cpp,
  INTVL_X_ARGS, INTVL_Y_ARGS, INTVL_X_INIT, INTVL_Y_INIT, Intersect
)
