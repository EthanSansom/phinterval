#include "fun-relations.h"
#include "type-interval.h"
#include "type-phinterval.h"
#include "type-point.h"
#include "vectorization.h"
#include <Rcpp.h>
using namespace Rcpp;

// within ----------------------------------------------------------------------

// [[Rcpp::export]]
LogicalVector phint_phint_within_cpp(
    IntegerVector x_size, List x_starts, List x_ends,
    IntegerVector y_size, List y_starts, List y_ends,
    String bounds
) {
  PhintVectorView x {x_size, x_starts, x_ends};
  PhintVectorView y {y_size, y_starts, y_ends};
  if (bounds == "[]") {
    return phint_relate(x, y, Within<true>{});
  } else {
    return phint_relate(x, y, Within<false>{});
  }
}

// [[Rcpp::export]]
LogicalVector phint_intvl_within_cpp(
    IntegerVector x_size, List x_starts, List x_ends,
    DatetimeVector y_starts, NumericVector y_spans,
    String bounds
) {
  PhintVectorView x {x_size, x_starts, x_ends};
  IntvlVectorView y {y_starts, y_spans};
  if (bounds == "[]") {
    return phint_relate(x, y, Within<true>{});
  } else {
    return phint_relate(x, y, Within<false>{});
  }
}

// [[Rcpp::export]]
LogicalVector intvl_phint_within_cpp(
    DatetimeVector x_starts, NumericVector x_spans,
    IntegerVector y_size, List y_starts, List y_ends,
    String bounds
) {
  IntvlVectorView x {x_starts, x_spans};
  PhintVectorView y {y_size, y_starts, y_ends};
  if (bounds == "[]") {
    return phint_relate(x, y, Within<true>{});
  } else {
    return phint_relate(x, y, Within<false>{});
  }
}

// [[Rcpp::export]]
LogicalVector intvl_intvl_within_cpp(
    DatetimeVector x_starts, NumericVector x_spans,
    DatetimeVector y_starts, NumericVector y_spans,
    String bounds
) {
  IntvlVectorView x {x_starts, x_spans};
  IntvlVectorView y {y_starts, y_spans};
  if (bounds == "[]") {
    return phint_relate(x, y, Within<true>{});
  } else {
    return phint_relate(x, y, Within<false>{});
  }
}

// [[Rcpp::export]]
LogicalVector point_phint_within_cpp(
    DatetimeVector x_points,
    IntegerVector y_size, List y_starts, List y_ends,
    String bounds
) {
  PointVectorView x {x_points};
  PhintVectorView y {y_size, y_starts, y_ends};
  if (bounds == "[]") {
    return phint_relate(x, y, Within<true>{});
  } else {
    return phint_relate(x, y, Within<false>{});
  }
}

// [[Rcpp::export]]
LogicalVector point_intvl_within_cpp(
    DatetimeVector x_points,
    DatetimeVector y_starts, NumericVector y_spans,
    String bounds
) {
  PointVectorView x {x_points};
  IntvlVectorView y {y_starts, y_spans};
  if (bounds == "[]") {
    return phint_relate(x, y, Within<true>{});
  } else {
    return phint_relate(x, y, Within<false>{});
  }
}

// overlaps --------------------------------------------------------------------

// [[Rcpp::export]]
LogicalVector phint_phint_overlaps_cpp(
    IntegerVector x_size, List x_starts, List x_ends,
    IntegerVector y_size, List y_starts, List y_ends,
    String bounds
) {
  PhintVectorView x {x_size, x_starts, x_ends};
  PhintVectorView y {y_size, y_starts, y_ends};
  if (bounds == "[]") {
    return phint_relate(x, y, Overlaps<true>{});
  } else {
    return phint_relate(x, y, Overlaps<false>{});
  }
}

// [[Rcpp::export]]
LogicalVector phint_intvl_overlaps_cpp(
    IntegerVector x_size, List x_starts, List x_ends,
    DatetimeVector y_starts, NumericVector y_spans,
    String bounds
) {
  PhintVectorView x {x_size, x_starts, x_ends};
  IntvlVectorView y {y_starts, y_spans};
  if (bounds == "[]") {
    return phint_relate(x, y, Overlaps<true>{});
  } else {
    return phint_relate(x, y, Overlaps<false>{});
  }
}

// [[Rcpp::export]]
LogicalVector intvl_phint_overlaps_cpp(
    DatetimeVector x_starts, NumericVector x_spans,
    IntegerVector y_size, List y_starts, List y_ends,
    String bounds
) {
  IntvlVectorView x {x_starts, x_spans};
  PhintVectorView y {y_size, y_starts, y_ends};
  if (bounds == "[]") {
    return phint_relate(x, y, Overlaps<true>{});
  } else {
    return phint_relate(x, y, Overlaps<false>{});
  }
}

// [[Rcpp::export]]
LogicalVector intvl_intvl_overlaps_cpp(
    DatetimeVector x_starts, NumericVector x_spans,
    DatetimeVector y_starts, NumericVector y_spans,
    String bounds
) {
  IntvlVectorView x {x_starts, x_spans};
  IntvlVectorView y {y_starts, y_spans};
  if (bounds == "[]") {
    return phint_relate(x, y, Overlaps<true>{});
  } else {
    return phint_relate(x, y, Overlaps<false>{});
  }
}
