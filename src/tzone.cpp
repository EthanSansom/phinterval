#include <tzdb/tzdb.h>
#include <string>
#include <Rcpp.h>
using namespace Rcpp;

// Modified from {clock}
// - https://github.com/r-lib/clock/blob/f9ab06db5d6d7cc913e0dfa3bfde794554baaad8/src/zone.cpp

// [[Rcpp::export]]
LogicalVector tzone_is_valid_cpp(String tzone) {
  std::string tzone_name(tzone);

  // Local time
  if (tzone_name.size() == 0) {
    return Rf_ScalarLogical(true);
  }

  const date::time_zone* p_time_zone;

  if (tzdb::locate_zone(tzone_name, p_time_zone)) {
    return Rf_ScalarLogical(true);
  } else {
    return Rf_ScalarLogical(false);
  }
}
