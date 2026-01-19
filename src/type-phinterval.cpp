#include "type-phinterval.h"
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

PhintBuffer::PhintBuffer(R_xlen_t n, int reserve_size) {
  size = IntegerVector(no_init(n));
  starts = List(no_init(n));
  ends = List(no_init(n));
  p_size = INTEGER(size);

  temp_starts.reserve(reserve_size);
  temp_ends.reserve(reserve_size);
}

void PhintBuffer::add_na_element() {
  p_size[current_elt] = NA_INTEGER;
  SET_VECTOR_ELT(starts, current_elt, R_NilValue);
  SET_VECTOR_ELT(ends, current_elt, R_NilValue);
  current_elt++;
}

void PhintBuffer::add_empty_element() {
  p_size[current_elt] = 0;
  SET_VECTOR_ELT(starts, current_elt, Rf_allocVector(REALSXP, 0));
  SET_VECTOR_ELT(ends, current_elt, Rf_allocVector(REALSXP, 0));
  current_elt++;
}

void PhintBuffer::add_scalar_element(double start, double end) {
  p_size[current_elt] = 1;
  SET_VECTOR_ELT(starts, current_elt, Rf_ScalarReal(start));
  SET_VECTOR_ELT(ends, current_elt, Rf_ScalarReal(end));
  current_elt++;
}

void PhintBuffer::add_span(double start, double end) {
  temp_starts.push_back(start);
  temp_ends.push_back(end);
}

void PhintBuffer::finish_element() {
  p_size[current_elt] = temp_starts.size();
  SET_VECTOR_ELT(starts, current_elt, wrap(temp_starts));
  SET_VECTOR_ELT(ends, current_elt, wrap(temp_ends));
  current_elt++;
  temp_starts.clear();
  temp_ends.clear();
}

List PhintBuffer::get_results() {
  return List::create(
    Named("size") = size,
    Named("starts") = starts,
    Named("ends") = ends
  );
}
