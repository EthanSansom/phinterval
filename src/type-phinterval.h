#ifndef PHINTERVAL_TYPE_PHINTERVAL_H
#define PHINTERVAL_TYPE_PHINTERVAL_H

#include "type-helpers.h"
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

class PhintVectorView;
using PhintView = SetView;
using PhintRecycled = Recycled<PhintVectorView, PhintView>;

class PhintVectorView {
private:
  const IntegerVector& m_size;
  const List& m_starts;
  const List& m_ends;
  const int* p_size;
  const R_xlen_t n;

public:
  PhintVectorView(const IntegerVector& size_, const List& starts_, const List& ends_)
    : m_size(size_),
      m_starts(starts_),
      m_ends(ends_),
      p_size(INTEGER(size_)),
      n(size_.size())
  {}

  PhintRecycled as_recycled() const;
  PhintView view(R_xlen_t i) const;

  R_xlen_t n_sets() const { return n; }
  int size(R_xlen_t i) const { return p_size[i]; }
};

inline PhintView PhintVectorView::view(R_xlen_t i) const {
  int size = p_size[i];
  if (size == NA_INTEGER) {
    return SetView::na_view();
  }
  SEXP starts_i = VECTOR_ELT(m_starts, i);
  SEXP ends_i = VECTOR_ELT(m_ends, i);
  return { size, REAL(starts_i), REAL(ends_i) };
}

inline PhintRecycled PhintVectorView::as_recycled() const {
  return PhintRecycled{*this};
}

class PhintBuffer {
private:
  IntegerVector size;
  List starts;
  List ends;
  int* p_size;
  R_xlen_t current_elt = 0;

  std::vector<double> temp_starts;
  std::vector<double> temp_ends;

public:
  PhintBuffer(R_xlen_t n, int reserve_size = 8);
  void add_na_element();
  void add_empty_element();
  void add_inf_element();
  void add_scalar_element(double start, double end);
  void add_set_element(const SetView& view);
  void add_set_element(const ScalarView& view);
  void add_span(double start, double end);
  void finish_element();
  List get_results();
};

inline List phint_result_hole() {
  return List::create(
    Named("size") = IntegerVector::create(0),
    Named("starts") = List::create(NumericVector()),
    Named("ends") = List::create(NumericVector())
  );
}

inline List phint_result_na() {
  return List::create(
    Named("size") = IntegerVector::create(NA_INTEGER),
    Named("starts") = List::create(R_NilValue),
    Named("ends") = List::create(R_NilValue)
  );
}

inline List phint_result_empty() {
  return List::create(
    Named("size") = IntegerVector(0),
    Named("starts") = List(0),
    Named("ends") = List(0)
  );
}

#endif
