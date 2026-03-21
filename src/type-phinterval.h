#ifndef PHINTERVAL_TYPE_PHINTERVAL_H
#define PHINTERVAL_TYPE_PHINTERVAL_H

#include "type-helpers.h"
#include <utility>
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

class PhintVectorView;
using PhintView = SetView;
using PhintRecycled = Recycled<PhintVectorView, PhintView>;

class PhintVectorView {
private:
  const List& starts;
  const List& ends;
  const int* p_size;
  const R_xlen_t n;

public:
  PhintVectorView(const IntegerVector& size_, const List& starts_, const List& ends_)
    : starts(starts_),
      ends(ends_),
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
  SEXP starts_i = VECTOR_ELT(starts, i);
  SEXP ends_i = VECTOR_ELT(ends, i);
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

  void insert_na_element(R_xlen_t i);
  void insert_hole_element(R_xlen_t i);
  void insert_inf_element(R_xlen_t i);
  void insert_scalar_element(double start, double end, R_xlen_t i);
  void insert_set_element(const SetView& view, R_xlen_t i);
  void insert_set_element(const ScalarView& view, R_xlen_t i);

  void add_na_element();
  void add_hole_element();
  void add_inf_element();
  void add_scalar_element(double start, double end);
  void add_set_element(const SetView& view);
  void add_set_element(const ScalarView& view);
  void add_span(double start, double end);
  void finish_element();
  PhintView view(R_xlen_t i) const;
  List get_results();
};

inline PhintView PhintBuffer::view(R_xlen_t i) const {
  int size = p_size[i];
  if (size == NA_INTEGER) {
    return SetView::na_view();
  }
  SEXP starts_i = VECTOR_ELT(starts, i);
  SEXP ends_i = VECTOR_ELT(ends, i);
  return { size, REAL(starts_i), REAL(ends_i) };
}

struct SetBuffer {
private:
  std::vector<double> starts;
  std::vector<double> ends;

public:
  SetBuffer(int reserve_size = 8) {
    starts.reserve(reserve_size);
    ends.reserve(reserve_size);
  }
  R_xlen_t n_spans() {
    return starts.size();
  }
  void add_hole_element() {
    return;
  };
  void add_scalar_element(double start, double end) {
    starts.push_back(start);
    ends.push_back(end);
  };
  void add_set_element(const SetView& view) {
    for (int i = 0; i < view.size; i++) {
      starts.push_back(view.start(i));
      ends.push_back(view.end(i));
    }
  };
  void add_set_element(const ScalarView& view) {
    starts.push_back(view.start(0));
    ends.push_back(view.end(0));
  };
  void add_span(double start, double end) {
    starts.push_back(start);
    ends.push_back(end);
  };
  void finish_element() {
    return;
  };
  void clear() {
    starts.clear();
    ends.clear();
  }

  // The pointers returned by view() will be invalidated if the buffer grows,
  // so this should only be called after the buffer has been filled.
  PhintView view() const;
  const std::vector<double>& view_starts() const { return starts; }
  const std::vector<double>& view_ends() const { return ends; }

  List get_results() {
    return List::create(
      Named("size") = Rf_ScalarInteger(starts.size()),
      Named("starts") = List::create(wrap(starts)),
      Named("ends") = List::create(wrap(ends))
    );
  }
};

inline PhintView SetBuffer::view() const {
  int size = starts.size();
  return { size, starts.data(), ends.data() };
}

struct SetSwapBuffer {
  SetBuffer a;
  SetBuffer b;
  SetBuffer* current = &a;
  SetBuffer* next = &b;

  void swap() {
    std::swap(current, next);
    next->clear();
  }

  SetView view() const { return current->view(); }
  void clear() { current->clear(); }
};

#endif
