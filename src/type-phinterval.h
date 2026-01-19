#ifndef PHINTERVAL_TYPE_PHINTERVAL_H
#define PHINTERVAL_TYPE_PHINTERVAL_H

#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

struct PhintView;
class PhintRecycled;

class PhintVector {
private:
  const IntegerVector& m_size;
  const List& m_starts;
  const List& m_ends;
  const int* p_size;
  const R_xlen_t n;

public:
  PhintVector(const IntegerVector& size_, const List& starts_, const List& ends_)
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

struct PhintView {
  const int size;
  const double* starts;
  const double* ends;
  const bool is_na;

  PhintView(int size_, const double* starts_, const double* ends_)
    : size(size_),
      starts(starts_),
      ends(ends_),
      is_na(size_ == NA_INTEGER)
  {}

  double start(int i) const { return starts[i]; }
  double end(int i) const { return ends[i]; }
  bool is_empty() const { return !size; }
  bool is_scalar() const { return size == 1; }
};

inline PhintView PhintVector::view(R_xlen_t i) const {
  int size = p_size[i];
  SEXP starts_i = VECTOR_ELT(m_starts, i);
  SEXP ends_i = VECTOR_ELT(m_ends, i);
  return { size, REAL(starts_i), REAL(ends_i) };
}

class PhintRecycled {
private:
  const PhintView m_view;
  const int m_size;

public:
  PhintRecycled(const PhintVector& phint)
    : m_view(phint.view(0)),
      m_size(phint.size(0))
  {
    if (phint.n_sets() != 1) {
      stop("Attempted to recycle a PhintVector of length %i.", phint.n_sets());
    }
  }

  inline PhintView view(R_xlen_t) const { return m_view; }
  inline R_xlen_t n_sets() const { return 1; }
  inline int size(R_xlen_t) const { return m_size; }
};

inline PhintRecycled PhintVector::as_recycled() const {
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
  void add_scalar_element(double start, double end);
  void add_span(double start, double end);
  void finish_element();
  List get_results();
};

#endif
