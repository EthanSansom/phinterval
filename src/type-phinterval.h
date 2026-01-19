#ifndef PHINTERVAL_TYPE_PHINTERVAL_H
#define PHINTERVAL_TYPE_PHINTERVAL_H

#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

struct PhintView {
  const int size;
  const double* starts;
  const double* ends;

  bool is_empty() const { return !size; }
};

class PhintVector {
private:
  const IntegerVector& m_size;
  const List& m_starts;
  const List& m_ends;
  const int* p_size;
  R_xlen_t n;

public:
  PhintVector(const IntegerVector& size_, const List& starts_, const List& ends_)
    : m_size(size_),
      m_starts(starts_),
      m_ends(ends_),
      p_size(INTEGER(size_)),
      n(size_.size())
  {}

  PhintView view(R_xlen_t i) const {
    SEXP starts_i = VECTOR_ELT(m_starts, i);
    SEXP ends_i = VECTOR_ELT(m_ends, i);
    return { p_size[i], REAL(starts_i), REAL(ends_i) };
  }
  R_xlen_t n_sets() const { return n; }
  int size(R_xlen_t i) const { return p_size[i]; }
};

class PhintScalar {
private:
  const PhintVector& m_phint;
  const PhintView m_view;
  const int m_size;

public:
  PhintScalar(const PhintVector& phint_)
    : m_phint(phint_),
      m_view(phint_.view(0)),
      m_size(phint_.size(0))
  {
    if (phint_.n_sets() != 1) {
      stop("Attempted to initialize a PhintScalar from a vector of length %i.", phint_.n_sets());
    }
  }

  PhintView view(R_xlen_t) const { return m_view; }
  R_xlen_t n_sets() const { return 1; }
  int size(R_xlen_t) { return m_size; }
};

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
