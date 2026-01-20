#ifndef PHINTERVAL_TYPE_HELPERS_H
#define PHINTERVAL_TYPE_HELPERS_H

#include <Rcpp.h>
using namespace Rcpp;

struct ScalarView {
  const int size;
  const double starts;
  const double ends;
  const bool is_na;

  ScalarView(double start_, double end_)
    : size(1),
      starts(start_),
      ends(end_),
      is_na(false)
  {}

  static ScalarView na_view() {
    return { NA_INTEGER, NA_REAL, NA_REAL, true };
  }

  inline double start(int) const { return starts; }
  inline double end(int) const { return ends; }
  inline bool is_empty() const { return false; }
  inline bool is_scalar() const { return !is_na; }

private:
  // Private constructor for the NA case
  ScalarView(int size_, double starts_, double ends_, bool is_na_)
    : size(size_), starts(starts_), ends(ends_), is_na(is_na_)
  {}
};

struct SetView {
  const int size;
  const double* starts;
  const double* ends;
  const bool is_na;

  SetView(int size_, const double* starts_, const double* ends_)
    : size(size_),
      starts(starts_),
      ends(ends_),
      is_na(size_ == NA_INTEGER)
  {}

  inline double start(int i) const { return starts[i]; }
  inline double end(int i) const { return ends[i]; }
  inline bool is_empty() const { return !size; }
  inline bool is_scalar() const { return size == 1; }
};

template <typename VectorType, typename ViewType>
class Recycled {
private:
  const ViewType m_view;
  const int m_size;

public:
  explicit Recycled(const VectorType& vec)
    : m_view(vec.view(0)),
      m_size(vec.view(0).size)
  {
    if (vec.n_sets() != 1) {
      stop("Attempted to recycle a vector of length %i.", vec.n_sets());
    }
  }

  inline ViewType view(R_xlen_t) const { return m_view; }
  inline R_xlen_t n_sets() const { return 1; }
  inline int size(R_xlen_t) const { return m_size; }
};

#endif
