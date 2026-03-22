#ifndef PHINTERVAL_TYPE_HELPERS_H
#define PHINTERVAL_TYPE_HELPERS_H

#include <Rcpp.h>
using namespace Rcpp;

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
  inline bool is_hole() const { return false; }
  inline bool is_scalar() const { return !is_na; }

  List get_results() const {
    if (is_na) return phint_result_na();
    return List::create(
      Named("size") = Rf_ScalarInteger(size),
      Named("starts") = List::create(Rf_ScalarReal(starts)),
      Named("ends") = List::create(Rf_ScalarReal(ends))
    );
  }

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

  static SetView na_view() {
    static const double na_sentinel = NA_REAL;
    return SetView(NA_INTEGER, &na_sentinel, &na_sentinel, true);
  }

  inline double start(int i) const { return starts[i]; }
  inline double end(int i) const { return ends[i]; }
  inline bool is_hole() const { return !size; }
  inline bool is_scalar() const { return size == 1; }

  List get_results() const {
    if (is_na) return phint_result_na();
    if (size == 0) return phint_result_hole();
    if (size == 1) {
      return List::create(
        Named("size") = Rf_ScalarInteger(size),
        Named("starts") = List::create(Rf_ScalarReal(starts[0])),
        Named("ends") = List::create(Rf_ScalarReal(ends[0]))
      );
    }
    return List::create(
      Named("size") = Rf_ScalarInteger(size),
      Named("starts") = List::create(NumericVector(starts, starts + size)),
      Named("ends") = List::create(NumericVector(ends, ends + size))
    );
  }

private:
  // Private constructor for the NA case
  SetView(int size_, const double* starts_, const double* ends_, bool is_na_)
    : size(size_), starts(starts_), ends(ends_), is_na(is_na_)
  {}
};

template <typename VectorType, typename ViewType>
class Recycled {
private:
  const ViewType m_view;
  const int m_size;

public:
  explicit Recycled(const VectorType& vec)
    : m_view(vec.view(0)), m_size(vec.view(0).size) {}

  inline ViewType view(R_xlen_t) const { return m_view; }
  inline R_xlen_t n_sets() const { return 1; }
  inline int size(R_xlen_t) const { return m_size; }
};

template<typename T>
inline constexpr bool is_scalar_view = false;

template<>
inline constexpr bool is_scalar_view<ScalarView> = true;

#endif
