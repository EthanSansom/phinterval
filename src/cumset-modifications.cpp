#include "type-helpers.h"
#include "type-interval.h"
#include "type-phinterval.h"
#include "fun-operators.h"
#include <Rcpp.h>
using namespace Rcpp;

// TODO: Test!!!
// TODO: phint_intersect_cpp, be sure to add a bounds = c("[]", "()") option
// TODO: intvl_*_cpp versions for receiving <Interval> vectors.

// TODO: Spell-check "Propogate"

// exports ---------------------------------------------------------------------

template <bool PropogateNA, typename VectorX, typename Op>
List phint_accumulate(const VectorX& x, Op op);

// [[Rcpp::export]]
List phint_cumunion_cpp(IntegerVector size, List starts, List ends, bool na_propogate) {
  PhintVectorView x {size, starts, ends};
  if (na_propogate) return phint_accumulate<true>(x, Union{});
  return phint_accumulate<false>(x, Union{});
}

// [[Rcpp::export]]
List intvl_cumunion_cpp(DatetimeVector starts, NumericVector spans, bool na_propogate) {
  IntvlVectorView x {starts, spans};
  if (na_propogate) return phint_accumulate<true>(x, Union{});
  return phint_accumulate<false>(x, Union{});
}

// [[Rcpp::export]]
List phint_cumintersect_cpp(
    IntegerVector size,
    List starts,
    List ends,
    bool na_propogate,
    String bounds
) {
  PhintVectorView x {size, starts, ends};
  if (na_propogate) {
    if (bounds == "[]") {
      return phint_accumulate<true>(x, Intersect<true>{});
    } else {
      return phint_accumulate<true>(x, Intersect<false>{});
    }
  }
  if (bounds == "[]") {
    return phint_accumulate<false>(x, Intersect<true>{});
  } else {
    return phint_accumulate<false>(x, Intersect<false>{});
  }
}

// [[Rcpp::export]]
List intvl_cumintersect_cpp(
    DatetimeVector starts,
    NumericVector spans,
    bool na_propogate,
    String bounds
) {
  IntvlVectorView x {starts, spans};
  if (na_propogate) {
    if (bounds == "[]") {
      return phint_accumulate<true>(x, Intersect<true>{});
    } else {
      return phint_accumulate<true>(x, Intersect<false>{});
    }
  }
  if (bounds == "[]") {
    return phint_accumulate<false>(x, Intersect<true>{});
  } else {
    return phint_accumulate<false>(x, Intersect<false>{});
  }
}

// implementation --------------------------------------------------------------

template <bool PropogateNA, typename VectorX, typename Op>
List phint_accumulate(const VectorX& x, Op op) {
  R_xlen_t n = x.n_sets();
  if (n == 0) return phint_result_empty();

  PhintBuffer buffer(n);
  R_xlen_t i = 0;
  auto view_elm = x.view(i);

  // Initiate the first element of the buffer, branching on PropogateNA
  if constexpr (PropogateNA) {
    if (view_elm.is_na) {
      goto fill_with_na;
    } else if (view_elm.is_empty()) {
      buffer.add_empty_element();
    } else {
      buffer.add_set_element(view_elm);
    }
  } else {
    if (view_elm.is_na || view_elm.is_empty()) {
      if constexpr (is_intersect_op<Op>) {
        goto fill_with_empty;
      } else {
        buffer.add_empty_element();
      }
    } else {
      buffer.add_set_element(view_elm);
    }
  }

  for (i++; i < n; i++) {
    if (!(i & 8191)) checkUserInterrupt();

    auto view_elm = x.view(i);
    auto view_lag = buffer.view(i - 1);

    // Handling the empty and NA cases, branching on PropogateNA
    if constexpr (PropogateNA) {
      if (view_elm.is_na) goto fill_with_na;

      // Handling the empty case, branching on Intersect vs. Union
      if constexpr (is_intersect_op<Op>) {
        // In cumulative intersection, as soon as a hole is hit all remaining
        // elements must either be a hole or an NA value which is propagated.
        if (view_elm.is_empty()) {
          buffer.add_empty_element();
          for (i++; i < n; i++) {
            if (x.view(i).is_na) goto fill_with_na;
            buffer.add_empty_element();
          }
          return buffer.get_results();
        }
      } else {
        // Union: if view_elm is empty -> union(view_elm, view_lag) == view_lag
        if (view_elm.is_empty()) {
          buffer.add_set_element(view_lag);
          continue;
        }
      }
    } else {
      // Empty and NA handling, not PropogateNA
      if (view_elm.is_na || view_elm.is_empty()) {
        if constexpr (is_intersect_op<Op>) {
          goto fill_with_empty;
        } else {
          buffer.add_set_element(view_lag);
          continue;
        }
      }
    } // Empty and NA handling done

    if (view_elm.is_scalar() && view_lag.is_scalar()) {
      op.apply_to_span(view_lag, view_elm, buffer);
    } else {
      op.apply_to_set(view_lag, view_elm, buffer);
    }
  }

  // TODO: Find out if the buffer is *already* full of NA elements
  fill_with_na:
    for (; i < n; i++) buffer.add_na_element();

  fill_with_empty:
    for (; i < n; i++) buffer.add_empty_element();

  return buffer.get_results();
}
