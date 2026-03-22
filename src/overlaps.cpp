#include "type-helpers.h"
#include "type-interval.h"
#include "type-phinterval.h"
#include "fun-operators.h"
#include <utility>
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

// exports ---------------------------------------------------------------------

template <bool KeepWithin, bool PropagateNA, typename VectorX>
List unoverlap_between_impl(const VectorX& x, const List& priority_locs);

template <bool PropagateNA, typename VectorX>
List unoverlap_within_impl(const VectorX& x);

// [[Rcpp::export]]
List phint_unoverlap_cpp(
    IntegerVector size,
    List starts,
    List ends,
    List priority_locs,
    String within_priority,
    bool na_propagate
) {
  PhintVectorView x {size, starts, ends};
  bool keep_within = (within_priority == "keep");
  if (keep_within && na_propagate) {
    return unoverlap_between_impl<true, true>(x, priority_locs);
  } else if (keep_within) {
    return unoverlap_between_impl<true, false>(x, priority_locs);
  } else if (na_propagate) {
    return unoverlap_between_impl<false, true>(x, priority_locs);
  } else {
    return unoverlap_between_impl<false, false>(x, priority_locs);
  }
}

// [[Rcpp::export]]
List intvl_unoverlap_cpp(
    DatetimeVector starts,
    NumericVector spans,
    List priority_locs,
    String within_priority,
    bool na_propagate
) {
  IntvlVectorView x {starts, spans};
  bool keep_within = (within_priority == "keep");
  if (keep_within && na_propagate) {
    return unoverlap_between_impl<true, true>(x, priority_locs);
  } else if (keep_within) {
    return unoverlap_between_impl<true, false>(x, priority_locs);
  } else if (na_propagate) {
    return unoverlap_between_impl<false, true>(x, priority_locs);
  } else {
    return unoverlap_between_impl<false, false>(x, priority_locs);
  }
}

// [[Rcpp::export]]
List phint_unoverlap_within_cpp(
    IntegerVector size,
    List starts,
    List ends,
    bool na_propagate
) {
  PhintVectorView x {size, starts, ends};
  if (na_propagate) {
    return unoverlap_within_impl<true>(x);
  } else {
    return unoverlap_within_impl<false>(x);
  }
}

// [[Rcpp::export]]
List intvl_unoverlap_within_cpp(
    DatetimeVector starts,
    NumericVector spans,
    bool na_propagate
) {
  IntvlVectorView x {starts, spans};
  if (na_propagate) {
    return unoverlap_within_impl<true>(x);
  } else {
    return unoverlap_within_impl<false>(x);
  }
}

// implementation --------------------------------------------------------------

template <bool KeepWithin, bool PropagateNA, typename VectorX>
List unoverlap_between_impl(const VectorX& x, const List& priority_locs) {
  R_xlen_t n_sets = x.n_sets();
  if (n_sets == 0) return phint_result_empty();
  R_xlen_t n_groups = priority_locs.length();

  PhintBuffer out(n_sets);

  // Swap buffers to achieve the pattern: cumunion <- union(x[i], cumunion)
  SetSwapBuffer within_group_cumunion;
  SetSwapBuffer between_group_cumunion;
  SetSwapBuffer current_element;

  Setdiff setdiff_op{};
  Union union_op{};

  R_xlen_t group = 0;
  for (; group < n_groups; group++) {
    SEXP locs = VECTOR_ELT(priority_locs, group);
    const int* p_locs = INTEGER(locs);
    R_xlen_t group_size = Rf_xlength(locs);

    bool propagated_na_in_group = false;
    for (R_xlen_t j = 0; j < group_size; j++) {
      int i = p_locs[j] - 1;
      auto x_i = x.view(i);

      // Case where `x[i]` is NA
      if constexpr (PropagateNA) {
        // `within_priority = "sequential", propagate NA's within-group
        if constexpr (!KeepWithin) {
          if (propagated_na_in_group) {
            out.insert_na_element(i);
            continue;
          }
        }
        // Always set `out[i] <- NA` if `PropagateNA`
        if (x_i.is_na) {
          propagated_na_in_group = true;
          out.insert_na_element(i);
          continue;
        }
      } else {
        // `na_propagate = FALSE`, treat NA values as holes
        if (x_i.is_na) {
          out.insert_hole_element(i);
          continue;
        }
      }

      // Case where `x[i]` is empty
      if (x_i.is_hole()) {
        out.insert_hole_element(i);
        continue;
      }

      // Case where `x[i]` contains spans:
      // 1. Between-overlaps: x[i] <- setdiff(x[i], x[priority > current_priority])
      // 2. Within-overlaps: x[i] <- setdiff(x[i], x[previous_group_members])

      // Resolve between-group overlaps: current_element <- setdiff(x[i], between_cumunion)
      setdiff_op.apply_to_set(x_i, between_group_cumunion.view(), *current_element.current);

      // Resolve within-group overlaps:
      if constexpr (KeepWithin) {
        // Don't care about within-group overlaps: out[i] <- current_element
        out.insert_set_element(current_element.view(), i);
      } else {
        // Resolve: out[i] <- setdiff(current_element, within_cumunion)
        setdiff_op.apply_to_set(current_element.view(), within_group_cumunion.view(), *current_element.next);
        current_element.swap();

        if (current_element.view().is_hole()) {
          out.insert_hole_element(i);
        } else {
          out.insert_set_element(current_element.view(), i);
        }
      }

      // Update within-group mask: within_cumunion <- union(current_element, within_cumunion)
      union_op.apply_to_set(current_element.view(), within_group_cumunion.view(), *within_group_cumunion.next);
      within_group_cumunion.swap();
      current_element.clear();
    } // Finished within-group overlap resolution

    // Update between-group mask: between_cumunion <- union(within_cumunion, between_cumunion)
    // This isn't necessary for the last (or only) group.
    if (group < n_groups - 1) {
      union_op.apply_to_set(
        within_group_cumunion.view(),
        between_group_cumunion.view(),
        *between_group_cumunion.next
      );
      between_group_cumunion.swap();
      within_group_cumunion.clear();
    }

    // Fill all future groups with NA elements
    if (PropagateNA) {
      if (propagated_na_in_group) {
        group++; // Increment to the next group
        goto fill_with_na;
      }
    }
  } // Finished between-group overlap resolution

  return out.get_results();

  fill_with_na:
    {
      // The remaining elements to propagate NA values to are spread across `x`,
      // so you can't fill via `for (; i < n_sets; i++) out.add_na_element();`.
      for (; group < n_groups; group++) {
        SEXP locs = VECTOR_ELT(priority_locs, group);
        const int* p_locs = INTEGER(locs);
        for (R_xlen_t j = 0; j < Rf_xlength(locs); j++) {
          int i = p_locs[j] - 1;
          out.insert_na_element(i);
        }
      }
      return out.get_results();
    }
}

template <bool PropagateNA, typename VectorX>
List unoverlap_within_impl(const VectorX& x) {
  R_xlen_t n_sets = x.n_sets();
  if (n_sets == 0) return phint_result_empty();

  PhintBuffer out(n_sets);
  SetSwapBuffer cumunion;

  Setdiff setdiff_op{};
  Union union_op{};

  R_xlen_t i = 0;
  for (; i < n_sets; i++) {
    auto x_i = x.view(i);

    if constexpr (PropagateNA) {
      if (x_i.is_na) goto fill_with_na;
      if (x_i.is_hole()) {
        out.add_hole_element();
        continue;
      }
    } else {
      if (x_i.is_na || x_i.is_hole()) {
        out.add_hole_element();
        continue;
      }
    }

    // out[i] <- setdiff(x[i], cumunion)
    setdiff_op.apply_to_set(x_i, cumunion.view(), out);

    // cumunion <- union(x[i], cumunion)
    union_op.apply_to_set(x_i, cumunion.view(), *cumunion.next);
    cumunion.swap();
  }

  return out.get_results();

  fill_with_na:
    {
      for (; i < n_sets; i++) out.add_na_element();
      return out.get_results();
    }
}

