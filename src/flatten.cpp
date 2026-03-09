#include "type-helpers.h"
#include "type-interval.h"
#include "type-phinterval.h"
#include "fun-modifiers.h"
#include "squash.h"
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

// exports ---------------------------------------------------------------------

template <typename XVector>
List phint_flatten_impl(const XVector& x, const String& what);

// [[Rcpp::export]]
List phint_flatten_cpp(IntegerVector size, List starts, List ends, String what) {
  return phint_flatten_impl(PhintVectorView{size, starts, ends}, what);
}

// [[Rcpp::export]]
List intvl_flatten_cpp(DatetimeVector starts, NumericVector spans, String what) {
  return phint_flatten_impl(IntvlVectorView{starts, spans}, what);
}

// [[Rcpp::export]]
List range_flatten_cpp(DatetimeVector starts, DatetimeVector ends, String what) {
  return phint_flatten_impl(RangeVectorView{starts, ends}, what);
}

// implementation --------------------------------------------------------------

template <typename VectorX>
List phint_flatten_impl(const VectorX& x, const String& what) {
  R_xlen_t n_sets = x.n_sets();
  if (n_sets == 0) return phint_result_empty();

  // This buffer stores the total number of spans in `x`. If `x` is an:
  // - <Interval> vector, then `n_spans == n_sets - sum(is.na(x))`
  // - <phinterval>, wide bounds, n_spans \in [0 (all holes), N >> n_sets (lots of gaps)]
  SetBuffer squash_in_buffer(n_sets);

  for (int i = 0; i < n_sets; i++) {
    auto x_i = x.view(i);
    if (x_i.is_na || x_i.is_empty()) continue;
    squash_in_buffer.add_set_element(x_i);
  }

  // If `x` is entirely NA or <hole>, the result is a length-0 <phinterval>
  R_xlen_t n_spans = squash_in_buffer.n_spans();
  if (n_spans == 0) return phint_result_empty();

  // Squashed size is `<= n_spans`, at equality when all input spans are disjoint
  SetBuffer squash_out_buffer(n_spans);
  std::vector<size_t> index_buffer;
  squash_scalar(
    squash_in_buffer.view_starts(),
    squash_in_buffer.view_ends(),
    index_buffer,
    squash_out_buffer
  );

  if (what == "spans") {
    R_xlen_t n_out_spans = squash_out_buffer.n_spans();
    auto out_starts = squash_out_buffer.view_starts();
    auto out_ends = squash_out_buffer.view_ends();

    PhintBuffer out(n_out_spans);
    for (size_t i = 0; i < n_out_spans; i++) {
      out.add_scalar_element(out_starts[i], out_ends[i]);
    }
    return out.get_results();
  }

  SetBuffer invert_buffer(squash_out_buffer.n_spans());
  Invert<HoleTo::Hole> invert_op{};
  invert_op.apply_to_set(squash_out_buffer.view(), invert_buffer);

  R_xlen_t n_out_spans = invert_buffer.n_spans();
  auto out_starts = invert_buffer.view_starts();
  auto out_ends = invert_buffer.view_ends();

  PhintBuffer out(n_out_spans);
  for (size_t i = 0; i < n_out_spans; i++) {
    out.add_scalar_element(out_starts[i], out_ends[i]);
  }
  return out.get_results();
}
