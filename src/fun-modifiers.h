#ifndef PHINTERVAL_FUN_MODIFIERS_H
#define PHINTERVAL_FUN_MODIFIERS_H

#include "type-phinterval.h"

// Modifiers never receive `NA` elements
// - apply_to_set() takes a PhintView (possibly empty) or a ScalarView type
// - apply_to_span() takes a single-span PhintView or a ScalarView type

struct DiscardInstants {
  template <typename XView, typename Buffer>
  void apply_to_set(const XView& x, Buffer& out);

  template <typename XView, typename Buffer>
  void apply_to_span(const XView& x, Buffer& out);
};

struct Complement {
  template <typename XView, typename Buffer>
  void apply_to_set(const XView& x, Buffer& out);

  template <typename XView, typename Buffer>
  void apply_to_span(const XView& x, Buffer& out);
};

enum class HoleTo : int { Hole = 0, Inf = 1, NA = 2 };

template <HoleTo hole_to>
struct Invert {
  template <typename XView, typename Buffer>
  void apply_to_set(const XView& x, Buffer& out);

  template <typename XView, typename Buffer>
  void apply_to_span(const XView& x, Buffer& out);
};

// discard instants ------------------------------------------------------------

template <typename XView, typename Buffer>
void DiscardInstants::apply_to_span(const XView& x, Buffer& out) {
  if (x.start(0) == x.end(0)) {
    out.add_hole_element();
  } else {
    out.add_scalar_element(x.start(0), x.end(0));
  }
};

template <typename XView, typename Buffer>
void DiscardInstants::apply_to_set(const XView& x, Buffer& out) {
  if (x.is_hole()) {
    out.add_hole_element();
    return;
  }

  for (int i = 0; i < x.size; i++) {
    if (x.start(i) == x.end(i)) continue;
    out.add_span(x.start(i), x.end(i));
  }

  out.finish_element();
}

// complement ------------------------------------------------------------------

template <typename XView, typename Buffer>
void Complement::apply_to_span(const XView& x, Buffer& out) {
  if (x.start(0) == x.end(0)) {
    out.add_inf_element();
    return;
  }

  // Case where start (end) is positive (negative) inf is covered by `==` check
  const bool inf_start = x.start(0) == R_NegInf;
  const bool inf_end = x.end(0) == R_PosInf;

  if (inf_start && inf_end) {
    out.add_hole_element();
  } else if (inf_start) {
    out.add_scalar_element(x.end(0), R_PosInf);
  } else if (inf_end) {
    out.add_scalar_element(R_NegInf, x.start(0));
  } else{
    out.add_span(R_NegInf, x.start(0));
    out.add_span(x.end(0), R_PosInf);
    out.finish_element();
  }
};

template <typename XView, typename Buffer>
void Complement::apply_to_set(const XView& x, Buffer& out) {
  if (x.is_hole()) {
    out.add_inf_element();
    return;
  }

  const bool inf_start = x.start(0) == R_NegInf;
  const bool inf_end = x.end(x.size - 1) == R_PosInf;

  // Safely assume `x.size > 1`, as `x.size == 1` is caught by `apply_to_span()`
  if (!inf_start) out.add_span(R_NegInf, x.start(0));
  int i = 0;
  for (; i < x.size - 1; i++) {
    out.add_span(x.end(i), x.start(i + 1));
  }
  if (!inf_end) out.add_span(x.end(i), R_PosInf);

  out.finish_element();
}

// invert ----------------------------------------------------------------------

template <HoleTo hole_to>
template <typename XView, typename Buffer>
void Invert<hole_to>::apply_to_span(const XView& x, Buffer& out) {
  out.add_hole_element();
};

template <HoleTo hole_to>
template <typename XView, typename Buffer>
void Invert<hole_to>::apply_to_set(const XView& x, Buffer& out) {
  if (x.is_hole()) {
    if constexpr (hole_to == HoleTo::Hole) {
      out.add_hole_element();
    } else if constexpr (hole_to == HoleTo::Inf) {
      out.add_inf_element();
    } else {
      out.add_na_element();
    }
    return;
  }

  double start, next_start, end;
  int i = 0;
  while (i < x.size - 1) {
    start = x.end(i);
    end = x.start(i + 1);
    next_start = x.end(i + 1);

    // Skip over instants in the middle of the set (#9)
    while (end == next_start && i < x.size - 2) {
      ++i;
      next_start = x.end(i + 1);
      end = x.start(i + 1);
    }
    out.add_span(start, end);
    ++i;
  }
  out.finish_element();
}

#endif
