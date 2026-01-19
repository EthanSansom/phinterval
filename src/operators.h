#ifndef PHINTERVAL_OPERATORS_H
#define PHINTERVAL_OPERATORS_H

#include "type-phinterval.h"
#include <algorithm>

// TODO: Do views need to be passed in by const reference (I don't think so)?

// Operators never receive `NA` elements
// - apply_to_set() takes a <phinterval> (possibly <hole>) or <Interval> element
// - apply_to_span() takes a single-span <phinterval> or <Interval> element

struct Intersect {
  template <typename XView, typename YView>
  void apply_to_set(const XView& x, const YView& y, PhintBuffer& out);

  template <typename XView, typename YView>
  void apply_to_span(const XView& x, const YView& y, PhintBuffer& out);
};

template <typename XView, typename YView>
void Intersect::apply_to_span(const XView& x, const YView& y, PhintBuffer& out) {
  double start { std::max(x.start(0), y.start(0)) };
  double end { std::min(x.end(0), y.end(0)) };
  if (end < start) {
    out.add_empty_element();
  } else {
    out.add_scalar_element(start, end);
  }
};

template <typename XView, typename YView>
void Intersect::apply_to_set(const XView& x, const YView& y, PhintBuffer& out) {
  if (x.is_empty() || y.is_empty()) {
    out.add_empty_element();
    return;
  }

  int i = 0, j = 0;
  while (i < x.size && j < y.size) {
    double start = std::max(x.start(i), y.start(i));
    double end = std::min(x.end(i), y.end(i));

    if (start <= end) {
      out.add_span(start, end);
    }

    if (x.end(i) < y.end(j)) {
      i++;
    } else {
      j++;
    }
  }

  out.finish_element();
}

#endif
