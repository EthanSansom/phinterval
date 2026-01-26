#ifndef PHINTERVAL_FUN_RELATIONS_H
#define PHINTERVAL_FUN_RELATIONS_H

#include "type-phinterval.h"
#include <algorithm>

// Relations never receive `NA` elements
// - apply_to_set() takes a PhintView (possibly empty) or a ScalarView type
// - apply_to_span() takes a single-span PhintView or a ScalarView type

struct Within {
  template <typename XView, typename YView>
  bool apply_to_set(const XView& x, const YView& y);

  template <typename XView, typename YView>
  bool apply_to_span(const XView& x, const YView& y);
};

struct Overlaps {
  template <typename XView, typename YView>
  bool apply_to_set(const XView& x, const YView& y);

  template <typename XView, typename YView>
  bool apply_to_span(const XView& x, const YView& y);
};

// implementation --------------------------------------------------------------

template <typename XView, typename YView>
bool Within::apply_to_span(const XView& x, const YView& y) {
  return y.start(0) <= x.start(0) && x.end(0) <= y.end(0);
};

template <typename XView, typename YView>
bool Within::apply_to_set(const XView& x, const YView& y) {
  if (x.is_empty() || y.is_empty()) {
    return false;
  }

  int j = 0;
  for (int i = 0; i < x.size; i++) {
    double x_start = x.start(i);
    double x_end = x.end(i);

    while (j < y.size && y.end(j) < x_start) j++;

    if (j == y.size) return false;

    double y_start = y.start(j);
    double y_end = y.end(j);
    if (x_start < y_start || x_end > y_end) return false;
  }

  return true;
}

template <typename XView, typename YView>
bool Overlaps::apply_to_span(const XView& x, const YView& y) {
  return std::max(x.start(0), y.start(0)) <= std::min(x.end(0), y.end(0));
};

template <typename XView, typename YView>
bool Overlaps::apply_to_set(const XView& x, const YView& y) {
  if (x.is_empty() || y.is_empty()) {
    return false;
  }

  int i = 0, j = 0;
  while (i < x.size && j < y.size) {
    double start = std::max(x.start(i), y.start(j));
    double end = std::min(x.end(i), y.end(j));

    if (start <= end) return true;

    if (x.end(i) < y.end(j)) {
      i++;
    } else {
      j++;
    }
  }

  return false;
}

#endif
