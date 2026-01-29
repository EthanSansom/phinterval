#ifndef PHINTERVAL_FUN_RELATIONS_H
#define PHINTERVAL_FUN_RELATIONS_H

#include "type-phinterval.h"
#include <algorithm>

// Relations never receive `NA` elements
// - apply_to_set() takes a PhintView (possibly empty) or a ScalarView type
// - apply_to_span() takes a single-span PhintView or a ScalarView type

template <bool IsInclusive>
struct Within {
  template <typename XView, typename YView>
  bool apply_to_set(const XView& x, const YView& y);

  template <typename XView, typename YView>
  bool apply_to_span(const XView& x, const YView& y);
};

template <bool IsInclusive>
struct Overlaps {
  template <typename XView, typename YView>
  bool apply_to_set(const XView& x, const YView& y);

  template <typename XView, typename YView>
  bool apply_to_span(const XView& x, const YView& y);
};

// implementation --------------------------------------------------------------

template <bool IsInclusive>
template <typename XView, typename YView>
bool Within<IsInclusive>::apply_to_span(const XView& x, const YView& y) {
  if constexpr (IsInclusive) {
    return y.start(0) <= x.start(0) && x.end(0) <= y.end(0);
  } else {
    if (x.start(0) == x.end(0)) {
      return y.start(0) < x.start(0) && x.end(0) < y.end(0);
    }
    return y.start(0) <= x.start(0) && x.end(0) <= y.end(0);
  }
};

template <bool IsInclusive>
template <typename XView, typename YView>
bool Within<IsInclusive>::apply_to_set(const XView& x, const YView& y) {
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

    // Instants on endpoints are considered not within
    if constexpr (!IsInclusive) {
      if (x_start == x_end && (x_start == y_start || x_end == y_end)) return false;
    }
  }

  return true;
}

template <bool IsInclusive>
template <typename XView, typename YView>
bool Overlaps<IsInclusive>::apply_to_span(const XView& x, const YView& y) {
  if constexpr (IsInclusive) {
    return std::max(x.start(0), y.start(0)) <= std::min(x.end(0), y.end(0));
  } else {
    return std::max(x.start(0), y.start(0)) < std::min(x.end(0), y.end(0));
  }
};

template <bool IsInclusive>
template <typename XView, typename YView>
bool Overlaps<IsInclusive>::apply_to_set(const XView& x, const YView& y) {
  if (x.is_empty() || y.is_empty()) {
    return false;
  }

  int i = 0, j = 0;
  while (i < x.size && j < y.size) {
    double start = std::max(x.start(i), y.start(j));
    double end = std::min(x.end(i), y.end(j));

    if constexpr (IsInclusive) {
      if (start <= end) return true;
    } else {
      if (start < end) return true;
    }

    if (x.end(i) < y.end(j)) {
      i++;
    } else {
      j++;
    }
  }

  return false;
}

#endif
