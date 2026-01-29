#ifndef PHINTERVAL_FUN_OPERATORS_H
#define PHINTERVAL_FUN_OPERATORS_H

#include "type-phinterval.h"
#include <algorithm>

// Operators never receive `NA` elements
// - apply_to_set() takes a PhintView (possible empty) or a ScalarView type
// - apply_to_span() takes a single-span PhintView or a ScalarView type

template <bool IsInclusive>
struct Intersect {
  template <typename XView, typename YView>
  void apply_to_set(const XView& x, const YView& y, PhintBuffer& out);

  template <typename XView, typename YView>
  void apply_to_span(const XView& x, const YView& y, PhintBuffer& out);
};

struct Union {
  template <typename XView, typename YView>
  void apply_to_set(const XView& x, const YView& y, PhintBuffer& out);

  template <typename XView, typename YView>
  void apply_to_span(const XView& x, const YView& y, PhintBuffer& out);
};

struct Setdiff {
  template <typename XView, typename YView>
  void apply_to_set(const XView& x, const YView& y, PhintBuffer& out);

  template <typename XView, typename YView>
  void apply_to_span(const XView& x, const YView& y, PhintBuffer& out);
};

// intersect -------------------------------------------------------------------

template <bool IsInclusive>
template <typename XView, typename YView>
void Intersect<IsInclusive>::apply_to_span(const XView& x, const YView& y, PhintBuffer& out) {
  double start { std::max(x.start(0), y.start(0)) };
  double end { std::min(x.end(0), y.end(0)) };

  if constexpr (IsInclusive) {
    if (end < start) {
      out.add_empty_element();
      return;
    }
  } else {
    if (end <= start) {
      out.add_empty_element();
      return;
    }
  }

  out.add_scalar_element(start, end);
};

template <bool IsInclusive>
template <typename XView, typename YView>
void Intersect<IsInclusive>::apply_to_set(const XView& x, const YView& y, PhintBuffer& out) {
  if (x.is_empty() || y.is_empty()) {
    out.add_empty_element();
    return;
  }

  int i = 0, j = 0;
  while (i < x.size && j < y.size) {
    double start = std::max(x.start(i), y.start(j));
    double end = std::min(x.end(i), y.end(j));

    if constexpr (IsInclusive) {
      if (start <= end) {
        out.add_span(start, end);
      }
    } else {
      if (start < end) {
        out.add_span(start, end);
      }
    }

    if (x.end(i) < y.end(j)) {
      i++;
    } else {
      j++;
    }
  }

  out.finish_element();
}

// union -----------------------------------------------------------------------

template <typename XView, typename YView>
void Union::apply_to_span(const XView& x, const YView& y, PhintBuffer& out) {
  if (x.end(0) < y.start(0)) {
    // x before y, add x first to maintain sorting
    out.add_span(x.start(0), x.end(0));
    out.add_span(y.start(0), y.end(0));
    out.finish_element();
  } else if (y.end(0) < x.start(0)) {
    // y before x, add y first to maintain sorting
    out.add_span(y.start(0), y.end(0));
    out.add_span(x.start(0), x.end(0));
    out.finish_element();
  } else {
    double start { std::min(x.start(0), y.start(0)) };
    double end { std::max(x.end(0), y.end(0)) };
    out.add_scalar_element(start, end);
  }
};

template <typename XView, typename YView>
void Union::apply_to_set(const XView& x, const YView& y, PhintBuffer& out) {
  const bool x_empty = x.is_empty();
  const bool y_empty = y.is_empty();

  if (x_empty && y_empty) {
    out.add_empty_element();
    return;
  } else if (x_empty) {
    out.add_set_element(y);
    return;
  } else if (y_empty) {
    out.add_set_element(x);
    return;
  }

  int i = 0, j = 0;
  double current_start, current_max_end;

  if (x.start(0) <= y.start(0)) {
    current_start = x.start(0);
    current_max_end = x.end(0);
    i = 1;
  } else {
    current_start = y.start(0);
    current_max_end = y.end(0);
    j = 1;
  }

  while (i < x.size || j < y.size) {
    double next_start, next_end;

    if (i >= x.size) {
      // x is exhausted, check remaining spans in y
      next_start = y.start(j);
      next_end = y.end(j);
      j++;
    } else if (j >= y.size) {
      // y is exhausted, check remaining spans in x
      next_start = x.start(i);
      next_end = x.end(i);
      i++;
    } else {
      // spans remain in x and y, check whichever span has the earliest start
      if (x.start(i) <= y.start(j)) {
        next_start = x.start(i);
        next_end = x.end(i);
        i++;
      } else {
        next_start = y.start(j);
        next_end = y.end(j);
        j++;
      }
    }

    if (next_start <= current_max_end) {
      current_max_end = std::max(current_max_end, next_end);
    } else {
      out.add_span(current_start, current_max_end);
      current_start = next_start;
      current_max_end = next_end;
    }
  }

  out.add_span(current_start, current_max_end);
  out.finish_element();
}

// setdiff ---------------------------------------------------------------------

template <typename XView, typename YView>
void Setdiff::apply_to_span(const XView& x, const YView& y, PhintBuffer& out) {
  double x_start = x.start(0);
  double x_end = x.end(0);
  double y_start = y.start(0);
  double y_end = y.end(0);

  if (x_end <= y_start || y_end <= x_start || y_start == y_end) {
    // x is outside of y or y is instantaneous, return x
    out.add_scalar_element(x_start, x_end);
    return;
  }
  if (y_start <= x_start && x_end <= y_end) {
    // y contains x, return an empty set
    out.add_empty_element();
    return;
  }

  if (x_start < y_start && y_end < x_end) {
    // x contains y, y punches a hole in x
    out.add_span(x_start, y_start);
    out.add_span(y_end, x_end);
    out.finish_element();
  } else if (x_start < y_start) {
    // y overlaps with the end of x, keep the start of x
    out.add_scalar_element(x_start, y_start);
  } else {
    // y overlaps with the start of x, keep the end of x
    out.add_scalar_element(y_end, x_end);
  }
};

template <typename XView, typename YView>
void Setdiff::apply_to_set(const XView& x, const YView& y, PhintBuffer& out) {
  if (x.is_empty()) {
    out.add_empty_element();
    return;
  } else if (y.is_empty()) {
    out.add_set_element(x);
    return;
  }

  int j = 0;
  for (int i = 0; i < x.size; i++) {
    double current_start = x.start(i);
    double current_end = x.end(i);

    // Skip over elements in y preceding the current element in x
    while (j < y.size && y.end(j) <= current_start) {
      j++;
    }

    // Punch holes in [current_start, current_end]
    double remainder_start = current_start;
    while (j < y.size && y.start(j) < current_end) {
      double y_start = y.start(j);
      double y_end = y.end(j);

      if (remainder_start < y_start) {
        out.add_span(remainder_start, std::min(current_end, y_start));
      }

      remainder_start = std::max(remainder_start, y_end);
      if (remainder_start >= current_end) {
        // We've moved past the end of x[i], stop punching holes
        break;
      }
      j++;
    }

    // Add the remaining portion of x[i], after punching holes. This is skipped
    // when a span in y aligned with the end of x[i].
    if (remainder_start < current_end) {
      out.add_span(remainder_start, current_end);
    }
  }

  out.finish_element();
}

#endif
