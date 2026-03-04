# todos ------------------------------------------------------------------------

# TODO: Instants
# - Instants aren't compatible with this approach, we'll have to sift them
#   first. Future intervals *can* overlap with prior instants, since `phint_setdiff()`
#   can't remove instants (and there's no way around this)
#
# 1. `phint_unoverlap()` just removes instants.
# 2. Create a `phint_widen(phint, duration = duration(1, "seconds"))` or similar,
#    to widen spans below a certain duration. This way, you can get normal
#    behavior by treating instants as 1-second long intervals.

# TODO: Solving the overlapping problem: `phint_unoverlap()`
#
# ## `phint_unoverlap(phint)`
# Removes overlaps from `phint` sequentially, by taking the difference:
# `phint_setdiff(phint[i], phint_squash(phint[1:(i - 1)])`
# So `phint[i]` contains *no* interval preceding itself.
#
## `phint_unoverlap(phint, priority = c(1, 1, 3, 3, 2))`
# Removes overlaps from `phint` in order of priority and then sequentially within
# priority:
# phint_setdiff(
#   phint[i],
#   phint_squash(
#     phint[
#       priority < priority[i] |          # Priority is lower than the current
#       (priority == priority[i] & k < i) # Within priority, it's an earlier span
#     ]
#   )
# )
#
## `phint_unoverlap(phint, priority = c(1, 1, 3, 3, 2), within_priority = "keep")`
# Removes overlaps from `phint` in order of priority, doesn't remove overlaps
# within priority:
# `phint_setdiff(phint[i], phint_squash(phint[priority <= priority[i]]))`
#
# SIGNATURE:
# phint_unoverlap(
#   phint,
#   priority = NULL,
#   priority_order = c("ascending", "descending", "appearance"),
#   within_priority = c("sequential", "keep")
# )
#
# ARGUMENTS:
# - phint: A phinterval or Interval vector
# - priority: Optional grouping vector defining priority groups. Earlier groups
#   (per priority_order) block later groups. Default NULL = no grouping.
# - priority_order: How to order priority groups for processing.
#   * "ascending" (default): Lower values processed first (priority 1 before 2)
#   * "descending": Higher values processed first (priority 9 before 2)
#   * "appearance": Process in order of first appearance
# - within_priority: How to handle overlaps within same priority group.
#   * "sequential" (default): Also resolve overlaps within group using row order
#   * "keep": Same-priority intervals can overlap
#
# BEHAVIOR:
# Without priority: Each interval is trimmed by all previous intervals
#   result[i] = setdiff(phint[i], union(phint[1:(i-1)]))
#
# With priority: Intervals grouped by priority, processed in priority_order.
#   Each interval trimmed by all intervals from earlier priority groups.
#   If within_priority = "sequential", also trimmed by earlier intervals
#   in same priority group.
#
# USE CASES:
#
## 1. Sequential removal (no grouping)
# data |>
#   arrange(level) |>
#   mutate(phint = phint_unoverlap(phint))
#
# # 2. Independent groups (no cross-group interference)
# data |>
#   group_by(machine) |>
#   arrange(level) |>
#   mutate(phint = phint_unoverlap(phint))
#
# # 3. Priority-based: higher priority blocks lower, same-priority can overlap
# data |>
#   mutate(phint = phint_unoverlap(
#     phint,
#     priority = level  # level 1 blocks level 2, level 2 blocks level 3
#   ))
#
# # 4. Priority-based with within-priority resolution
# data |>
#   arrange(level) |>
#   mutate(phint = phint_unoverlap(
#     phint,
#     priority = group,            # group A blocks group B
#     within_priority = "sequential"  # within each group, also resolve by row order
#   ))
#
# # 5. Custom priority ordering
# data |>
#   mutate(phint = phint_unoverlap(
#     phint,
#     priority = level,
#     priority_order = "descending"  # Process level 9, then 3, then 1
#   ))

## testing ---------------------------------------------------------------------

load_all()
library(lubridate)

# helpers ----------------------------------------------------------------------

phint_cumunion <- function(phint, na_propogate = FALSE, reverse = FALSE) {
  check_phintish(phint)
  check_bool(na_propogate)
  check_bool(reverse)

  if (!na_propogate) {
    phint[is.na(phint)] <- hole(tzone = get_tzone(phint))
  }

  phint_accumulate(phint, phint_union, reverse = reverse)
}

# helpers ----------------------------------------------------------------------

phint_accumulate <- function(.x, .f, ..., reverse = FALSE) {
  f <- function(x, y) .f(x, y, ...)
  vctrs::list_unchop(Reduce(f, .x, accumulate = TRUE, right = reverse, simplify = FALSE))
}

# phint_unoverlap --------------------------------------------------------------

# TODO: This seems to be working! Move to the correct location and do some
#       unit tests. You'll need to implement `phint_cummunion()` correctly first,
#       which will be simple enough in C++.

phint_unoverlap <- function(
    phint,
    priority = NULL,
    priority_order = c("asc", "desc", "appearance"),
    within_priority = c("sequential", "keep"),
    na_propogate = FALSE
) {
  check_phintish(phint)
  check_vector(priority, allow_null = TRUE)
  check_recycleable_to(priority, phint)

  priority_order <- arg_match0(priority_order, c("asc", "desc", "appearance"))
  within_priority <- arg_match0(within_priority, c("sequential", "keep"))
  check_bool(na_propogate)

  if (!na_propogate) {
    hole <- hole(tzone = get_tzone(phint))
    phint[is.na(phint)] <- hole
  }

  # Un-overlap elements within the same group
  unoverlap_within <- function(phint, within_priority) {
    if (within_priority == "keep") {
      return(phint)
    }
    # `within_priority == "sequential"`, remove `phint[1:(i-1)]` from `phint[[i]]`
    mask <- phint_cumunion(c(hole(1L), phint[-length(phint)]), na_propogate = na_propogate)

    print("unoverlap_within: mask")
    print(mask)
    cat("\n")

    phint_resolved <- phint_setdiff(phint, mask)
    print("unoverlap_within: phint_resolved")
    print(phint_resolved)
    cat("\n")

    phint_resolved
  }

  # If `priority` is `NULL` we consider the entirety of `phint` to be within
  # the same group.
  if (is.null(priority) || is_scalar(priority)) {
    return(unoverlap_within(phint, within_priority))
  }

  groups <- switch(
    priority_order,
    asc = vec_locate_sorted_groups(priority, direction = "asc"),
    desc = vec_locate_sorted_groups(priority, direction = "desc"),
    appearance = vec_group_loc(priority)
  )

  # For now, use a phinterval as a buffer to assign into
  output_buffer <- hole(n = length(phint), tzone = tzone)

  # Mask of past groups that we accumulate and difference from subsequent groups
  mask <- hole(n = 1L, tzone = tzone)

  # `groups` is already arranged in the order that we want to resolve conflicts
  for (i in seq(nrow(groups))) {
    # `loc[[i]]` gives us the group of `phinterval` elements that we want to un-overlap
    locations <- groups$loc[[i]]
    phint_subset <- phint[locations]

    print("phint_subset:")
    print(phint_subset)
    cat("\n")

    # For the first group, we only need to resolve internal overlaps
    if (i == 1) {
      phint_resolved <- unoverlap_within(phint_subset, within_priority)

      print("phint_resolved (1):")
      print(phint_resolved)
      cat("\n")

      output_buffer[locations] <- phint_resolved
      mask <- phint_squash(phint_resolved, na.rm = !na_propogate)
      next
    }

    # For remaining groups, we need to:
    # 1. Resolve overlaps with previous groups
    # 2. Resolve overlaps within the group
    phint_resolved <- phint_setdiff(phint_subset, mask)
    phint_resolved <- unoverlap_within(phint_resolved, within_priority)
    output_buffer[locations] <- phint_resolved

    print("phint_resolved (>1):")
    print(phint_resolved)
    cat("\n")

    # Update the mask on every iteration but the last
    if (i < nrow(groups)) {
      mask_i <- phint_squash(phint_resolved, na.rm = !na_propogate)
      mask <- phint_union(mask, mask_i)
    }
  }

  print("Returning:")
  output_buffer
}

## Arguments
phint <- phinterval(origin + c(1, 9, 10), origin + c(2, 11, 12))
priority <- c(1, 2, 1)
priority_order <- "appearance"
within_priority <- "sequential"

phint[2] <- NA

phint_unoverlap(
  phint = phint,
  priority = priority,
  priority_order = priority_order,
  within_priority = within_priority,
  na_propogate = TRUE
)
