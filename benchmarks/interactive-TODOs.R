# todos ------------------------------------------------------------------------

# TODO: Make these into GitHub issues? Look at how people normally implement
#       features. Make a branch for each feature?

# todo cumulative family -------------------------------------------------------

# TODO: Fixes + New Functions
# - phint_squash() and `datetime_squash()` should immediately return length-1 inputs
# - phint_cumunion(), phint_cumintersect(), phint_cumsetdiff() -> cumulative union, intersect, setdiff
# - phint_setdiff_symmetric() + cumulative version

# todo locate family -----------------------------------------------------------

# TODO: Long term
# - phint_locate_groups() -> based on ivs::iv_locate_groups(), also `phint_identify_group()`
# - phint_locate_splits() -> based on ivs::iv_locate_splits()
#   - this is actually similar to `phint_unoverlap()`, since splits must be disjoint
# - phint_identify_containers() -> https://davisvaughan.github.io/ivs/reference/iv-containers.html
#
# For all of these, need to think about why you couldn't just use ivs. I still
# don't see a compelling reason not to use `ivs`.
#
# TODO: Final thought, I think only `phint_locate_groups()` is really critical,
#       the rest can be converted to `ivs`. I already have most of `phint_locate_groups()`
#       implemented during squashing, I just need to track which elements belong
#       to which group! Maybe `phint_locate_splits()` since that's also in
#       `phint_unoverlap()`...

# todo duration family ---------------------------------------------------------

# phint_filter(phint, min_dur = NULL, max_dur = NULL)
# - Remove spans within `phint[i]` where `min_dur <= end - start <= max_dur`

# phint_widen(
#   phint,
#   min_dur = NULL,
#   max_dur = NULL,
#   anchor = c("start", "center", "end")
# )
# - Resize spans within `phint[i]` such that their duration is `>= min_dur` and `<= max_dur`
# - Anchor determines which endpoint stays the same, the start or end, or
#   if we resize relative to the center

# phint_fill(phint, min_gap = NULL)
# - Fill gaps between spans in a each element `phint[i]` which are less than
#   the duration `min_gap`
# - E.g. {[0, 10], [20, 30]} becomes {[0, 30]} when `min_gap == dseconds(10)`

# phint_shift(phint, duration)
# - Shift `phint[i]` by `duration[i]`, negative durations mean backwards

# todo overlap family ----------------------------------------------------------

# Second un-overlapping method, `phint_unblock()`. This has the *same*
# arguments as `phint_unoverlap()`, but resolves overlaps by shifting
# intervals forward in time, such that `phint_start(phint[i]) >= phint_end(phint[i - 1])`.
# For holey intervals, we don't attempt to fill holes (confusing).
#
# phint_unblock([1, 5], [3, 6], [9, 12])
# [1, 5]
# [5, 8]  # Still duration-3, but shifted
# [9, 12] # Don't need to shift
#
# phint_unoverlap([1, 5], [3, 6], [9, 12])
# [1, 5]
# [5, 6]  # Setdiffed
# [9, 12] # Don't need to setdiff
#
# Other functions:
# - `phint_has_overlaps(phint, ...)` : does a <phinterval> have internal overlaps
# - Invariant: If `!any(phint_has_overlaps(phint, ...))` then phint_unoverlap(phint, ...) == phint
# - `phint_has_blockers(phint, ...)`, synonym of `phint_has_overlaps`, same invariant for phint_unblock
# - phint_any_overlaps(phint, ...), phint_any_blockers(phint, ...), scalar versions (e.g. TRUE or FALSE)

# TODO: Argument, `propogate_na = FALSE`
# Should NA values from previous elements (e.g. phint[i - 1]) or groups
# (e.g. phint[priority < priority[i]]) propagate to `phint[i]` while
# unoverlapping. In the implementation, we'll have to resolve each interval
# in order of priority, so if we hit an NA and `propogate_na = TRUE`, we can
# return the rest of the results as NA.

# todo unoverlap ---------------------------------------------------------------

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
