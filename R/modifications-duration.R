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
