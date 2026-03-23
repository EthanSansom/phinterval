# phint_unoverlap --------------------------------------------------------------

test_that("phint_unoverlap() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:30:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:40:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:50:00", tz = "UTC")
  int12 <- phinterval(t1, t2)
  int23 <- phinterval(t2, t3)
  int34 <- phinterval(t3, t4)
  int45 <- phinterval(t4, t5)
  int56 <- phinterval(t5, t6)
  int13 <- phinterval(t1, t3)
  int14 <- phinterval(t1, t4)
  int24 <- phinterval(t2, t4)
  int36 <- phinterval(t3, t6)
  int46 <- phinterval(t4, t6)
  hole  <- hole(tzone = "UTC")
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")

  # No overlaps: result is unchanged
  expect_equal(
    phint_unoverlap(c(int12, int34, int56)),
    c(int12, int34, int56)
  )

  # Adjacent intervals: no overlap to resolve
  expect_equal(
    phint_unoverlap(c(int12, int23, int34)),
    c(int12, int23, int34)
  )

  # Overlapping: later elements are trimmed by earlier ones
  expect_equal(
    phint_unoverlap(c(int13, int24)),
    c(int13, int34)
  )
  expect_equal(
    phint_unoverlap(c(int14, int13, int36)),
    c(int14, hole, int46)
  )
  expect_equal(
    phint_unoverlap(c(int12, int13, int14)),
    c(int12, int23, int34)
  )

  # Identical elements: all but the first become holes
  expect_equal(
    phint_unoverlap(c(int12, int12, int12)),
    c(int12, hole, hole)
  )

  # priority: lower values processed first, blocking higher values
  expect_equal(
    phint_unoverlap(c(int13, int12, int36), priority = c(1, 2, 1)),
    c(int13, hole, int36)
  )
  expect_equal(
    phint_unoverlap(c(int12, int13, int36), priority = c(2, 1, 2)),
    c(hole, int13, int36)
  )

  # priority_order = "desc": higher values processed first
  expect_equal(
    phint_unoverlap(c(int13, int12, int36), priority = c(1, 2, 1), priority_order = "desc"),
    c(int23, int12, int36)
  )

  # within_priority = "keep": overlaps within a group are preserved
  expect_equal(
    phint_unoverlap(c(int13, int12, int36), priority = c(1, 1, 2), within_priority = "keep"),
    c(int13, int12, int36)
  )
  expect_equal(
    phint_unoverlap(c(int13, int12, int36), priority = c(1, 1, 1), within_priority = "keep"),
    c(int13, int12, int36)
  )

  # within_priority = "sequential" (default): overlaps within group resolved by row order
  expect_equal(
    phint_unoverlap(c(int13, int12, int36), priority = c(1, 1, 2), within_priority = "sequential"),
    c(int13, hole, int36)
  )

  # NA elements treated as holes by default
  expect_equal(
    phint_unoverlap(c(int13, na_phint, int36)),
    c(int13, na_phint, int36)
  )

  # na_propagate = TRUE: NA propagates forward
  expect_equal(
    phint_unoverlap(c(int12, na_phint, int34), na_propagate = TRUE),
    c(int12, na_phint, na_phint)
  )

  # Vectorized over disjoint elements
  phint_multi <- phint_squash(c(int12, int34))
  expect_equal(
    phint_unoverlap(c(phint_multi, int13)),
    c(phint_multi, int23)
  )

  # Single-element priority groups: each element is its own group
  expect_equal(
    phint_unoverlap(c(int12, int13, int14), priority = c(1, 2, 3)),
    c(int12, int23, int34)
  )
  expect_equal(
    phint_unoverlap(c(int14, int13, int12), priority = c(1, 2, 3)),
    c(int14, hole, hole)
  )

  # Single-element groups with NA: treated as hole by default
  expect_equal(
    phint_unoverlap(c(int12, na_phint, int34), priority = c(1, 2, 3)),
    c(int12, na_phint, int34)
  )

  # Single-element groups with NA: propagates to lower-priority groups
  expect_equal(
    phint_unoverlap(c(int12, na_phint, int34), priority = c(1, 2, 3), na_propagate = TRUE),
    c(int12, na_phint, na_phint)
  )

  # Single-element groups: NA in first group propagates to all
  expect_equal(
    phint_unoverlap(c(na_phint, int12, int34), priority = c(1, 2, 3), na_propagate = TRUE),
    c(na_phint, na_phint, na_phint)
  )

  # Single-element groups: NA in last group does not affect earlier groups
  expect_equal(
    phint_unoverlap(c(int12, int34, na_phint), priority = c(1, 2, 3), na_propagate = TRUE),
    c(int12, int34, na_phint)
  )
})

test_that("phint_unoverlap() handles length-1, hole, and NA inputs", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  int12 <- phinterval(t1, t2)
  int13 <- phinterval(t1, t3)
  int23 <- phinterval(t2, t3)
  hole  <- hole(tzone = "UTC")
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")

  # Length-1 input is unchanged
  expect_equal(phint_unoverlap(int12), int12)
  expect_equal(phint_unoverlap(hole), hole)
  expect_equal(phint_unoverlap(na_phint), na_phint)
  expect_equal(phint_unoverlap(na_phint, na_propagate = TRUE), na_phint)

  # All holes: unchanged
  expect_equal(phint_unoverlap(c(hole, hole, hole)), c(hole, hole, hole))

  # All NA: left as-is by default
  expect_equal(
    phint_unoverlap(c(na_phint, na_phint, na_phint)),
    c(na_phint, na_phint, na_phint)
  )

  # All NA with na_propagate = TRUE
  expect_equal(
    phint_unoverlap(c(na_phint, na_phint, na_phint), na_propagate = TRUE),
    c(na_phint, na_phint, na_phint)
  )

  # Holes mixed with real intervals: holes don't contribute to mask
  expect_equal(
    phint_unoverlap(c(hole, int12, int13)),
    c(hole, int12, int23)
  )
  expect_equal(
    phint_unoverlap(c(int12, hole, int13)),
    c(int12, hole, int23)
  )

  # NA mixed with real intervals: NA ignored by default
  expect_equal(
    phint_unoverlap(c(na_phint, int12, int13)),
    c(na_phint, int12, int23)
  )
  expect_equal(
    phint_unoverlap(c(int12, na_phint, int13)),
    c(int12, na_phint, int23)
  )
})

test_that("phint_unoverlap() respects within_priority = 'keep' edge cases", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:30:00", tz = "UTC")
  int12 <- phinterval(t1, t2)
  int13 <- phinterval(t1, t3)
  int14 <- phinterval(t1, t4)
  int23 <- phinterval(t2, t3)
  int24 <- phinterval(t2, t4)
  int34 <- phinterval(t3, t4)

  # within_priority = "keep" with priority = NULL: result is unchanged
  expect_equal(
    phint_unoverlap(c(int13, int12, int14), within_priority = "keep"),
    c(int13, int12, int14)
  )

  # within_priority = "keep" with single priority group: result is unchanged
  expect_equal(
    phint_unoverlap(c(int13, int12), priority = c(1, 1), within_priority = "keep"),
    c(int13, int12)
  )

  # within_priority = "keep" still resolves cross-group overlaps
  expect_equal(
    phint_unoverlap(
      c(int13, int12, int24),
      priority = c(1, 1, 2),
      within_priority = "keep"
    ),
    c(int13, int12, int34)
  )

  # within_priority = "sequential" vs "keep" produce different results
  expect_false(identical(
    phint_unoverlap(c(int13, int12), priority = c(1, 1), within_priority = "sequential"),
    phint_unoverlap(c(int13, int12), priority = c(1, 1), within_priority = "keep")
  ))
})

test_that("phint_unoverlap() respects priority_order = 'appearance'", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:30:00", tz = "UTC")
  int12 <- phinterval(t1, t2)
  int13 <- phinterval(t1, t3)
  int14 <- phinterval(t1, t4)
  int23 <- phinterval(t2, t3)
  int24 <- phinterval(t2, t4)
  int34 <- phinterval(t3, t4)
  hole  <- hole(tzone = "UTC")

  # appearance: groups processed in order of first appearance
  # priority = c(2, 1, 2): group 2 appears first, so it blocks group 1
  expect_equal(
    phint_unoverlap(c(int13, int12, int14), priority = c(2, 1, 2), priority_order = "appearance"),
    c(int13, hole, int34)
  )

  # appearance vs asc produce different results when priority order differs
  expect_equal(
    phint_unoverlap(c(int13, int12, int14), priority = c(2, 1, 2), priority_order = "asc"),
    c(int23, int12, int34)
  )

  # appearance with already-ascending priorities matches asc
  expect_equal(
    phint_unoverlap(c(int12, int13, int14), priority = c(1, 2, 2), priority_order = "appearance"),
    phint_unoverlap(c(int12, int13, int14), priority = c(1, 2, 2), priority_order = "asc")
  )
})

test_that("phint_unoverlap() handles na_propagate = TRUE with priority groups", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:30:00", tz = "UTC")
  int12 <- phinterval(t1, t2)
  int23 <- phinterval(t2, t3)
  int13 <- phinterval(t1, t3)
  int34 <- phinterval(t3, t4)
  hole  <- hole(tzone = "UTC")
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")

  # NA in high-priority group propagates to lower-priority groups
  expect_equal(
    phint_unoverlap(
      c(int12, na_phint, int34),
      priority = c(1, 1, 2),
      na_propagate = TRUE
    ),
    c(int12, na_phint, na_phint)
  )

  # NA in low-priority group does not affect higher-priority groups
  expect_equal(
    phint_unoverlap(
      c(int12, int13, na_phint),
      priority = c(1, 2, 2),
      na_propagate = TRUE
    ),
    c(int12, int23, na_phint)
  )

  # NA propagates within sequential within_priority
  expect_equal(
    phint_unoverlap(
      c(na_phint, int12, int13),
      priority = c(1, 1, 1),
      na_propagate = TRUE
    ),
    c(na_phint, na_phint, na_phint)
  )
})

test_that("phint_unoverlap() handles multi-span elements correctly", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:30:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:40:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:50:00", tz = "UTC")
  int12 <- phinterval(t1, t2)
  int23 <- phinterval(t2, t3)
  int34 <- phinterval(t3, t4)
  int45 <- phinterval(t4, t5)
  int56 <- phinterval(t5, t6)
  int14 <- phinterval(t1, t4)
  int13 <- phinterval(t1, t3)
  int36 <- phinterval(t3, t6)
  hole  <- hole(tzone = "UTC")

  phint_12_34 <- phint_squash(c(int12, int34))
  phint_23_45 <- phint_squash(c(int23, int45))
  phint_12_56 <- phint_squash(c(int12, int56))
  phint_13_56 <- phint_squash(c(int13, int56))

  # Multi-span blocker: each span of the blocking element contributes to mask
  expect_equal(
    phint_unoverlap(c(phint_12_34, int14)),
    c(phint_12_34, int23)
  )

  # Hole punch
  expect_equal(
    phint_unoverlap(c(int23, int14)),
    c(int23, phint_12_34)
  )

  # Multi-span blocked: each span is trimmed independently
  expect_equal(phint_unoverlap(c(int13, phint_23_45)), c(int13, int45))
  expect_equal(phint_unoverlap(c(int13, int36, phint_23_45)), c(int13, int36, hole))

  # Both blocker and blocked are multi-span
  expect_equal(
    phint_unoverlap(c(phint_12_34, phint_23_45)),
    c(phint_12_34, phint_23_45)
  )
  expect_equal(
    phint_unoverlap(c(phint_13_56, phint_23_45)),
    c(phint_13_56, int45)
  )

  # Multi-span with priority
  expect_equal(
    phint_unoverlap(
      c(phint_12_34, phint_23_45, int56),
      priority = c(2, 1, 2)
    ),
    c(phint_squash(c(int12, int34)), phint_23_45, int56)
  )
})

test_that("phint_unoverlap() handles within_priority = 'keep' with na_propagate = TRUE", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:30:00", tz = "UTC")
  int12 <- phinterval(t1, t2)
  int13 <- phinterval(t1, t3)
  int23 <- phinterval(t2, t3)
  int34 <- phinterval(t3, t4)
  hole  <- hole(tzone = "UTC")
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")

  # NA within a group does NOT propagate to same-priority elements
  # (within_priority = "keep" means same-priority elements are independent)
  expect_equal(
    phint_unoverlap(
      c(int12, na_phint, int13),
      priority = c(1, 2, 2),
      within_priority = "keep",
      na_propagate = TRUE
    ),
    c(int12, na_phint, int23)
  )

  # NA does propagate to lower-priority groups
  expect_equal(
    phint_unoverlap(
      c(na_phint, int12, int34),
      priority = c(1, 1, 2),
      within_priority = "keep",
      na_propagate = TRUE
    ),
    c(na_phint, int12, na_phint)
  )

  # NA after non-NA in same group: does NOT propagate within group
  expect_equal(
    phint_unoverlap(
      c(int12, na_phint, int34),
      priority = c(1, 1, 2),
      within_priority = "keep",
      na_propagate = TRUE
    ),
    c(int12, na_phint, na_phint)
  )

  # All NA in group: propagates to lower-priority groups
  expect_equal(
    phint_unoverlap(
      c(na_phint, na_phint, int34),
      priority = c(1, 1, 2),
      within_priority = "keep",
      na_propagate = TRUE
    ),
    c(na_phint, na_phint, na_phint)
  )

  # NA in group with keep: same-priority non-NA elements are unaffected
  expect_equal(
    phint_unoverlap(
      c(int12, na_phint, int13, int34),
      priority = c(1, 1, 1, 2),
      within_priority = "keep",
      na_propagate = TRUE
    ),
    c(int12, na_phint, int13, na_phint)
  )

  # Contrast with within_priority = "sequential": NA propagates within group
  expect_equal(
    phint_unoverlap(
      c(int12, na_phint, int13, int34),
      priority = c(1, 1, 1, 2),
      within_priority = "sequential",
      na_propagate = TRUE
    ),
    c(int12, na_phint, na_phint, na_phint)
  )
})

# phint_has_overlaps -----------------------------------------------------------

test_that("phint_has_overlaps() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:30:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:40:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:50:00", tz = "UTC")
  int12 <- phinterval(t1, t2)
  int23 <- phinterval(t2, t3)
  int34 <- phinterval(t3, t4)
  int45 <- phinterval(t4, t5)
  int56 <- phinterval(t5, t6)
  int13 <- phinterval(t1, t3)
  int14 <- phinterval(t1, t4)
  int36 <- phinterval(t3, t6)
  hole  <- hole(tzone = "UTC")
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")

  # No overlaps: all FALSE
  expect_equal(phint_has_overlaps(int12), FALSE)
  expect_equal(phint_has_overlaps(c(int12, int34, int56)), c(FALSE, FALSE, FALSE))
  expect_equal(phint_has_overlaps(c(int12, int23, int34)), c(FALSE, FALSE, FALSE))

  # Only the blocked element is TRUE, not the blocker
  expect_equal(phint_has_overlaps(c(int13, int12)), c(FALSE, TRUE))
  expect_equal(phint_has_overlaps(c(int12, int13, int14)), c(FALSE, TRUE, TRUE))
  expect_equal(phint_has_overlaps(c(int14, int13, int36)), c(FALSE, TRUE, TRUE))

  # Identical elements: first is not flagged, rest are
  expect_equal(phint_has_overlaps(c(int12, int12, int12)), c(FALSE, TRUE, TRUE))

  # Non-overlapping elements are FALSE even when others overlap
  expect_equal(phint_has_overlaps(c(int13, int12, int56)), c(FALSE, TRUE, FALSE))

  # Holes are never blockers and never blocked
  expect_equal(phint_has_overlaps(c(hole, int12, int13)), c(FALSE, FALSE, TRUE))
  expect_equal(phint_has_overlaps(c(int12, hole, int13)), c(FALSE, FALSE, TRUE))
  expect_equal(phint_has_overlaps(c(int13, int12, hole)), c(FALSE, TRUE, FALSE))

  # na_propagate = FALSE: NA elements return FALSE
  expect_equal(phint_has_overlaps(c(int12, na_phint, int34)), c(FALSE, FALSE, FALSE))
  expect_equal(phint_has_overlaps(c(int13, na_phint, int12)), c(FALSE, FALSE, TRUE))
  expect_equal(phint_has_overlaps(c(na_phint, int12, int13)), c(FALSE, FALSE, TRUE))

  # na_propagate = TRUE: NA propagates forward
  expect_equal(
    phint_has_overlaps(c(int12, na_phint, int34), na_propagate = TRUE),
    c(FALSE, NA, NA)
  )
  expect_equal(
    phint_has_overlaps(c(na_phint, int12, int13), na_propagate = TRUE),
    c(NA, NA, NA)
  )
  expect_equal(
    phint_has_overlaps(c(int12, int34, na_phint), na_propagate = TRUE),
    c(FALSE, FALSE, NA)
  )

  # priority: only blocked elements (higher priority) are TRUE
  expect_equal(
    phint_has_overlaps(c(int13, int12, int36), priority = c(1, 2, 1)),
    c(FALSE, TRUE, FALSE)
  )
  expect_equal(
    phint_has_overlaps(c(int12, int13, int36), priority = c(2, 1, 2)),
    c(TRUE, FALSE, FALSE)
  )

  # priority_order = "desc": higher values processed first
  expect_equal(
    phint_has_overlaps(c(int13, int12, int36), priority = c(1, 2, 1), priority_order = "desc"),
    c(TRUE, FALSE, FALSE)
  )

  # within_priority = "keep": within-group overlaps not flagged
  expect_equal(
    phint_has_overlaps(c(int13, int12, int36), priority = c(1, 1, 2), within_priority = "keep"),
    c(FALSE, FALSE, FALSE)
  )
  expect_equal(
    phint_has_overlaps(c(int13, int12, int36), priority = c(1, 1, 1), within_priority = "keep"),
    c(FALSE, FALSE, FALSE)
  )

  # within_priority = "sequential": within-group overlaps are flagged
  expect_equal(
    phint_has_overlaps(c(int13, int12, int36), priority = c(1, 1, 2), within_priority = "sequential"),
    c(FALSE, TRUE, FALSE)
  )

  # Invariant: phint_unoverlap() ensures phint_has_overlaps() is FALSE
  phint <- c(int12, int13, int14)
  expect_false(any(phint_has_overlaps(phint_unoverlap(phint)), na.rm = TRUE))

  # Invariant: non-overlapping elements are unchanged by phint_unoverlap()
  phint <- c(int13, int12, int56)
  overlapping <- phint_has_overlaps(phint)
  expect_equal(phint[!overlapping], phint_unoverlap(phint)[!overlapping])
})

test_that("phint_has_overlaps() invariants hold", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:30:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:40:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:50:00", tz = "UTC")
  int12 <- phinterval(t1, t2)
  int13 <- phinterval(t1, t3)
  int14 <- phinterval(t1, t4)
  int34 <- phinterval(t3, t4)
  int36 <- phinterval(t3, t6)
  int56 <- phinterval(t5, t6)
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")
  phint_12_34 <- phint_squash(c(int12, int34))
  phint_23_45 <- phint_squash(c(phinterval(t2, t3), phinterval(t4, t5)))

  check_invariants <- function(phint, ...) {
    overlapping  <- phint_has_overlaps(phint, ...)
    unoverlapped <- phint_unoverlap(phint, ...)
    not_overlapping <- !overlapping & !is.na(overlapping)
    # Non-overlapping elements are unchanged by phint_unoverlap()
    expect_equal(phint[not_overlapping], unoverlapped[not_overlapping])
    # After phint_unoverlap(), phint_has_overlaps() is FALSE for all elements
    expect_false(any(phint_has_overlaps(unoverlapped, ...), na.rm = TRUE))
  }

  # No priority
  check_invariants(c(int12, int13, int14))
  check_invariants(c(int13, int12, int56))
  check_invariants(c(int12, int12, int12))

  # With priority
  check_invariants(c(int13, int12, int36), priority = c(1, 2, 1))
  check_invariants(c(int12, int13, int36), priority = c(2, 1, 2))
  check_invariants(c(int13, int12, int36), priority = c(1, 2, 1), priority_order = "desc")
  check_invariants(c(int13, int12, int14), priority = c(2, 1, 2), priority_order = "appearance")

  # within_priority = "keep"
  check_invariants(c(int13, int12, int36), priority = c(1, 1, 2), within_priority = "keep")
  check_invariants(c(int13, int12, int36), priority = c(1, 1, 1), within_priority = "keep")

  # NA elements
  check_invariants(c(int12, na_phint, int34))
  check_invariants(c(int12, na_phint, int34), na_propagate = TRUE)
  check_invariants(c(int13, int12, int36), priority = c(1, 1, 2), na_propagate = TRUE)

  # Multi-span elements
  check_invariants(c(phint_12_34, int13))
  check_invariants(c(phint_12_34, phint_23_45))
})

# phint_any_overlaps -----------------------------------------------------------

test_that("phint_any_overlaps() works as expected", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:30:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:40:00", tz = "UTC")
  int12 <- phinterval(t1, t2)
  int13 <- phinterval(t1, t3)
  int34 <- phinterval(t3, t4)
  int56 <- phinterval(t4, t5)
  hole  <- hole(tzone = "UTC")
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")

  # No overlaps: FALSE
  expect_false(phint_any_overlaps(int12))
  expect_false(phint_any_overlaps(c(int12, int34, int56)))

  # Overlapping: TRUE
  expect_true(phint_any_overlaps(c(int13, int12)))
  expect_true(phint_any_overlaps(c(int12, int13, int34)))

  # NA elements are ignored (na.rm = TRUE behaviour)
  expect_false(phint_any_overlaps(c(int12, na_phint, int34)))
  expect_true(phint_any_overlaps(c(int13, na_phint, int12)))

  # na_propagate = TRUE: propagated NAs are ignored
  expect_false(phint_any_overlaps(c(na_phint, int12, int34), na_propagate = TRUE))
  expect_false(phint_any_overlaps(c(int12, na_phint, int34), na_propagate = TRUE))

  # Invariant: equivalent to any(phint_has_overlaps(...), na.rm = TRUE)
  phint <- c(int13, int12, int56)
  expect_equal(
    phint_any_overlaps(phint),
    any(phint_has_overlaps(phint), na.rm = TRUE)
  )
  phint_p <- c(int13, int12, int34)
  priority <- c(1, 2, 1)
  expect_equal(
    phint_any_overlaps(phint_p, priority = priority),
    any(phint_has_overlaps(phint_p, priority = priority), na.rm = TRUE)
  )
})

test_that("phint_any_overlaps() invariants hold", {
  t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
  t2 <- as.POSIXct("2021-01-01 00:10:00", tz = "UTC")
  t3 <- as.POSIXct("2021-01-01 00:20:00", tz = "UTC")
  t4 <- as.POSIXct("2021-01-01 00:30:00", tz = "UTC")
  t5 <- as.POSIXct("2021-01-01 00:40:00", tz = "UTC")
  t6 <- as.POSIXct("2021-01-01 00:50:00", tz = "UTC")
  int12 <- phinterval(t1, t2)
  int13 <- phinterval(t1, t3)
  int14 <- phinterval(t1, t4)
  int34 <- phinterval(t3, t4)
  int36 <- phinterval(t3, t6)
  int56 <- phinterval(t5, t6)
  na_phint <- phinterval(NA_POSIXct_, NA_POSIXct_, tzone = "UTC")
  phint_12_34 <- phint_squash(c(int12, int34))
  phint_23_45 <- phint_squash(c(phinterval(t2, t3), phinterval(t4, t5)))

  check_invariant <- function(phint, ...) {
    # phint_any_overlaps() == any(phint_has_overlaps(...), na.rm = TRUE)
    expect_equal(
      phint_any_overlaps(phint, ...),
      any(phint_has_overlaps(phint, ...), na.rm = TRUE)
    )
    # After phint_unoverlap(), phint_any_overlaps() is FALSE
    expect_false(phint_any_overlaps(phint_unoverlap(phint, ...), ...))
  }

  # No priority
  check_invariant(c(int12, int13, int14))
  check_invariant(c(int13, int12, int56))
  check_invariant(c(int12, int34, int56))

  # With priority
  check_invariant(c(int13, int12, int36), priority = c(1, 2, 1))
  check_invariant(c(int12, int13, int36), priority = c(2, 1, 2))
  check_invariant(c(int13, int12, int36), priority = c(1, 2, 1), priority_order = "desc")
  check_invariant(c(int13, int12, int14), priority = c(2, 1, 2), priority_order = "appearance")

  # within_priority = "keep"
  check_invariant(c(int13, int12, int36), priority = c(1, 1, 2), within_priority = "keep")
  check_invariant(c(int13, int12, int36), priority = c(1, 1, 1), within_priority = "keep")

  # NA elements
  check_invariant(c(int12, na_phint, int34))
  check_invariant(c(int12, na_phint, int34), na_propagate = TRUE)
  check_invariant(c(int13, int12, int36), priority = c(1, 1, 2), na_propagate = TRUE)

  # Multi-span elements
  check_invariant(c(phint_12_34, int13))
  check_invariant(c(phint_12_34, phint_23_45))
})
