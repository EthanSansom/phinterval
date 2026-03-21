#' Resolve overlapping intervals sequentially or by priority
#'
#' @description
#'
#' `phint_unoverlap()` removes overlaps across elements of a `<phinterval>`
#' vector by trimming each element against all preceding elements. The result
#' is a vector where no two elements share any time.
#'
#' Without `priority`, each element is trimmed by the union of all previous
#' elements:
#'
#' ```
#' result[i] = phint_setdiff(phint[i], phint_squash(phint[1:(i - 1)]))
#' ```
#'
#' With `priority`, elements are grouped and processed in `priority_order`.
#' Each element is trimmed by all elements from earlier priority groups. Within
#' a priority group, `within_priority` controls whether overlapping elements
#' within each group are kept as-is or trimmed.
#'
#' @inheritParams params
#'
#' @param priority `[vector / NULL]`
#'
#' An optional grouping vector defining priority groups. Earlier groups (per
#' `priority_order`) are processed first and block later groups. Must be
#' recyclable with `phint`. If `NULL` (the default), all elements are resolved
#' considered to be within the same group.
#'
#' `priority` may be any vector in the vctrs sense. See [vctrs::obj_is_vector()]
#' for details.
#'
#' @param priority_order `["asc" / "desc" / "appearance"]`
#'
#' How to order priority groups for processing:
#' - `"asc"` (default): Lower values are processed first (priority 1 before 2).
#' - `"desc"`: Higher values are processed first (priority 9 before 2).
#' - `"appearance"`: Groups are processed in order of first appearance in `priority`.
#'
#' @param within_priority `["sequential" / "keep"]`
#'
#' How to handle overlaps within the same priority group:
#' - `"sequential"` (default): Overlaps within a group are resolved by row
#'   order, so earlier elements block later elements within the same group.
#' - `"keep"`: Overlaps within a group are preserved; only overlaps with
#'   higher-priority groups are removed.
#'
#' @param na_propagate `[FALSE / TRUE]`
#'
#' Whether `NA` elements propagate to subsequent elements:
#' - `FALSE` (default): `NA` elements are treated as [hole()]s and do not
#'   affect subsequent results.
#' - `TRUE`: An `NA` element causes all subsequent elements (or lower-priority
#'   group elements) to become `NA`.
#'
#' @return A `<phinterval>` vector the same length as `phint`, where no two
#' elements overlap.
#'
#' @seealso
#' - [phint_has_overlaps()] to test whether a `<phinterval>` vector has
#'   cross-element overlaps.
#'
#' @examples
#' monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
#' tuesday <- interval(as.Date("2025-11-11"), as.Date("2025-11-12"))
#' wednesday <- interval(as.Date("2025-11-12"), as.Date("2025-11-13"))
#' mon_to_wed <- interval(as.Date("2025-11-10"), as.Date("2025-11-13"))
#' mon_to_tue <- interval(as.Date("2025-11-10"), as.Date("2025-11-12"))
#'
#' # Sequential removal: each element is trimmed by all previous elements
#' phint_unoverlap(c(wednesday, mon_to_wed, mon_to_tue))
#'
#' # Priority-based: lower priority values are processed first
#' phint_unoverlap(
#'   c(mon_to_wed, mon_to_tue, wednesday),
#'   priority = c(1, 2, 1)
#' )
#'
#' # within_priority = "keep": overlaps within a group are preserved
#' phint_unoverlap(
#'   c(mon_to_wed, mon_to_tue, wednesday),
#'   priority = c(1, 1, 2),
#'   within_priority = "keep"
#' )
#'
#' # priority_order = "desc": higher priority values are processed first
#' phint_unoverlap(
#'   c(mon_to_wed, mon_to_tue, wednesday),
#'   priority = c(1, 2, 1),
#'   priority_order = "desc"
#' )
#'
#' # NA elements are treated as holes by default
#' phint_unoverlap(c(mon_to_wed, NA, wednesday))
#'
#' # NA elements propagate forward with na_propagate = TRUE
#' phint_unoverlap(c(mon_to_wed, NA, wednesday), na_propagate = TRUE)
#'
#' @export
phint_unoverlap <- function(
    phint,
    priority = NULL,
    priority_order = c("asc", "desc", "appearance"),
    within_priority = c("sequential", "keep"),
    na_propagate = FALSE
) {
  phint_type <- validate_type_phintish(phint)
  priority <- priority %||% 1L
  check_vector(priority)
  check_recycleable_to(priority, phint)

  priority_order <- arg_match0(priority_order, c("asc", "desc", "appearance"))
  within_priority <- arg_match0(within_priority, c("sequential", "keep"))
  check_bool(na_propagate)

  # If `priority` is scalar, consider all elements to be within the same group
  if (is_scalar(priority)) {
    if (within_priority == "keep") {
      return(phint)
    }
    out <- phint_unary_dispatch(
      x = phint,
      x_type = phint_type,
      funs_cpp = list(
        phint = phint_unoverlap_within_cpp,
        intvl = intvl_unoverlap_within_cpp
      ),
      na_propagate = na_propagate
    )
    return(new_phinterval_bare(out, tzone = get_tzone(phint)))
  }

  priority_groups <- switch(
    priority_order,
    asc = vec_locate_sorted_groups(priority, direction = "asc"),
    desc = vec_locate_sorted_groups(priority, direction = "desc"),
    appearance = vec_group_loc(priority)
  )

  out <- phint_unary_dispatch(
    x = phint,
    x_type = phint_type,
    funs_cpp = list(
      phint = phint_unoverlap_cpp,
      intvl = intvl_unoverlap_cpp
    ),
    priority_locs = .subset2(priority_groups, "loc"),
    within_priority = within_priority,
    na_propagate = na_propagate
  )
  new_phinterval_bare(out, tzone = get_tzone(phint))
}

#' Test whether a phinterval vector has cross-element overlaps
#'
#' @description
#'
#' `phint_has_overlaps()` returns a logical vector indicating which elements
#' of `phint` would be modified by [phint_unoverlap()] or are blockers of a
#' subsequent element. `phint_any_overlaps()` is a fast scalar equivalent to
#' `any(phint_has_overlaps(...), na.rm = TRUE)`.
#'
#' Both functions accept the same arguments as [phint_unoverlap()] and use the
#' same priority and within-priority resolution rules.
#'
#' The following invariants hold:
#'
#' ```r
#' # phint_unoverlap() ensures that phint_has_overlaps() is FALSE
#' phint <- phint_unoverlap(phint, ...)
#' !any(phint_has_overlaps(phint, ...), na.rm = TRUE)
#'
#' # phint_unoverlap() does not alter non-overlapping elements
#' overlapping <- phint_has_overlaps(phint, ...)
#' all(phint[!overlapping] == phint_unoverlap(phint, ...)[!overlapping])
#'
#' # phint_any_overlaps() is equivalent to any(phint_has_overlaps(...))
#' phint_any_overlaps(phint, ...) == any(phint_has_overlaps(phint, ...), na.rm = TRUE)
#' ```
#'
#' @inheritParams phint_unoverlap
#'
#' @return
#'
#' `phint_has_overlaps()` returns a logical vector the same length as `phint`:
#' - `TRUE`: the element would be modified by [phint_unoverlap()] or blocks a
#'   subsequent element.
#' - `FALSE`: the element would not be affected by [phint_unoverlap()].
#' - `NA`: the element is `NA`, or would become `NA` due to propagation when
#'   `na_propagate = TRUE`.
#'
#' `phint_any_overlaps()` returns a single `TRUE` or `FALSE`.
#'
#' @seealso
#' - [phint_unoverlap()] to resolve the overlaps detected by this function.
#'
#' @examples
#' monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
#' tuesday <- interval(as.Date("2025-11-11"), as.Date("2025-11-12"))
#' wednesday <- interval(as.Date("2025-11-12"), as.Date("2025-11-13"))
#' mon_to_wed <- interval(as.Date("2025-11-10"), as.Date("2025-11-13"))
#' mon_to_tue <- interval(as.Date("2025-11-10"), as.Date("2025-11-12"))
#'
#' # No overlaps: all FALSE
#' phint_has_overlaps(c(monday, tuesday, wednesday))
#' phint_any_overlaps(c(monday, tuesday, wednesday))
#'
#' # Overlapping: blocker and blocked are both TRUE
#' phint_has_overlaps(c(mon_to_wed, mon_to_tue))
#' phint_any_overlaps(c(mon_to_wed, mon_to_tue))
#'
#' # Non-overlapping elements are FALSE even when others overlap
#' phint_has_overlaps(c(mon_to_wed, mon_to_tue, wednesday))
#'
#' # Priority-based: same rules as phint_unoverlap()
#' phint_has_overlaps(
#'   c(mon_to_wed, mon_to_tue, wednesday),
#'   priority = c(1, 2, 1)
#' )
#'
#' # NA elements return NA
#' phint_has_overlaps(c(mon_to_wed, NA, wednesday))
#'
#' # na_propagate = TRUE: NA propagates forward
#' phint_has_overlaps(c(monday, NA, wednesday), na_propagate = TRUE)
#' phint_any_overlaps(c(monday, NA, wednesday), na_propagate = TRUE)
#'
#' @name phinterval-overlap-predicates
NULL

#' @rdname phinterval-overlap-predicates
#' @export
phint_has_overlaps <- function(
  phint,
  priority = NULL,
  priority_order = c("asc", "desc", "appearance"),
  within_priority = c("sequential", "keep"),
  na_propagate = FALSE
) {
  # TODO: Placeholder
  check_phintish(phint)
  return(rep_along(TRUE, phint))
}

#' @rdname phinterval-overlap-predicates
#' @export
phint_any_overlaps <- function(
  phint,
  priority = NULL,
  priority_order = c("asc", "desc", "appearance"),
  within_priority = c("sequential", "keep"),
  na_propagate = FALSE
) {
  # TODO: Placeholder
  check_phintish(phint)
  return(TRUE)
}
