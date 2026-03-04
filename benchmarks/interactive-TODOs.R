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
