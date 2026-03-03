---
title: 'phinterval: An R package for representing and manipulating time spans with gaps'
tags:
  - R
  - dates
  - timespans
  - set operations
authors:
  - name: Ethan Sansom
    orcid: 0009-0000-1573-0186
    affiliation: 1
affiliations:
  - name: "Department of Statistical Sciences, University of Toronto"
    index: 1
date: "2025-12-18"
bibliography: paper.bib
output:
  rticles::joss_article:
    keep_tex: false
  # md_document:
  #   preserve_yaml: TRUE
  #   variant: "markdown_strict"
journal: JOSS
---

# Summary

`phinterval` is an R [@R] package for representing and manipulating time spans 
that may include gaps. It implements the *phinterval* vector class, designed as 
a generalization of the `lubridate` [@lubridate] package's *Interval* class to 
support disjoint and empty time spans.

While existing interval classes represent contiguous spans of time, each element 
of a *phinterval* is a union of zero or more disjoint intervals. This enables 
representation of empty time spans (e.g., the intersection of two non-overlapping 
events) and disjoint time spans (e.g., periods of employment separated by gaps of 
unemployment).

The package provides a closed algebra of set operations on time spans: 
`phint_union()`, `phint_intersect()`, `phint_setdiff()`, and `phint_complement()` 
all accept arbitrary *phinterval* or *Interval* vectors and return valid 
*phinterval* objects, allowing analysts to safely compose operations without 
handling edge cases manually. Additional functionality includes merging overlapping 
intervals, extracting gaps between spans, and testing containment and overlap 
relationships.

The package is designed to work seamlessly with `lubridate`: all functions accept 
either *Interval* or *phinterval* vectors as input, enabling integration into 
existing workflows with minimal code changes.

<!-- TODO: Mention dependencies and add references! --->

# Statement of Need

Accurately representing and manipulating dates and times is challenging due to
complexities such as time zone adjustments, daylight saving transitions, and leap 
years. As a result, temporal data analysis is a common source of frustration 
and error-prone code [@lubridate; @itsabouttime]. Several R packages provide 
intuitive interfaces that handle these complexities automatically. In particular, 
`lubridate`, which is widely used for date-time manipulation in R, and `ivs` 
[@ivs] simplify the representation and manipulation of time spans, reducing 
users' cognitive load and the likelihood of mistakes.

However, these packages assume that time spans are contiguous. This assumption 
breaks down in common analytical scenarios where operations naturally produce 
disjoint or empty results. In longitudinal employment studies, analysts may need 
to remove periods of leave from employment intervals, creating disjoint spans. 
Summarizing total enrollment time in educational research requires taking the union 
of multiple enrollment periods separated by gaps or transfers between institutions. 
Time-use research identifies periods of multi-tasking by computing the intersection 
of reported activities. When standard set operations are applied in these
scenarios - removing sub-intervals from larger spans, taking unions of disjoint 
events, or computing intersections of potentially non-overlapping activities - the
results cannot be represented as single contiguous intervals.

Existing R packages handle these cases inconsistently. The `ivs` package raises 
errors when vectorized set operations produce disjoint results, requiring analysts 
to pre-filter their data or restructure their analysis. The `lubridate` package 
coerces empty intersections to missing values (`NA`), creating ambiguity between 
genuinely empty time spans and missing data. While the `intervals` package [@intervals] 
supports disjoint spans, it is limited to numeric endpoints (not native date-time objects) 
and implements intervals as a scalar class, requiring users working with tabular 
data to manually manage lists of scalar objects. As a result, analysts lack a 
vectorized, date-time representation for disjoint spans that integrates with 
standard tabular workflows.

The `phinterval` package addresses this gap by providing a vectorized representation
of disjoint and empty time spans with a closed algebra of set operations. By 
ensuring that all operations return valid *phinterval* objects rather than errors 
or missing values, the package enables safe composition of operations without 
manual edge case handling. To reduce friction, `phinterval` closely mirrors the 
`lubridate` interface and accepts *Interval* vectors as input, allowing it to 
integrate seamlessly into existing workflows. In @vivalt, for example, `phinterval` 
was used to construct participant employment histories from longitudinal survey 
data involving overlapping and disjoint job spells.

# Examples

# References
