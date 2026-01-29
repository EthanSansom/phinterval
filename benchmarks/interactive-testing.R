# load -------------------------------------------------------------------------

load_all()

# bounds -----------------------------------------------------------------------

s1 <- as.POSIXct(1)
s2 <- as.POSIXct(2)
s3 <- as.POSIXct(3)
s4 <- as.POSIXct(4)

int12 <- interval(s1, s2)
int23 <- interval(s2, s3)
int34 <- interval(s3, s4)

int13 <- interval(s1, s3)
int24 <- interval(s2, s4)

phint_overlaps(int12, int23)
phint_overlaps(int12, int23, bounds = "()")
phint_overlaps(int13, int24, bounds = "()")

# docs -------------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)
library(tidyr)

set.seed(1)

jobs <- tibble::tribble(
  ~fname,     ~lname,      ~job_title,             ~start,        ~end,
  "Greg",     "Hirsch",    "Mascot",               "2018-01-01",  "2018-06-03",
  "Greg",     "Hirsch",    "Executive Assistant",  "2018-06-10",  "2020-04-01",
  "Greg",     "Hirsch",    "Chief of Staff",       "2020-03-01",  "2020-11-28",
  "Tom",      "Wambsgans", "Chairman",             "2019-05-01",  "2020-11-10",
  "Tom",      "Wambsgans", "CEO",                  "2020-11-10",  "2020-12-31",
  "Shiv",     "Roy",       "Political Consultant", "2017-01-01",  "2019-04-01",
  "Kendall",  "Roy",       "COO",                  "2018-01-01",  "2018-08-15",
  "Kendall",  "Roy",       "Acting CEO",           "2018-08-15",  "2018-10-30",
  "Roman",    "Roy",       "COO",                  "2019-01-01",  "2020-06-30",
  "Roman",    "Roy",       "CEO",                  "2020-07-01",  "2020-09-15",
  "Gerri",    "Kellman",   "General Counsel",      "2015-01-01",  "2020-12-31",
  "Gerri",    "Kellman",   "Interim CEO",          "2018-10-30",  "2019-02-01",
  "Frank",    "Vernon",    "COO",                  "2016-01-01",  "2018-12-31",
  "Karl",     "Muller",    "CFO",                  "2016-01-01",  "2020-12-31",
  "Connor",   "Roy",       "Entrepreneur",         "2010-01-01",  "2020-12-31"
)

# Example: We can use `datetime_squash()` by argument to emulate dplyr workflow
jobs |>
  group_by(fname) |>
  summarize(phint = datetime_squash(ymd(start), ymd(end))) |>
  ungroup()

# Notice: `datetime_squash()` returns a `data.frame` by default, we cast to a
#         tibble for nicer formatting.
#
# Notice: `dplyr::group_by()` sorts the output by `fname` (alphabetically in this
# case), while `datetime_squash()` returns the `by` groups by first appearance
datetime_squash(
  start = ymd(jobs$start),
  end = ymd(jobs$end),
  by = jobs$fname,
  keep_by = TRUE
) |> as_tibble()

# Example: To emulate `dplyr` ordering, use the `order_by` argument
datetime_squash(
  start = ymd(jobs$start),
  end = ymd(jobs$end),
  by = jobs$fname,
  keep_by = TRUE,
  order_by = TRUE
) |> as_tibble()

# Example: In many workflows, `by` includes multiple columns. `datetime_squash()`
# and `phint_squash()` `by` argument can be a tibble.
jobs |>
  group_by(fname, lname) |>
  summarize(phint = datetime_squash(ymd(start), ymd(end))) |>
  ungroup()

# Notice: In a `dplyr` pipeline, reframe with `by` can emulate above
jobs |>
  reframe(
    datetime_squash(
      start = ymd(start),
      end = ymd(end),
      by = pick(fname, lname),
      keep_by = TRUE,
      order_by = TRUE
    )
  ) |>
  unnest(by) # Unnests fname, lname data.frame

# Example
big_jobs <- jobs |>
  slice_sample(n = 20000) |>
  mutate(lname = sample(LETTERS, size = n(), replace = TRUE))

head(big_jobs)

bench::mark(
  group_by = big_jobs |>
    mutate(start = ymd(start), end = ymd(end)) |>
    group_by(job_title, lname) |>
    summarize(phint = datetime_squash(start, end), .groups = "drop"),

  squash_by = datetime_squash(
    start = ymd(big_jobs$start),
    end = ymd(big_jobs$end),
    by = big_jobs |> select(job_title, lname),
    keep_by = TRUE,
    order_by = TRUE
  ) |>
    unnest(by)
)[1:6]

datetime_squash(
  start = ymd(jobs$start),
  end = ymd(jobs$end),
  # `by` works like the `.by` argument in `dplyr::summarize()`
  by = jobs |> select(fname, lname),
  keep_by = TRUE
) |>
  as_tibble() # For nicer formatting

employment <- jobs |>
  dplyr::mutate(span = interval(start, end)) |>
  dplyr::group_by(name) |>
  dplyr::summarize(employed = phint_squash(span))

employment

employment |>
  group_by(name) |>
  reframe(periods = phint_unnest(employed)) |>
  unnest(periods)

employment |>
  reframe(phint_unnest(employed, key = name))

# basics -----------------------------------------------------------------------

opts <- options("phinterval.print_max_width")
options(digits.secs = 0L)

starts <- seq(as.Date("2021-01-01"), length.out = 5, by = "day")
ends <- starts + 1
phint <- phinterval(starts, ends, by = c(1, 4, 3, 4, 4), tzone = "EST")
phint[1] <- NA
phint <- c(phint, new_phinterval(0L, list(numeric(0)), list(numeric(0))))

options(phinterval.print_max_width = 90)
phint

options(phinterval.print_max_width = 60)
phint

options(phinterval.print_max_width = 15)
phint

tibble::tibble(
  phint = phint,
  intvl = interval(phint_start(phint), phint_end(phint))
)

# Reset options
options(opts)

# pillar -----------------------------------------------------------------------

starts <- as.POSIXct(seq(as.Date("2021-01-01"), length.out = 5, by = "day")) + 0.05
ends <- starts + (24 * 60 * 60)

phint <- phinterval(starts, ends, by = c(1, 4, 3, 4, 4), tzone = "EST")
intvl <- interval(starts, ends, tzone = "EST")

phint
intvl

tibble::tibble(
  # intvl = intvl[1:3],
  phint = phint,
  start = starts[1:3]
)

options(digits.secs = 4)
phint
intvl

options(digits.secs = 0)

# unnest -----------------------------------------------------------------------

intvl <- interval(starts, ends, tzone = "EST")

phint_unnest(phint, hole_to = c("na"), keep_size = TRUE)
phint_unnest(intvl, hole_to = c("na"), keep_size = FALSE)

# invert -----------------------------------------------------------------------

starts <- as.POSIXct(c(1, 10, 20))
ends <- as.POSIXct(c(5, 13, 30))
phint <- phinterval(starts, ends, by = 1L, tzone = "UTC")

phint_invert(phint) |> phint_unnest() |> dplyr::mutate(across(-key, as.numeric)) |> tibble::as_tibble()
phint_complement(phint) |> phint_unnest() |> dplyr::mutate(across(-key, as.numeric)) |> tibble::as_tibble()

phint <- c(phint, new_hole())
phint_invert(phint, hole_to = "inf")
phint_invert(phint, hole_to = "na")
phint_invert(phint, hole_to = "hole")

phint_complement(phint) |> phint_unnest() |> dplyr::mutate(across(-key, as.numeric)) |> tibble::as_tibble()

# sift -------------------------------------------------------------------------

starts <- as.POSIXct(c(1, 10, 20))
ends <- as.POSIXct(c(5, 13, 20))
phint <- phinterval(starts, ends, by = 1L, tzone = "EST")

phint
phint_sift(phint)

# datetime squash --------------------------------------------------------------

datetime_squash(
  as.POSIXct(c(1, 10, 20), "UTC"),
  as.POSIXct(c(5, 13, 20), "UTC"),
  by = c(1, 1, 2)
)

x <- 1:10
bench::mark(is_string(x), anyNA(x))

# time zone ---------------------------------------------------------------------

phint <- phinterval(as.POSIXct(c(1, 10, 20)), as.POSIXct(c(5, 13, 25)))
attr(phint, "tzone") <- NA_character_
unclass(phint)

intvl <- interval(as.POSIXct(0), as.POSIXct(1))
attr(intvl, "tzone") <- NA_character_
intvl
