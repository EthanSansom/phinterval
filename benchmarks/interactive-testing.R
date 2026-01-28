# load -------------------------------------------------------------------------

load_all()

# docs -------------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)
library(tidyr)

jobs <- tibble::tribble(
  ~name,   ~job_title,             ~start,        ~end,
  "Greg",  "Mascot",               "2018-01-01",  "2018-06-03",
  "Greg",  "Executive Assistant",  "2018-06-10",  "2020-04-01",
  "Greg",  "Chief of Staff",       "2020-03-01",  "2020-11-28",
  "Tom",   "Chairman",             "2019-05-01",  "2020-11-10",
  "Tom",   "CEO",                  "2020-11-10",  "2020-12-31",
  "Shiv",  "Political Consultant", "2017-01-01",  "2019-04-01"
)

employment <- jobs |>
  dplyr::mutate(span = interval(start, end)) |>
  dplyr::group_by(name) |>
  dplyr::summarize(employed = phint_squash(span))

employment

employment |>
  group_by(name) |>
  reframe(periods = phint_unnest(employed)) |>
  unnest(periods)

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

# timezone ---------------------------------------------------------------------

phint <- phinterval(as.POSIXct(c(1, 10, 20)), as.POSIXct(c(5, 13, 25)))
attr(phint, "tzone") <- NA_character_
unclass(phint)

intvl <- interval(as.POSIXct(0), as.POSIXct(1))
attr(intvl, "tzone") <- NA_character_
intvl
