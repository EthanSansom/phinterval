load_all()
library(dplyr, warn.conflicts = FALSE)

jobs <- tribble(
  ~name,   ~job_title,             ~start,        ~end,
  "Greg",  "Mascot",               "2018-01-01",  "2018-06-03",
  "Greg",  "Executive Assistant",  "2018-06-10",  "2020-04-01",
  "Tom",   "Chairman",             "2019-05-01",  "2020-11-10",
  "Tom",   "CEO",                  "2020-11-10",  "2020-12-31",
  "Shiv",  "Political Consultant", "2017-01-01",  "2019-04-01"
)

leaves <- tribble(
  ~name,   ~job_title,             ~start,        ~end,
  "Greg",  "Executive Assistant",  "2019-12-15",  "2020-12-27",
  "Tom",   "Chairman",             "2019-06-01",  "2019-06-07",
  "Tom",   "Chairman",             "2019-12-13",  "2019-12-27",
  "Shiv",  "Political Consultant", "2017-12-23",  "2018-01-03",
  "Shiv",  "Political Consultant", "2018-10-30",  "2018-11-01",
)

# 1. Between jobs, did the employees have gaps in their employment?
# 2. How many days were employees working, while not on leave?
# 3. What about only in the 2018-06 - 2019-06 period?

employment <- jobs |>
  group_by(name) |>
  summarize(
    employed = datetime_squash(as.Date(start), as.Date(end)),
    nonemployed = phint_invert(employed)
  )

on_leave <- leaves |>
  group_by(name) |>
  summarize(leave = datetime_squash(as.Date(start), as.Date(end)))

# Days of employment, minus leave
working <- employment |>
  left_join(on_leave, by = "name") |>
  mutate(
    working = phint_setdiff(employed, leave),
    n_days = working / duration(days = 1)
  ) |>
  select(name, working, n_days)

# Working spans, within the window
window <- interval("2018-06-01", "2019-06-01")
working |>
  mutate(working_in_study = phint_intersect(working, window)) |>
  select(name, working_in_study)
