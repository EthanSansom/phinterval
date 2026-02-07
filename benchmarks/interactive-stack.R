# Car Owner -------------------------------------------------------------------
# https://stackoverflow.com/questions/79042272/finding-date-intervals-in-dataframe-with-plausibilty-for-breaks-or-overlap-in-r/79043235#79043235

person <- c("Layla", "Layla", "Layla", "John", "John", "John", "Bo", "Bo")
car <- c("Volvo x", "Volvo y", "Fiat Q", "Tesla C", "Mazda H", "Ford W", "Honda 1",  "Honda 2")
from_date <- as.Date(c("2000-01-01", "2009-01-02", "2011-01-05", "2000-08-01", "2004-07-09", "2008-01-01", "2001-01-01", "2003-07-01"))
end_date <- as.Date(c("2010-01-01", "2012-07-01", "2015-08-09", "2002-01-01", "2020-10-22", "2010-01-01", "2020-08-09", "2019-10-01"))

data <- data.frame(
  person = c("Layla", "Layla", "Layla", "John", "John", "John", "Bo", "Bo"),
  car = c("Volvo x", "Volvo y", "Fiat Q", "Tesla C", "Mazda H", "Ford W", "Honda 1",  "Honda 2"),
  from_date = as.Date(c("2000-01-01", "2009-01-02", "2011-01-05", "2000-08-01", "2004-07-09", "2008-01-01", "2001-01-01", "2003-07-01")),
  end_date = as.Date(c("2010-01-01", "2012-07-01", "2015-08-09", "2002-01-01", "2020-10-22", "2010-01-01", "2020-08-09", "2019-10-01"))
)

data |>
  group_by(person) |>
  summarise(owned_car = datetime_squash(from_date, end_date)) |>
  mutate(carowner_yearsum = owned_car %/% duration(years = 1))

datetime_squash(
  start = data$from_date,
  end = data$end_date,
  by = data$person,
  keep_by = TRUE
) |>
  mutate(carowner_yearsum = phint %/% duration(years = 1))

# Business Hours ---------------------------------------------------------------
# https://stackoverflow.com/questions/63139030/how-to-calculate-business-hours-between-two-dates-when-business-hours-vary-depen

# TODO: This one is old and has been answered. Also you can do it with lubridate.

library(lubridate)

df <- tibble::tribble(
  ~start,               ~end,            ~expected_hours,
  "07/24/2020 22:20",   "07/25/2020 21:20",   12.67,
  "07/14/2020 21:00",   "07/16/2020 09:30",   18.50,
  "07/18/2020 08:26",   "07/19/2020 10:00",   13.00,
  "07/10/2020 08:00",   "07/13/2020 11:00",   42.00
) |>
  mutate(across(c(start, end), mdy_hm))

business_days <- seq.Date(as.Date(min(df$start)), as.Date(max(df$end)), by = "day")
weekends <- weekdays(business_days) %in% c("Saturday", "Sunday")
starts <- business_days + dhours(8 + weekends)
ends <- business_days + dhours(21 + (2 * !weekends))

work_hours <- datetime_squash(starts, ends)

df |>
  mutate(
    open = phint_intersect(interval(start, end), work_hours),
    business_hours = open / duration(hours = 1)
  ) |>
  select(open, business_hours, expected_hours)

# Alternative, generate all possible business days first, then join
possible_hours <- tibble(start = starts, end = ends)
realized_hours <- df |>
  inner_join(
    possible_hours,
    by = join_by(overlaps(x$start, x$end, y$start, y$end))
  ) |>
  mutate(open = intersect(start.x %--% end.x, start.y %--% end.y)) |>
  group_by(start = start.x, end = end.x) |>
  summarize(
    expected_hours = first(expected_hours),
    hours = sum(open / duration(hours = 1)),
    .groups = "drop"
  )

# Machine Event Data -----------------------------------------------------------
# https://stackoverflow.com/questions/79610857/how-to-split-overlapping-time-intervals-in-r-assign-precedence-and-fill-gaps-w

startstate <- structure(list(
  event_code = c(101, 202, 303, 202, 404),
  start_time = structure(c(1735689600, 1735707600, 1735725600, 1735743600, 1735756200), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  end_time   = structure(c(1735718400, 1735722000, 1735740000, 1735758000, 1735776000), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
  precedence = structure(c(1L, 2L, 1L, 2L, 3L), levels = c("Level 1", "Level 2", "Level 3"), class = c("ordered", "factor"))
), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -5L))

endstate <- structure(list(
  event_code = c(101, 202, 0, 303, 0, 202, 404),
  start_time = structure(c(1735689600, 1735707600, 1735722000, 1735725600, 1735740000, 1735743600, 1735756200), tzone = "UTC", class = c("POSIXct", "POSIXt")),
  end_time   = structure(c(1735707600, 1735722000, 1735725600, 1735740000, 1735743600, 1735756200, 1735776000), tzone = "UTC", class = c("POSIXct", "POSIXt")),
  precedence = c("Level 1", "Level 2", "Level 0", "Level 1", "Level 0", "Level 2", "Level 3")
), row.names = c(NA, -7L), class = c("tbl_df", "tbl", "data.frame"))

# Combine events by code and precedence
events <- startstate |>
  summarize(
    event = datetime_squash(start_time, end_time),
    .by = c(event_code, precedence)
  )

# `datetime_squash()` and `phint_squash()` merge overlapping spans
events

# `phint_invert()` gets times in between disjoint spans
events |> mutate(intervening = phint_invert(event)) |> select(-event)

# `phint_unnest()` returns a tibble of [start, end] spans
events |> reframe(phint_unnest(event, key = pick(event_code, precedence)))

# Get the time-spans within any higher precedence event to remove
higher_events <- events |>
  summarize(level_events = phint_squash(event), .by = precedence) |>
  mutate(
    # Take the cumulative union of higher precedence events
    higher_precedence_events = purrr::accumulate(
      c(level_events[-1], hole()),
      phint_union,
      .dir = "backward"
    )
  ) |>
  select(precedence, higher_precedence_events)

# If you have a small fixed number of levels, this is faster
higher_events <- tibble(
  precedence = unique(events$precedence),
  higher_precedence_events = c(
    phint_squash(events$event[events$precedence > "Level 1"]), # > Level 1
    phint_squash(events$event[events$precedence > "Level 2"]), # > Level 2
    hole() # Nothing is > Level 3
  )
)

# Remove between-precedence overlaps, prioritizing higher precedence
events <- events |>
  left_join(higher_events, by = "precedence") |>
  mutate(event = phint_setdiff(event, higher_precedence_events))

# Get the intervening times between events of any precedence
intervening <- events |>
  summarize(
    event_code = 0,
    event = event |> phint_squash() |> phint_invert(),
    precedence = "Level 0"
  )

# Unnest the events into a data frame of start and end times
result <- bind_rows(events, intervening) |>
  reframe(phint_unnest(event), .by = c(event_code, precedence)) |>
  select(event_code, start_time = start, end_time = end, precedence) |>
  arrange(start_time)

waldo::compare(result, endstate)

# We're a little bit faster, maybe more so on longer data
bench::mark(
  {
    startstate |>
      pivot_longer(start_time:end_time, values_to = "start_time") |>
      arrange(start_time) |>
      mutate(active_by_level = cumsum(name == "start_time") -
               cumsum(name == "end_time"), .by = precedence) |>
      pivot_wider(names_from = precedence, values_from = active_by_level) |>
      mutate(`Level 0` = 1) |>
      fill(everything()) |>
      pivot_longer(-c(event_code:start_time), names_to = "precedence", values_drop_na = TRUE) |>
      summarize(precedence = max(precedence[value > 0]), .by = start_time) |>
      filter(precedence != lag(precedence, 1, "")) |>
      left_join(startstate, by = c("start_time", "precedence")) |>
      transmute(event_code = coalesce(as.character(event_code), "0"),
                start_time, end_time = coalesce(lead(start_time), end_time),
                precedence) |>
      filter(!is.na(end_time)) |>
      mutate(event_code = as.integer(event_code))
  },

  {
    startstate %>%
      summarize(
        event = datetime_squash(start_time, end_time),
        .by = c(event_code, precedence)
      ) %>%
      left_join(
        tibble(
          precedence = c("Level 1", "Level 2", "Level 3"),
          higher_precedence = c(
            phint_squash(.$event[.$precedence > "Level 1"]),
            phint_squash(.$event[.$precedence > "Level 2"]),
            hole()
          )
        ),
        by = "precedence"
      ) %>%
      mutate(event = phint_setdiff(event, higher_precedence)) %>%
      bind_rows(
        summarize(
          .,
          event_code = 0,
          event = event |> phint_squash() |> phint_invert(),
          precedence = "Level 0"
        )
      ) %>%
      reframe(phint_unnest(event), .by = c(event_code, precedence)) %>%
      select(event_code, start_time = start, end_time = end, precedence) %>%
      arrange(start_time)
  }
)
