load_all()

phint <- phinterval(as.Date(1:5), as.Date(2:6))
phint[1] <- phinterval(as.Date(0), as.Date(3))

hole <- hole(tzone = get_tzone(phint))
phint[is.na(phint)] <- hole

mask <- phint_cumunion(c(hole, phint[-length(phint)]))

phint_unoverlap(phint)

# testing ----------------------------------------------------------------------

dates <- tibble::tribble(
  ~start, ~end, ~group,
  1, 5, 1,
  2, 10, 2,
  12, 13, 1,
  13, 14, 3
) |> dplyr::mutate(start = as.Date(start), end = as.Date(end))

phint <- phinterval(dates$start, dates$end)

phint_unoverlap(phint)
