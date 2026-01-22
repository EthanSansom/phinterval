# load -------------------------------------------------------------------------

load_all()

# basics -----------------------------------------------------------------------

opts <- options("phinterval.print_max_width")

starts <- seq(as.Date("2021-01-01"), length.out = 5, by = "day")
ends <- starts + 1
phint <- phinterval(starts, ends, by = c(1, 4, 3, 4, 4), tzone = "EST")
phint[1] <- NA
phint[2] <- new_hole()

options(phinterval.print_max_width = 90)
phint

options(phinterval.print_max_width = 50)
phint

options(phinterval.print_max_width = 15)
phint

# Reset options
options(opts)

# unnest -----------------------------------------------------------------------

intvl <- interval(starts, ends, tzone = "EST")

phint_unnest(phint, hole_to = c("na"), keep_size = TRUE)
phint_unnest(intvl, hole_to = c("na"), keep_size = FALSE)
