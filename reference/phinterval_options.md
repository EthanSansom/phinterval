# Package options

The `phinterval` package uses the following global options to control
printing and default behaviors. These options can be set using
[`options()`](https://rdrr.io/r/base/options.html) and queried using
[`getOption()`](https://rdrr.io/r/base/options.html).

## Options

- `phinterval.print_max_width`: Character width at which a printed or
  formatted `<phinterval>` element is truncated for display, default:
  `90`. Set to `1` to always truncate formatted `<phinterval>` vectors.

## Examples

``` r
monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
friday <- interval(as.Date("2025-11-14"), as.Date("2025-11-15"))

# Get the default setting
getOption("phinterval.print_max_width")
#> [1] 90
phinterval(c(monday, friday))
#> <phinterval<UTC>[1]>
#> [1] {2025-11-10--2025-11-11, 2025-11-14--2025-11-15}

# Change the setting for the session duration
opts <- options(phinterval.print_max_width = 25)
phinterval(c(monday, friday))
#> <phinterval<UTC>[1]>
#> [1] <phint[2]>

# Reset to the previous settings
options(opts)
```
