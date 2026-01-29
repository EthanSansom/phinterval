#nocov start

phint_binary_dispatch <- function(
    x,
    y,
    x_type,
    y_type,
    funs_cpp,
    ...,
    x_arg = caller_arg(x),
    y_arg = caller_arg(y),
    call = caller_env()
) {
  check_recycleable(x, y, x_arg = x_arg, y_arg = y_arg, call = call)

  switch(
    paste(x_type, y_type, sep = "_"),
    phint_phint = funs_cpp$phint_phint(
      x_size = field(x, "size"),
      x_starts = field(x, "starts"),
      x_ends = field(x, "ends"),
      y_size = field(y, "size"),
      y_starts = field(y, "starts"),
      y_ends = field(y, "ends"),
      ...
    ),
    phint_intvl = funs_cpp$phint_intvl(
      x_size = field(x, "size"),
      x_starts = field(x, "starts"),
      x_ends = field(x, "ends"),
      y_starts = lubridate::int_start(y),
      y_spans = lubridate::int_length(y),
      ...
    ),
    intvl_phint = funs_cpp$intvl_phint(
      x_starts = lubridate::int_start(x),
      x_spans = lubridate::int_length(x),
      y_size = field(y, "size"),
      y_starts = field(y, "starts"),
      y_ends = field(y, "ends"),
      ...
    ),
    intvl_intvl = funs_cpp$intvl_intvl(
      x_starts = lubridate::int_start(x),
      x_spans = lubridate::int_length(x),
      y_starts = lubridate::int_start(y),
      y_spans = lubridate::int_length(y),
      ...
    ),
    # `point_*` combinations used by `phint_within()`
    point_phint = funs_cpp$point_phint(
      x_points = as.POSIXct(x),
      y_size = field(y, "size"),
      y_starts = field(y, "starts"),
      y_ends = field(y, "ends"),
      ...
    ),
    point_intvl = funs_cpp$point_intvl(
      x_points = as.POSIXct(x),
      y_starts = lubridate::int_start(y),
      y_spans = lubridate::int_length(y),
      ...
    ),
    abort(
      sprintf("Unexpected type combination: `%s` and `%s`.", x_type, y_type),
      .internal = TRUE
    )
  )
}

phint_unary_dispatch <- function(
    x,
    x_type,
    funs_cpp,
    ...,
    arg = caller_arg(x),
    call = caller_env()
) {
  switch(
    x_type,
    phint = funs_cpp$phint(
      size = field(x, "size"),
      starts = field(x, "starts"),
      ends = field(x, "ends"),
      ...
    ),
    intvl = funs_cpp$intvl(
      starts = lubridate::int_start(x),
      spans = lubridate::int_length(x),
      ...
    ),
    point = funs_cpp$point(
      points = as.POSIXct(x),
      ...
    ),
    abort(
      sprintf("Unexpected type: `%s`.", x_type),
      .internal = TRUE
    )
  )
}

#nocov end
