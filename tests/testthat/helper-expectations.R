expect_phint_equal <- function(object, expected, ...) {
  expect_equal(
    as_phinterval(object),
    as_phinterval(expected),
    label = rlang::caller_arg(object),
    expected.label = rlang::caller_arg(expected),
    ...
  )
}
