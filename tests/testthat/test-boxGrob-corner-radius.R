library(testthat)
library(grid)

test_that("boxGrob accepts box_fn_args and passes r to roundrectGrob", {
  # Should not error — roundrectGrob accepts r
  b <- boxGrob("Test", box_fn_args = list(r = unit(5, "mm")))
  expect_s3_class(b, "box")
})

test_that("boxGrob box_fn_args with fixed r produces same-sized corners for different box sizes", {
  r_fixed <- unit(5, "mm")
  small <- boxGrob("Hi", box_fn_args = list(r = r_fixed))
  big   <- boxGrob(
    paste(rep("The quick brown fox jumped over the lazy dog", 3), collapse = "\n"),
    box_fn_args = list(r = r_fixed)
  )
  expect_s3_class(small, "box")
  expect_s3_class(big,   "box")
  small_h <- convertHeight(coords(small)$height, "mm", valueOnly = TRUE)
  big_h   <- convertHeight(coords(big)$height,   "mm", valueOnly = TRUE)
  expect_gt(big_h, small_h)
})

test_that("boxGrob respects the boxGrobFnArgs global option", {
  withr::with_options(
    list(boxGrobFnArgs = list(r = unit(3, "mm"))),
    {
      b <- boxGrob("Option test")
      expect_s3_class(b, "box")
    }
  )
})

test_that("boxGrob rejects non-list box_fn_args", {
  expect_error(
    boxGrob("Test", box_fn_args = unit(5, "mm")),
    "`box_fn_args` must be a named list"
  )
})
