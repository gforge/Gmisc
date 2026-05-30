library(testthat)
library(grid)

test_that("boxGrob respects global boxGrobTxtPadding option", {
  old <- options(boxGrobTxtPadding = unit(1, "mm"))
  on.exit(options(old), add = TRUE)

  b_small <- boxGrob("Padding test", txt_gp = gpar(cex = 1))
  w_small <- convertWidth(coords(b_small)$width, "mm", valueOnly = TRUE)

  options(boxGrobTxtPadding = unit(8, "mm"))
  b_large <- boxGrob("Padding test", txt_gp = gpar(cex = 1))
  w_large <- convertWidth(coords(b_large)$width, "mm", valueOnly = TRUE)

  expect_gt(w_large, w_small)
})

test_that("boxGrob accepts numeric txt_padding in millimeters", {
  b <- boxGrob("Padding test", txt_padding = 2)
  expect_s3_class(b, "box")
})
