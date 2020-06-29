library(testthat)

test_that("Box move relative", {
  box1 <- boxGrob("A simple box", x = .5, y = .8)
  box2 <- moveBox(box1, x = -.2, y = -.1, space = "relative")
  cvt <- function(x, fn = convertX) convertX(x, unitTo = "mm", valueOnly = TRUE)
  expect_gt(cvt(attr(box1, "viewport_data")$x), cvt(attr(box2, "viewport_data")$x))
  expect_gt(cvt(attr(box1, "viewport_data")$y), cvt(attr(box2, "viewport_data")$y))

  box3 <- moveBox(box1, y = 0.1, space = "relative")
  expect_equal(cvt(attr(box1, "viewport_data")$x), cvt(attr(box3, "viewport_data")$x))
  expect_lt(cvt(attr(box1, "viewport_data")$y), cvt(attr(box3, "viewport_data")$y))
})

test_that("Box move absolute", {
  box1 <- boxGrob("A simple box", x = .5, y = .8)
  box2 <- moveBox(box1, x = .2, y = .1, space = "abs")
  cvt <- function(x, fn = convertX) convertX(x, unitTo = "mm", valueOnly = TRUE)
  expect_gt(cvt(attr(box1, "viewport_data")$x), cvt(attr(box2, "viewport_data")$x))
  expect_gt(cvt(attr(box1, "viewport_data")$y), cvt(attr(box2, "viewport_data")$y))
  
  box3 <- moveBox(box1, x = .3, space = "abs")
  expect_equal(cvt(attr(box1, "viewport_data")$y), cvt(attr(box3, "viewport_data")$y))
  expect_gt(cvt(attr(box1, "viewport_data")$x), cvt(attr(box3, "viewport_data")$x))
})

