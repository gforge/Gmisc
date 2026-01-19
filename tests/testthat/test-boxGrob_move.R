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


test_that(".subelement moves a single element in a list (shallow)", {
  b1 <- boxGrob("b1", x = .1, y = .1)
  b2 <- boxGrob("b2", x = .2, y = .2)
  L <- list(b1 = b1, b2 = b2)
  L2 <- moveBox(L, x = .1, subelement = "b1", space = "relative")
  expect_s3_class(L2, "Gmisc_list_of_boxes")
  expect_equal(names(L2), names(L))
  expect_false(identical(attr(L2$b1, "viewport_data")$x, attr(L$b1, "viewport_data")$x))
  expect_equal(attr(L2$b2, "viewport_data")$x, attr(L$b2, "viewport_data")$x)
})


test_that(".subelement supports nested path", {
  b <- boxGrob("b", x = .5, y = .5)
  L <- list(group = list(sub = b))
  L2 <- moveBox(L, y = -0.2, subelement = c("group", "sub"), space = "relative")
  expect_false(identical(attr(L2$group$sub, "viewport_data")$y, attr(L$group$sub, "viewport_data")$y))
})


test_that(".subelement supports numeric index", {
  b1 <- boxGrob("b1", x = .1, y = .1)
  b2 <- boxGrob("b2", x = .2, y = .2)
  L <- list(b1, b2)
  L2 <- moveBox(L, x = .1, subelement = 1, space = "relative")
  expect_false(identical(attr(L2[[1]], "viewport_data")$x, attr(L[[1]], "viewport_data")$x))
  expect_equal(attr(L2[[2]], "viewport_data")$x, attr(L[[2]], "viewport_data")$x)
})
