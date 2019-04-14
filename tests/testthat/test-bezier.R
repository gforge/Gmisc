library(testthat)

context("Bézier functions")

test_that("Basic funcitonality", {
  df <- data.frame(x = c(1, 2, 4, 5),
                   y = c(5, 2, 3, 1))
  arrow <-
    getBezierAdj4Arrw(x = df$x,
                      y = df$y,
                      length_out = 10,
                      arrow_length = .1)
  expect_equal(length(arrow$x), 10)
  expect_equal(length(arrow$y), 10)
  expect_false(all(attr(arrow, "spline_ctrl")$x == df$x),
               info = "The spline should be adjusted according to the arrow length")
  expect_false(all(attr(arrow, "spline_ctrl")$y == df$y),
               info = "The spline should be adjusted according to the arrow length")
  arrow <-
    getBezierAdj4Arrw(x = df$x,
                      y = df$y,
                      length_out = 10,
                      arrow_length = 0)
  expect_equal(length(arrow$x), 10)
  expect_equal(length(arrow$y), 10)
  expect_true(all(attr(arrow, "spline_ctrl")$x == df$x),
              info = "The spline should be adjusted according to the arrow length")
  expect_true(all(attr(arrow, "spline_ctrl")$y == df$y),
              info = "The spline should be adjusted according to the arrow length")

})

test_that("Check helpers", {
  expect_error(validateAndConvertVectorInputs(1:3, unit(1:3, "npc"), x_origo = 0, y_origo = 0))
  expect_error(validateAndConvertVectorInputs(1:3, 1:3, x_origo = 1))
  expect_error(validateAndConvertVectorInputs(1:3, 1:3, x_origo = 1, y_origo = unit(1, "npc")))

})

test_that("Check vertical lines",{
  structure(list(x = c(68.0508333333333, 68.0508333333333, 68.0508333333334,
                       68.0508333333333, 68.0508333333333, 68.0508333333333,
                       68.0508333333334, 68.0508333333333, 68.0508333333333,
                       68.0508333333333),
                 y = c(13.7230555555556, 13.7569206164501, 13.8576907109782,
                       14.0242092679277, 14.2554021625002, 14.5502479980992,
                       14.9077508947427, 15.326915529896, 15.8067242521733,
                       16.346116150104)),
            .Names = c("x", "y")) -> test

  out <- calculateLinesAndArrow(test$x, test$y, offset = 5)
  expect_true(all(abs(out$right$x - out$right$x[1]) <= .Machine$double.eps*10^3))
  expect_true(all(abs(out$left$x - out$left$x[1]) <= .Machine$double.eps*10^3))
  expect_true(10 - (out$right$x[1] - out$left$x[1]) <= .Machine$double.eps)


  out <- calculateLinesAndArrow(test$x, rev(test$y), offset = 3)
  # There is some kind of floating error that causes an offset in the
  # value 10^-14 for a few values
  expect_true(all(abs(out$right$x - out$right$x[1]) <= .Machine$double.eps*10^3))
  expect_true(all(abs(out$left$x - out$left$x[1]) <= .Machine$double.eps*10^3))
  expect_true(6 + (out$right$x[1] - out$left$x[1]) <= .Machine$double.eps)
})


test_that("Horizontal_lines",{
  structure(list(x = c(204.1525, 204.0243, 203.6438, 203.0167,
                       202.1486, 201.0447, 199.7103, 198.1507, 196.3712, 194.3776, 192.1755,
                       189.7712, 187.1712, 184.3824, 181.4123, 178.2687, 174.9601, 171.4954,
                       167.8839, 164.1358, 160.2614, 156.2717, 152.1782, 147.9927, 143.7274,
                       139.3949, 135.008, 130.5799, 126.1237, 121.6528, 117.1804, 112.7201,
                       108.2848, 103.8878, 99.5418, 95.2593, 91.0526, 86.9334, 82.9131,
                       79.0026, 75.2124, 71.5522, 68.0316, 64.6594, 61.4438, 58.3927,
                       55.5135, 52.813, 50.2976, 47.9734, 45.8461, 43.9211, 42.2038,
                       40.6991, 39.4121, 38.348, 37.5422),
                 y = c(96.0614, 96.0614, 96.0614,
                       96.0614, 96.0614, 96.0614, 96.0614, 96.0614, 96.0614, 96.0614,
                       96.0614, 96.0614, 96.0614, 96.0614, 96.0614, 96.0614, 96.0614,
                       96.0614, 96.0614, 96.0614, 96.0614, 96.0614, 96.0614, 96.0614,
                       96.0614, 96.0614, 96.0614, 96.0614, 96.0614, 96.0614, 96.0614,
                       96.0614, 96.0614, 96.0614, 96.0614, 96.0614, 96.0614, 96.0614,
                       96.0614, 96.0614, 96.0614, 96.0614, 96.0614, 96.0614, 96.0614,
                       96.0614, 96.0614, 96.0614, 96.0614, 96.0614, 96.0614, 96.0614,
                       96.0614, 96.0614, 96.0614, 96.0614, 96.0614)),
            .Names = c("x", "y")) -> test

  out <- calculateLinesAndArrow(test$x, test$y, offset = 3)
  # There is some kind of floating error that causes an offset in the
  # value 10^-14 for a few values
  expect_true(all(abs(out$right$y - out$right$y[1]) <= .Machine$double.eps*10^3))
  expect_true(all(abs(out$left$y - out$left$y[1]) <= .Machine$double.eps*10^3))
  expect_true(6 - (out$right$y[1] - out$left$y[1]) <= .Machine$double.eps)

  out <- calculateLinesAndArrow(rev(test$x), test$y, offset = 3)
  # There is some kind of floating error that causes an offset in the
  # value 10^-14 for a few values
  expect_true(all(abs(out$right$y - out$right$y[1]) <= .Machine$double.eps*10^3))
  expect_true(all(abs(out$left$y - out$left$y[1]) <= .Machine$double.eps*10^3))
  expect_true(6 + (out$right$y[1] - out$left$y[1]) <= .Machine$double.eps)
})


