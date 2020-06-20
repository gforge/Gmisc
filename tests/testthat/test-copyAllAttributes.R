library("testthat")
context("copyAllNewAttributes")

test_that("Basic copy", {
  test_from <- 1
  attr(test_from, "t") <- 2

  test_2 <- 2
  test_2 <- copyAllNewAttributes(
    from = test_from,
    to = test_2
  )

  expect_equal(attributes(test_2), attributes(test_from))
})


test_that("A little more advanced copy", {
  test_from <- matrix(1, ncol = 2, nrow = 2)
  attr(test_from, "t") <- 2

  test_2 <- matrix(2, ncol = 3, nrow = 3)
  test_2 <- copyAllNewAttributes(
    from = test_from,
    to = test_2
  )

  expect_equal(attributes(test_2)$t, attributes(test_from)$t)
  expect_false(all(attributes(test_2)$dim == attributes(test_from)$dim))
})

test_that("An advanced copy", {
  test_from <- matrix(1, ncol = 2, nrow = 2)
  attr(test_from, "t") <- 2

  test_2 <- matrix(2, ncol = 3, nrow = 3)
  attr(test_2, "t") <- 1
  test_2 <- copyAllNewAttributes(
    from = test_from,
    to = test_2
  )
  expect_false(attributes(test_2)$t == attributes(test_from)$t)

  test_2 <- copyAllNewAttributes(
    from = test_from,
    to = test_2,
    attr2force = "t"
  )
  expect_true(attributes(test_2)$t == attributes(test_from)$t)
})