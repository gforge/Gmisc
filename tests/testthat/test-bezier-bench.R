library(testthat)

context("bezier arrow performance")

test_that("arrows compute quickly", {
  skip_on_cran()
  skip_if_not_installed("bench")

  b <- bench::mark(
    simple = bezierArrowSmpl(x = c(.1, .3, .6, .9), y = c(.2, .2, .9, .9)),
    grad   = bezierArrowGradient(x = c(.1, .3, .6, .9), y = c(.2, .2, .9, .9)),
    iterations = 10,
    check = FALSE
  )

  # bench_time -> seconds (numeric)
  med_s <- as.numeric(b$median) / 1e9
  expect_true(all(med_s < 0.5))
})
