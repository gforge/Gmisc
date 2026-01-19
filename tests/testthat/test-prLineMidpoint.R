library(testthat)

test_that("prLineMidpoint returns npc units and midpoint for a simple straight segment", {
  line <- list(
    x = unit.c(unit(0.1, "npc"), unit(0.9, "npc")),
    y = unit.c(unit(0.5, "npc"), unit(0.5, "npc"))
  )

  mp <- prLineMidpoint(line)
  expect_s3_class(mp$x, "unit")
  expect_s3_class(mp$y, "unit")

  expect_equal(prGetNpcValue(mp$x, "x"), 0.5, tolerance = 1e-6)
  expect_equal(prGetNpcValue(mp$y, "y"), 0.5, tolerance = 1e-6)
})
