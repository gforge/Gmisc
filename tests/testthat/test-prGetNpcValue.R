library(testthat)

test_that("prGetNpcValue returns npc numeric for unit and numeric and NA for invalid", {
  # simple npc unit
  expect_equal(prGetNpcValue(unit(0.3, "npc"), "x"), 0.3)

  # numeric value should be accepted
  expect_equal(prGetNpcValue(0.25, "x"), 0.25)

  # string that cannot be coerced -> NA
  expect_true(is.na(prGetNpcValue("not-a-number", "x")))

  # y axis should use convertY (basic check with unit)
  expect_equal(prGetNpcValue(unit(0.4, "npc"), "y"), 0.4)
})
