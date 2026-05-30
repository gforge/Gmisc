library(testthat)

test_that("prGetNpcSize returns npc numeric for unit and numeric and NA for invalid", {
  # simple npc unit for width
  expect_equal(prGetNpcSize(unit(0.3, "npc"), "x"), 0.3)

  # numeric value should be accepted (interpreted as npc)
  expect_equal(prGetNpcSize(0.25, "x"), 0.25)

  # string that cannot be coerced -> NA
  expect_true(is.na(prGetNpcSize("not-a-number", "x")))

  # y axis should use convertHeight (basic check with unit)
  expect_equal(prGetNpcSize(unit(0.4, "npc"), "y"), 0.4)
})
