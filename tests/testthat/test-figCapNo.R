context("Figure caption numbers")

test_that("Test figure caption basic functionality",{
  options(fig_caption_no = NULL)
  expect_error(figCapNoLast())
  expect_equivalent(figCapNoNext(), 1)
  expect_match(figCapNo("a", roman = FALSE, sprintf_str = "%s: %s"),
               "1+: a")
  
  expect_match(figCapNo("a", roman = FALSE, sprintf_str = "%s: %s"),
               "2: a")
  
  expect_match(figCapNo("a", roman = TRUE, sprintf_str = "%s: %s"),
               "III: a")

  options(fig_caption_no = NULL)
  expect_match(figCapNo("a", roman = TRUE, sprintf_str = "%s: %s"),
               "I: a")
  
  expect_error(figCapNo())
})

test_that("Test figure caption options",{
  options(fig_caption_no = 2)
  expect_equivalent(figCapNoLast(), 2)
  expect_equivalent(figCapNoNext(), 3)
  
  options(fig_caption_no_roman = TRUE)
  expect_equivalent(figCapNoLast(), "II")
  expect_equivalent(figCapNoNext(), "III")

  options(fig_caption_no = 10)
  expect_equivalent(figCapNo("test"), "Fig. XI: test")
  
  options(fig_caption_no_roman = FALSE)
  expect_equivalent(figCapNo("test"), "Fig. 12: test")
})
  