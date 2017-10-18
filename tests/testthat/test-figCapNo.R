context("Figure caption numbers")

test_that("Test figure caption basic functionality",{
  options(fig_caption_no = NULL)
  expect_error(figCapNoLast())
  expect_equivalent(figCapNoNext(), 1)
  no <- figCapNo("a", roman = FALSE, sprintf_str = "%s: %s")
  expect_match(no, "1+: a")

  no <- figCapNo("a", roman = FALSE, sprintf_str = "%s: %s")
  expect_match(no, "2: a")
  
  no <- figCapNo("a", roman = TRUE, sprintf_str = "%s: %s")
  expect_match(no, "III: a")

  options(fig_caption_no = NULL)
  expect_match(figCapNo("a", roman = TRUE, sprintf_str = "%s: %s"),
               "I: a")

  expect_error(figCapNo())

  options(fig_caption_no = FALSE)
  expect_error(figCapNoNext())

  expect_equal(figCapNo("test"), "test")

  options(fig_caption_no = TRUE)
  expect_equal(figCapNoNext(), 1)
  no <- figCapNo("a", roman = FALSE, sprintf_str = "%s: %s")
  expect_equal(no, "1: a")
})

test_that("Test figure caption options",{
  options(fig_caption_no = 2)
  no <- figCapNoLast()
  expect_equivalent(no, 2)
  no <- figCapNoNext()
  expect_equivalent(no, 3)

  options(fig_caption_no_roman = TRUE)
  expect_equivalent(figCapNoLast(), "II")
  expect_equivalent(figCapNoNext(), "III")

  options(fig_caption_no = 10)
  expect_equivalent(figCapNo("test"), "Fig. XI: test")

  options(fig_caption_no_roman = FALSE)
  expect_equivalent(figCapNo("test"), "Fig. 12: test")
})
