library(testthat)

test_that("Check that multi-line is higher than single line box", {
  b1 <- boxPropGrob(label = "Label",
                    label_left = "Left",
                    label_right = "Right",
                    prop = 0.5)
  b2 <- boxPropGrob(label = "Label", 
                    label_left = "Left1\nLeft2",
                    label_right = "Right", 
                    prop = 0.5)

  expect_gt(grobHeight(b2) %>% prCnvrtY,
            grobHeight(b1) %>% prCnvrtY)

  b3 <- boxPropGrob(label = "Label1\nlabel2", 
                    label_left = "Left",
                    label_right = "Right",
                    prop = 0.5)
  expect_gt(grobHeight(b3) %>% prCnvrtY,
            grobHeight(b1) %>% prCnvrtY)

  b4 <- boxPropGrob(label = "Label",
                    label_left = "Left",
                    label_right = "Right1\nRight2",
                    prop = 0.5)
  expect_gt(grobHeight(b4) %>% prCnvrtY,
            grobHeight(b1) %>% prCnvrtY)
})

test_that("Check that not providing label reduces the height of the box", {
  b1 <- boxPropGrob(label = "Label",
                    label_left = "Left", "Right",
                    prop = 0.5)
  b2 <- boxPropGrob(label_left = "Left",
                    label_right = "Right",
                    prop = 0.5)
  
  expect_lt(grobHeight(b2) %>% prCnvrtY,
            grobHeight(b1) %>% prCnvrtY)
  
  b3 <- boxPropGrob(label = "Label", prop = 0.5)
  expect_lt(grobHeight(b3) %>% prCnvrtY,
            grobHeight(b1) %>% prCnvrtY)
})

test_that("Label with two lines increases the height", {
  b1 <- boxPropGrob(label = "A",
                    label_left = "A",
                    label_right = "A", prop = .5) %>% 
    grobHeight %>% 
    convertY(unitTo = "mm", valueOnly = TRUE)
  b2 <- boxPropGrob(label = "A\nA", 
                    label_left = "AA",
                    label_right = "AA", prop = .5) %>% 
    grobHeight %>% 
    convertY(unitTo = "mm", valueOnly = TRUE)
  expect_gt(b2, b1, label = "Prop box height should be defined from the width of the text")
})
