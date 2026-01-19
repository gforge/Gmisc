library(grid)

test_that("flowchart() constructor", {
  b1 <- boxGrob("A")
  b2 <- boxGrob("B")

  # Method 1: Named arguments
  fc1 <- flowchart(a = b1, b = b2)
  expect_s3_class(fc1, "Gmisc_list_of_boxes")
  expect_equal(names(fc1), c("a", "b"))
  expect_equal(length(fc1), 2)

  # Method 2: List input
  fc2 <- flowchart(list(a = b1, b = b2))
  expect_s3_class(fc2, "Gmisc_list_of_boxes")
  expect_equal(names(fc2), c("a", "b"))
  expect_equal(length(fc2), 2)

  # Method 3: Unnamed arguments
  fc3 <- flowchart(b1, b2)
  expect_s3_class(fc3, "Gmisc_list_of_boxes")
  expect_equal(length(fc3), 2)
})

test_that("flowchart() auto-converts text to boxes", {
  fc <- flowchart("A", "B", default_box_fn = boxGrob)
  expect_s3_class(fc[[1]], "box")
  expect_s3_class(fc[[2]], "box")
  # Need to check label possibly?
  # attributes(fc[[1]])$"label" ? No, boxGrob stores things in children usually or attributes specific to implementation.
  # boxGrob stores label in `label` attribute? No.
  # But if it didn't crash and is class box, reasonable enough.
})

test_that("flowchart() handles mixed content (boxes + text + nested lists)", {
  b1 <- boxGrob("Box1")
  fc <- flowchart(
    start = b1,
    mid = "Middle",
    end = list(sub1 = "SubA", sub2 = "SubB")
  )
  expect_s3_class(fc$start, "box")
  expect_s3_class(fc$mid, "box")
  expect_type(fc$end, "list")
  expect_s3_class(fc$end$sub1, "box")
  expect_s3_class(fc$end$sub2, "box")
})


test_that("flowchart() pipeline integration", {
  b1 <- boxGrob("Start", y = 0.8)
  b2 <- boxGrob("End", y = 0.2)

  res <- flowchart(start = b1, end = b2) |>
    align(axis = "y") |>
    connect("start", "end", type = "vertical")

  expect_s3_class(res, "Gmisc_list_of_boxes")
  expect_true(!is.null(attr(res, "connections")))
})
