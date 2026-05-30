library(testthat)

test_that("prExtendClass avoids duplicates", {
  obj <- list(a = 1)
  class(obj) <- c("my_class", "list")

  # First extension
  obj2 <- prExtendClass(obj, "my_class")
  expect_equal(class(obj2), c("my_class", "list"))

  # Second extension with new class
  obj3 <- prExtendClass(obj2, "new_class")
  expect_equal(class(obj3), c("new_class", "my_class", "list"))

  # Re-adding existing class
  obj4 <- prExtendClass(obj3, "my_class")
  expect_equal(class(obj4), c("my_class", "new_class", "list"))

  # But is "my_class" now first?
  # unique(c("my_class", "new_class", "my_class", "list")) -> "my_class", "new_class", "list"
  # Yes, unique keeps the first occurrence.
})

test_that("prExtendClass requires single string", {
  expect_error(prExtendClass(list(), c("A", "B")))
})

test_that("Reproduce Gmisc_list_of_boxes duplication scenario", {
  # Simulate how spread/align might chain
  # spreadVertical returns Gmisc_list_of_boxes
  # alignVertical checks and re-adds it?

  l <- list(dummy = 1)
  # Mimic spreadVertical
  l <- prExtendClass(l, "Gmisc_list_of_boxes")
  expect_equal(class(l), c("Gmisc_list_of_boxes", "list"))

  # Mimic alignVertical on the result
  l <- prExtendClass(l, "Gmisc_list_of_boxes")
  expect_equal(class(l), c("Gmisc_list_of_boxes", "list"))

  # What if we had duplicates initially?
  class(l) <- c("Gmisc_list_of_boxes", "Gmisc_list_of_boxes", "list")
  l <- prExtendClass(l, "Gmisc_list_of_boxes")
  expect_equal(class(l), c("Gmisc_list_of_boxes", "list"))
})
