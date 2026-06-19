library(testthat)
library(grid)

test_that("equalizeHeights() equalizes selected nested boxes and preserves centers", {
  fc <- flowchart(
    groups = list(
      "Short",
      "A much longer label\nwith two lines"
    ),
    groups2 = list(
      "Tiny",
      "Another longer\nlabel here"
    )
  ) |>
    spread(axis = "y", margin = unit(0.02, "npc")) |>
    spread(subelement = "groups",  axis = "x", margin = unit(.05, "npc")) |>
    spread(subelement = "groups2", axis = "x", margin = unit(.05, "npc"))

  target_paths <- list(c("groups", 1), c("groups2", 1), c("groups", 2), c("groups2", 2))

  y_before <- vapply(target_paths, function(p) {
    b <- Gmisc:::get_list_element_by_path(fc, p)
    convertY(coords(b)$y, "npc", valueOnly = TRUE)
  }, numeric(1))

  out <- equalizeHeights(fc, subelement = target_paths)

  heights_mm <- vapply(target_paths, function(p) {
    b <- Gmisc:::get_list_element_by_path(out, p)
    convertHeight(coords(b)$height, "mm", valueOnly = TRUE)
  }, numeric(1))

  y_after <- vapply(target_paths, function(p) {
    b <- Gmisc:::get_list_element_by_path(out, p)
    convertY(coords(b)$y, "npc", valueOnly = TRUE)
  }, numeric(1))

  # All heights should be equal (max of the group)
  expect_true(all(abs(heights_mm - heights_mm[1]) < 1e-6))
  # Centers should be preserved
  expect_equal(y_after, y_before, tolerance = 1e-8)
})

test_that("equalizeHeights() supports selecting a list-of-boxes path", {
  fc <- flowchart(
    groups = list("A", "Longer\nlabel\nwith\nthree lines")
  )

  out <- equalizeHeights(fc, subelement = "groups", height = unit(25, "mm"))

  heights_mm <- vapply(out$groups, function(b) {
    convertHeight(coords(b)$height, "mm", valueOnly = TRUE)
  }, numeric(1))

  expect_true(all(abs(heights_mm - 25) < 1e-6))
})

test_that("equalizeHeights() on a single box with explicit height works", {
  b <- boxGrob("Hello")
  b2 <- equalizeHeights(b, height = unit(30, "mm"))
  h <- convertHeight(coords(b2)$height, "mm", valueOnly = TRUE)
  expect_equal(h, 30, tolerance = 1e-6)
})

test_that("equalizeHeights() on a single box without height returns it unchanged", {
  b <- boxGrob("Hello")
  b2 <- equalizeHeights(b)
  expect_identical(b, b2)
})
