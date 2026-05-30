library(testthat)
library(grid)

test_that("equalizeWidths() equalizes selected nested boxes and preserves centers", {
  fc <- flowchart(
    groups = list(
      "Short",
      "A much longer label"
    ),
    groups2 = list(
      "Tiny",
      "Another longer label"
    )
  ) |>
    spread(axis = "y", margin = unit(0.02, "npc")) |>
    spread(subelement = "groups", axis = "x", margin = unit(.05, "npc")) |>
    spread(subelement = "groups2", axis = "x", margin = unit(.05, "npc"))

  target_paths <- list(c("groups", 1), c("groups2", 1), c("groups", 2), c("groups2", 2))

  x_before <- vapply(target_paths, function(p) {
    b <- Gmisc:::get_list_element_by_path(fc, p)
    convertX(coords(b)$x, "npc", valueOnly = TRUE)
  }, numeric(1))

  out <- equalizeWidths(fc, subelement = target_paths)

  widths_mm <- vapply(target_paths, function(p) {
    b <- Gmisc:::get_list_element_by_path(out, p)
    convertWidth(coords(b)$width, "mm", valueOnly = TRUE)
  }, numeric(1))

  x_after <- vapply(target_paths, function(p) {
    b <- Gmisc:::get_list_element_by_path(out, p)
    convertX(coords(b)$x, "npc", valueOnly = TRUE)
  }, numeric(1))

  expect_true(all(abs(widths_mm - widths_mm[1]) < 1e-6))
  expect_equal(x_after, x_before, tolerance = 1e-8)
})

test_that("equalizeWidths() supports selecting a list-of-boxes path", {
  fc <- flowchart(
    groups = list("A", "Longer group text")
  )

  out <- equalizeWidths(fc, subelement = "groups", width = unit(30, "mm"))

  widths_mm <- vapply(out$groups, function(b) {
    convertWidth(coords(b)$width, "mm", valueOnly = TRUE)
  }, numeric(1))

  expect_true(all(abs(widths_mm - 30) < 1e-6))
})
