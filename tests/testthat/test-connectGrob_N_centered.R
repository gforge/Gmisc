library(testthat)

test_that("N connector uses straight center branch for one-to-many", {
  start <- boxGrob("Start")
  ends <- list(boxGrob("L"), boxGrob("C"), boxGrob("R"))
  # spread ends symmetric around center
  ends <- spreadHorizontal(ends, .from = unit(0.1, "npc"), .to = unit(0.9, "npc"), .type = "center")

  con <- connectGrob(start, ends, type = "N")
  # one of the returned grobs should be vertical (x coords equal)
  is_vertical <- vapply(con, function(g) {
    xvals <- convertX(attr(g, "line")$x, "npc", valueOnly = TRUE)
    max(xvals) - min(xvals) < 1e-6
  }, logical(1))
  expect_true(any(is_vertical))
})


test_that("N connector uses straight center branch for many-to-one", {
  ends <- list(boxGrob("L"), boxGrob("C"), boxGrob("R"))
  ends <- spreadHorizontal(ends, .from = unit(0.1, "npc"), .to = unit(0.9, "npc"), .type = "center")
  target <- boxGrob("Target")

  con <- connectGrob(ends, target, type = "N")
  # last length(ends) elements are trunks in our implementation
  trunks <- tail(con, length(ends))
  is_vertical <- vapply(trunks, function(g) {
    xvals <- convertX(attr(g, "line")$x, "npc", valueOnly = TRUE)
    max(xvals) - min(xvals) < 1e-6
  }, logical(1))
  expect_true(any(is_vertical))
})
