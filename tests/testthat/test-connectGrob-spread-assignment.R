library(testthat)

test_that("spreadHorizontal returns updated boxes without mutating originals", {
  b1 <- boxGrob("Visit 1", x = 0.1, y = 0.7)
  b2 <- boxGrob("Visit 2", x = 0.2, y = 0.7)
  b3 <- boxGrob("Visit 3", x = 0.9, y = 0.7)

  b2_x_before <- convertX(coords(b2)$x, "npc", valueOnly = TRUE)

  spread_res <- spreadHorizontal(
    b1,
    b2,
    b3,
    from = unit(0.05, "npc"),
    to = unit(0.95, "npc"),
    type = "between"
  )

  b2_x_after <- convertX(coords(b2)$x, "npc", valueOnly = TRUE)
  b2_x_spread <- convertX(coords(spread_res[[2]])$x, "npc", valueOnly = TRUE)

  expect_equal(b2_x_after, b2_x_before)
  expect_true(abs(b2_x_spread - b2_x_before) > 1e-6)
})


test_that("connectGrob uses post-spread coordinates when updated objects are used", {
  visit1 <- boxGrob("Visit 1", x = 0.05, y = 0.45)
  visit5 <- boxGrob("Visit 5", x = 0.95, y = 0.45)
  visit2 <- boxGrob("Visit 2", x = 0.20, y = 0.45)
  visit3 <- boxGrob("Visit 3", x = 0.50, y = 0.45)
  visit4 <- boxGrob("Visit 4", x = 0.70, y = 0.45)

  spread_visits <- alignVertical(
    reference = visit1,
    visit2, visit3, visit4,
    position = "top"
  ) |>
    spreadHorizontal(
      from = visit1,
      to = visit5,
      type = "between"
    )

  con_old <- connectGrob(visit1, visit2, type = "horizontal")
  con_new <- connectGrob(visit1, spread_visits[[1]], type = "horizontal")

  x_old_end <- convertX(attr(con_old, "line")$x[2], "npc", valueOnly = TRUE)
  x_new_end <- convertX(attr(con_new, "line")$x[2], "npc", valueOnly = TRUE)

  x_old_expected <- convertX(coords(visit2)$left, "npc", valueOnly = TRUE)
  x_new_expected <- convertX(coords(spread_visits[[1]])$left, "npc", valueOnly = TRUE)

  expect_equal(x_old_end, x_old_expected, tolerance = 1e-6)
  expect_equal(x_new_end, x_new_expected, tolerance = 1e-6)
  expect_true(abs(x_old_end - x_new_end) > 1e-6)
})