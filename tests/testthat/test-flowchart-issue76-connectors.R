library(testthat)
library(grid)

make_issue76_fc <- function() {
  flowchart(
    rando = "Randomised N = 100",
    groups = list("Group1\nn = 50", "Group2\nn = 50"),
    ex1 = list("Excluded\nn = 1", "Excluded\nn = 2"),
    groups1 = list("bla\nn = 49", "bla\nn = 48"),
    ex2 = list("Excluded\nn = 1", "Excluded\nn = 2"),
    groups2 = list("blas\nn = 49", "blas\nn = 48"),
    ex3 = list("Excluded\nn = 1", "Excluded\nn = 2"),
    groups3 = list("Analysed\nn = 49", "Analysed\nn = 48")
  ) |>
    spread(axis = "y", margin = unit(0.02, "npc")) |>
    spread(subelement = stringr::regex("^groups"), axis = "x", margin = unit(.3, "npc")) |>
    spread(subelement = stringr::regex("^ex"), axis = "x", margin = unit(.1, "npc"))
}

test_that("spread() preserves regex selectors through the S3 wrapper", {
  fc <- make_issue76_fc()

  left_ex <- convertX(coords(fc$ex1[[1]])$x, "npc", valueOnly = TRUE)
  right_ex <- convertX(coords(fc$ex1[[2]])$x, "npc", valueOnly = TRUE)

  expect_lt(left_ex, 0.2)
  expect_gt(right_ex, 0.8)
})

test_that("connect() accepts list selectors for grouped side return arrows", {
  fc <- make_issue76_fc() |>
    connect(
      from = list("ex1", "ex2", "ex3"),
      to = "groups3",
      type = "side",
      lty_gp = gpar(lty = 2)
    )

  con <- tail(attr(fc, "connections"), 1)[[1]]
  expect_s3_class(con, "connect_boxes_list")
  expect_equal(length(con), 2)
  expect_gte(length(con[[1]]$children), 5)
})

test_that("connect() accepts regex selectors for arm-wise fan-in", {
  fc <- make_issue76_fc() |>
    connect(
      from = stringr::regex("^ex"),
      to = "groups3",
      type = "side",
      lty_gp = gpar(lty = 2),
      side = "right",
      side_fan_in_offset = unit(5, "mm")
    )

  con <- tail(attr(fc, "connections"), 1)[[1]]
  expect_s3_class(con, "connect_boxes_list")
  expect_equal(length(con), 2)

  target_left <- convertX(coords(fc$groups3[[1]])$left, "npc", valueOnly = TRUE)
  first_line <- attr(con[[1]], "line")
  first_x <- convertX(first_line$x, "npc", valueOnly = TRUE)
  first_y <- convertY(first_line$y, "npc", valueOnly = TRUE)

  expect_equal(tail(first_x, 1), target_left, tolerance = 1e-6)
  expect_equal(first_y[length(first_y) - 1], tail(first_y, 1), tolerance = 1e-6)
  expect_gt(first_x[1], convertX(coords(make_issue76_fc()$ex1[[1]])$right, "npc", valueOnly = TRUE))
})

test_that("side connectors can enter a requested destination side", {
  start <- boxGrob("Excluded", x = .1, y = .7)
  end <- boxGrob("Analysed", x = .4, y = .2)

  con <- connectGrob(
    start,
    end,
    type = "side",
    side = "left",
    end_side = "left"
  )

  line <- attr(con, "line")
  xs <- convertX(line$x, "npc", valueOnly = TRUE)
  ys <- convertY(line$y, "npc", valueOnly = TRUE)

  expect_equal(tail(xs, 1), convertX(coords(end)$left, "npc", valueOnly = TRUE), tolerance = 1e-6)
  expect_equal(ys[length(ys) - 1], tail(ys, 1), tolerance = 1e-6)
})

test_that("move() can use positions resolved from another flowchart box", {
  offset <- unit(10, "mm")
  fc <- flowchart(
    groups = list("Group 1", "Group 2"),
    ex = list("Excluded 1", "Excluded 2")
  ) |>
    spread(axis = "y") |>
    spread(subelement = "groups", axis = "x", from = .25, to = .75, type = "center") |>
    spread(subelement = "ex", axis = "x", from = .25, to = .75, type = "center") |>
    move(
      subelement = c("ex", 1),
      x = position(c("groups", 1), position = "center", type = "x") + offset
    )

  expect_equal(
    convertX(coords(fc$ex[[1]])$x, "mm", valueOnly = TRUE),
    convertX(coords(fc$groups[[1]])$x + offset, "mm", valueOnly = TRUE),
    tolerance = 1e-6
  )
})

test_that("position() dispatches for boxes and coords", {
  box <- boxGrob("A", x = .4, y = .6)

  expect_equal(
    convertX(position(box, position = "center", type = "x"), "npc", valueOnly = TRUE),
    .4,
    tolerance = 1e-6
  )
  expect_equal(
    convertY(position(coords(box), position = "center", type = "y"), "npc", valueOnly = TRUE),
    .6,
    tolerance = 1e-6
  )
  expect_s3_class(position(c("groups", 1), position = "center", type = "x"), "unit")
})

test_that("position() can resolve grouped list centers", {
  fc <- flowchart(
    rando = "Randomised",
    groups = list("Group 1", "Group 2")
  ) |>
    spread(axis = "y") |>
    spread(subelement = "groups", axis = "x", from = .1, to = .8, type = "center") |>
    move(subelement = "rando", x = position("groups", position = "center", type = "x"))

  expect_equal(
    convertX(coords(fc$rando)$x, "npc", valueOnly = TRUE),
    mean(vapply(fc$groups, function(box) {
      convertX(coords(box)$x, "npc", valueOnly = TRUE)
    }, numeric(1))),
    tolerance = 1e-6
  )
})

test_that("arm-wise fan-in errors on incompatible grouped target lengths", {
  fc <- make_issue76_fc()
  fc$groups3 <- fc$groups3[1]

  expect_error(
    connect(fc, from = stringr::regex("^ex"), to = "groups3", type = "side"),
    "same number of boxes"
  )
})
