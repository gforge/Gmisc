library(grid)
library(testthat)

npc_x <- function(x) convertX(x, "npc", valueOnly = TRUE)
npc_y <- function(y) convertY(y, "npc", valueOnly = TRUE)

axis_box <- function(label, x, y, width = 0.1, height = 0.1) {
  boxGrob(
    label,
    x = unit(x, "npc"),
    y = unit(y, "npc"),
    width = unit(width, "npc"),
    height = unit(height, "npc")
  )
}

test_that("vertical_axis uses source bottom and target top when source is above target", {
  start <- axis_box("Start", x = 0.3, y = 0.7)
  end <- axis_box("End", x = 0.5, y = 0.2, width = 0.8)

  con <- connectGrob(start, end, type = "vertical_axis")
  line <- attr(con, "line")

  expect_equal(npc_y(line$y[1]), npc_y(coords(start)$bottom), tolerance = 1e-6)
  expect_equal(npc_y(line$y[2]), npc_y(coords(end)$top), tolerance = 1e-6)
})

test_that("vertical_axis uses source top and target bottom when source is below target", {
  start <- axis_box("Start", x = 0.3, y = 0.2)
  end <- axis_box("End", x = 0.5, y = 0.7, width = 0.8)

  con <- connectGrob(start, end, type = "vertical_axis")
  line <- attr(con, "line")

  expect_equal(npc_y(line$y[1]), npc_y(coords(start)$top), tolerance = 1e-6)
  expect_equal(npc_y(line$y[2]), npc_y(coords(end)$bottom), tolerance = 1e-6)
})

test_that("vertical_axis lands at source x inside target span", {
  start <- axis_box("Start", x = 0.3, y = 0.7)
  end <- axis_box("End", x = 0.5, y = 0.2, width = 0.8)

  con <- connectGrob(start, end, type = "vertical_axis")
  line <- attr(con, "line")

  expect_equal(npc_x(line$x[1]), npc_x(coords(start)$x), tolerance = 1e-6)
  expect_equal(npc_x(line$x[2]), npc_x(coords(start)$x), tolerance = 1e-6)
})

test_that("vertical_axis clamps target x to target left and right boundaries", {
  left_start <- axis_box("Left", x = 0.05, y = 0.7)
  right_start <- axis_box("Right", x = 0.95, y = 0.7)
  end <- axis_box("End", x = 0.5, y = 0.2, width = 0.6)

  left_con <- connectGrob(left_start, end, type = "vertical_axis")
  right_con <- connectGrob(right_start, end, type = "vertical_axis")

  expect_equal(npc_x(attr(left_con, "line")$x[2]), npc_x(coords(end)$left), tolerance = 1e-6)
  expect_equal(npc_x(attr(right_con, "line")$x[2]), npc_x(coords(end)$right), tolerance = 1e-6)
})

test_that("vertical_axis errors when boxes overlap vertically", {
  start <- axis_box("Start", x = 0.3, y = 0.5, height = 0.3)
  end <- axis_box("End", x = 0.6, y = 0.55, height = 0.3)

  expect_error(
    connectGrob(start, end, type = "vertical_axis"),
    "vertically separated"
  )
})

test_that("vertical behavior remains unchanged", {
  start <- axis_box("Start", x = 0.3, y = 0.7)
  end <- axis_box("End", x = 0.6, y = 0.2)

  con <- connectGrob(start, end, type = "vertical")
  line <- attr(con, "line")

  expect_equal(npc_x(line$x[1]), npc_x(coords(start)$x), tolerance = 1e-6)
  expect_equal(npc_x(line$x[2]), npc_x(coords(end)$x), tolerance = 1e-6)
})

test_that("horizontal_axis uses source right and target left when source is left of target", {
  start <- axis_box("Start", x = 0.2, y = 0.4)
  end <- axis_box("End", x = 0.7, y = 0.5, height = 0.6)

  con <- connectGrob(start, end, type = "horizontal_axis")
  line <- attr(con, "line")

  expect_equal(npc_x(line$x[1]), npc_x(coords(start)$right), tolerance = 1e-6)
  expect_equal(npc_x(line$x[2]), npc_x(coords(end)$left), tolerance = 1e-6)
})

test_that("horizontal_axis uses source left and target right when source is right of target", {
  start <- axis_box("Start", x = 0.8, y = 0.4)
  end <- axis_box("End", x = 0.3, y = 0.5, height = 0.6)

  con <- connectGrob(start, end, type = "horizontal_axis")
  line <- attr(con, "line")

  expect_equal(npc_x(line$x[1]), npc_x(coords(start)$left), tolerance = 1e-6)
  expect_equal(npc_x(line$x[2]), npc_x(coords(end)$right), tolerance = 1e-6)
})

test_that("horizontal_axis lands at source y inside target span", {
  start <- axis_box("Start", x = 0.2, y = 0.4)
  end <- axis_box("End", x = 0.7, y = 0.5, height = 0.6)

  con <- connectGrob(start, end, type = "horizontal_axis")
  line <- attr(con, "line")

  expect_equal(npc_y(line$y[1]), npc_y(coords(start)$y), tolerance = 1e-6)
  expect_equal(npc_y(line$y[2]), npc_y(coords(start)$y), tolerance = 1e-6)
})

test_that("horizontal_axis clamps target y to target bottom and top boundaries", {
  low_start <- axis_box("Low", x = 0.2, y = 0.05)
  high_start <- axis_box("High", x = 0.2, y = 0.95)
  end <- axis_box("End", x = 0.7, y = 0.5, height = 0.6)

  low_con <- connectGrob(low_start, end, type = "horizontal_axis")
  high_con <- connectGrob(high_start, end, type = "horizontal_axis")

  expect_equal(npc_y(attr(low_con, "line")$y[2]), npc_y(coords(end)$bottom), tolerance = 1e-6)
  expect_equal(npc_y(attr(high_con, "line")$y[2]), npc_y(coords(end)$top), tolerance = 1e-6)
})

test_that("horizontal_axis errors when boxes overlap horizontally", {
  start <- axis_box("Start", x = 0.5, y = 0.3, width = 0.3)
  end <- axis_box("End", x = 0.55, y = 0.7, width = 0.3)

  expect_error(
    connectGrob(start, end, type = "horizontal_axis"),
    "horizontally separated"
  )
})

test_that("horizontal behavior remains unchanged", {
  start <- axis_box("Start", x = 0.2, y = 0.3)
  end <- axis_box("End", x = 0.7, y = 0.6)

  con <- connectGrob(start, end, type = "horizontal")
  line <- attr(con, "line")

  expect_equal(npc_y(line$y[1]), npc_y(coords(start)$y), tolerance = 1e-6)
  expect_equal(npc_y(line$y[2]), npc_y(coords(end)$y), tolerance = 1e-6)
})

test_that("many-to-one vertical_axis gives each source its own target landing point", {
  starts <- list(
    axis_box("A", x = 0.25, y = 0.7),
    axis_box("B", x = 0.50, y = 0.7),
    axis_box("C", x = 0.75, y = 0.7)
  )
  end <- axis_box("End", x = 0.5, y = 0.2, width = 0.8)

  con <- connectGrob(starts, end, type = "vertical_axis")

  expect_s3_class(con, "connect_boxes_list")
  expect_equal(length(con), 3)
  expect_equal(
    vapply(con, function(g) npc_x(attr(g, "line")$x[2]), numeric(1)),
    vapply(starts, function(s) npc_x(coords(s)$x), numeric(1)),
    tolerance = 1e-6
  )
})

test_that("many-to-one horizontal_axis gives each source its own target landing point", {
  starts <- list(
    axis_box("A", x = 0.2, y = 0.25),
    axis_box("B", x = 0.2, y = 0.50),
    axis_box("C", x = 0.2, y = 0.75)
  )
  end <- axis_box("End", x = 0.8, y = 0.5, height = 0.8)

  con <- connectGrob(starts, end, type = "horizontal_axis")

  expect_s3_class(con, "connect_boxes_list")
  expect_equal(length(con), 3)
  expect_equal(
    vapply(con, function(g) npc_y(attr(g, "line")$y[2]), numeric(1)),
    vapply(starts, function(s) npc_y(coords(s)$y), numeric(1)),
    tolerance = 1e-6
  )
})

test_that("one-to-many axis connectors return one connector per target", {
  start <- axis_box("Start", x = 0.5, y = 0.8)
  vertical_targets <- list(
    axis_box("A", x = 0.3, y = 0.2, width = 0.4),
    axis_box("B", x = 0.7, y = 0.2, width = 0.4)
  )

  vertical_con <- connectGrob(start, vertical_targets, type = "vertical_axis")

  expect_s3_class(vertical_con, "connect_boxes_list")
  expect_equal(length(vertical_con), 2)

  h_start <- axis_box("Start", x = 0.2, y = 0.5)
  horizontal_targets <- list(
    axis_box("A", x = 0.8, y = 0.3, height = 0.4),
    axis_box("B", x = 0.8, y = 0.7, height = 0.4)
  )

  horizontal_con <- connectGrob(h_start, horizontal_targets, type = "horizontal_axis")

  expect_s3_class(horizontal_con, "connect_boxes_list")
  expect_equal(length(horizontal_con), 2)
})

test_that("S3 list-to-list axis connector behavior remains pairwise", {
  fc <- flowchart(
    starts = list(
      axis_box("A", x = 0.25, y = 0.7),
      axis_box("B", x = 0.75, y = 0.7)
    ),
    ends = list(
      axis_box("C", x = 0.25, y = 0.2),
      axis_box("D", x = 0.75, y = 0.2)
    )
  ) |>
    connect("starts", "ends", type = "vertical_axis")

  con <- tail(attr(fc, "connections"), 1)[[1]]

  expect_s3_class(con, "connect_boxes_list")
  expect_equal(length(con), 2)
})
