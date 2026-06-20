library(testthat)

test_that("Box spread vertically with center", {
  box1 <- boxGrob("B1", x = .2, y = .8)
  box2 <- boxGrob("B2", x = .2, y = .8)
  box3 <- boxGrob("B3\ntricky", x = .2, y = .8)
  box4 <- boxGrob("B4", x = .2, y = .8)
  box5 <- boxGrob("B5", x = .2, y = .8)

  spread_boxes <- spreadVertical(box1,
    box2,
    box3,
    b = box4,
    c = box5,
    type = "center"
  )
  sapply(
    spread_boxes,
    function(b) coords(b)$x %>% convertX(unitTo = "npc", valueOnly = TRUE)
  ) %>%
    unique() %>%
    expect_equivalent(.2)

  sapply(
    spread_boxes,
    function(b) coords(b)$y %>% convertX(unitTo = "npc", valueOnly = TRUE)
  ) %>%
    unique() %>%
    length() %>%
    expect_equivalent(length(spread_boxes))
  convertY(coords(spread_boxes[[1]])$top, unitTo = "npc", valueOnly = TRUE) %>%
    expect_equivalent(
      expected = 1,
      tolerance = 1e-3
    )

  convertY(coords(tail(spread_boxes, 1)[[1]])$bottom, unitTo = "npc", valueOnly = TRUE) %>%
    expect_equivalent(
      expected = 0,
      tolerance = 1e-3
    )

  expect_equivalent(
    distance(spread_boxes[1:2], type = "v", center = TRUE),
    distance(spread_boxes[2:3], type = "v", center = TRUE)
  )

  expect_equivalent(
    distance(spread_boxes[3:4], type = "v", center = TRUE),
    distance(spread_boxes[4:5], type = "v", center = TRUE)
  )
})

test_that("Box spread vertically with between", {
  box1 <- boxGrob("B1", x = .2, y = .8)
  box2 <- boxGrob("B2\nmulti\n\n\nline", x = .2, y = .8)
  box3 <- boxGrob("B3", x = .2, y = .8)
  box4 <- boxGrob("B4", x = .2, y = .8)
  box5 <- boxGrob("B5\ntricky", x = .2, y = .8)

  spread_boxes <- spreadVertical(box1,
    box2,
    box3,
    b = box4,
    c = box5,
    type = "between"
  )
  sapply(
    spread_boxes,
    function(b) coords(b)$x %>% convertX(unitTo = "npc", valueOnly = TRUE)
  ) %>%
    unique() %>%
    expect_equivalent(.2)

  sapply(
    spread_boxes,
    function(b) coords(b)$y %>% convertX(unitTo = "npc", valueOnly = TRUE)
  ) %>%
    unique() %>%
    length() %>%
    expect_equivalent(length(spread_boxes))
  convertY(coords(spread_boxes[[1]])$top, unitTo = "npc", valueOnly = TRUE) %>%
    expect_equivalent(
      expected = 1,
      tolerance = 1e-3
    )

  convertY(coords(tail(spread_boxes, 1)[[1]])$bottom, unitTo = "npc", valueOnly = TRUE) %>%
    expect_equivalent(
      expected = 0,
      tolerance = 1e-3
    )

  expect_equivalent(
    distance(spread_boxes[1:2], type = "v"),
    distance(spread_boxes[2:3], type = "v")
  )

  expect_equivalent(
    distance(spread_boxes[3:4], type = "v"),
    distance(spread_boxes[4:5], type = "v")
  )
})

test_that("Box spread vertically can exclude top-level elements", {
  boxes <- flowchart(
    start = boxGrob("Start", y = .9),
    excluded = boxGrob("Excluded", y = .44),
    middle = boxGrob("Middle", y = .6),
    end = boxGrob("End", y = .2)
  )

  out <- boxes |> spread(axis = "y", exclude = "excluded", type = "center")

  expect_equal(
    convertY(coords(out$excluded)$y, "npc", valueOnly = TRUE),
    convertY(coords(boxes$excluded)$y, "npc", valueOnly = TRUE),
    tolerance = 1e-6
  )

  main_y <- vapply(out[c("start", "middle", "end")], function(box) {
    convertY(coords(box)$y, "npc", valueOnly = TRUE)
  }, numeric(1))

  expect_equal(length(unique(round(main_y, 6))), 3)
  expect_equal(unname(abs(main_y[1] - main_y[2])), unname(abs(main_y[2] - main_y[3])), tolerance = 1e-6)
})

test_that("Box spread exclude validates selectors", {
  boxes <- flowchart(
    start = boxGrob("Start"),
    excluded = boxGrob("Excluded"),
    end = boxGrob("End")
  )

  expect_error(
    boxes |> spread(axis = "y", exclude = c("missing", 1)),
    "`exclude` paths must refer to elements directly inside the list being spread.",
    fixed = TRUE
  )

  expect_error(
    boxes |> spread(axis = "y", exclude = list("start", "excluded", "end")),
    "`exclude` cannot remove every element from the spread.",
    fixed = TRUE
  )
})

test_that("Box spread horizontally with center", {
  box1 <- boxGrob("B1 some long text", x = .2, y = .8)
  box2 <- boxGrob("B2", x = .2, y = .8)
  box3 <- boxGrob("B3", x = .2, y = .8)
  box4 <- boxGrob("B4", x = .2, y = .8)
  box5 <- boxGrob("B5", x = .2, y = .8)

  spread_boxes <- spreadHorizontal(box1,
    a = box2,
    box3,
    b = box4,
    box5,
    type = "center"
  )
  sapply(
    spread_boxes,
    function(b) coords(b)$y %>% convertX(unitTo = "npc", valueOnly = TRUE)
  ) %>%
    unique() %>%
    expect_equivalent(.8)

  sapply(
    spread_boxes,
    function(b) coords(b)$x %>% convertX(unitTo = "npc", valueOnly = TRUE)
  ) %>%
    unique() %>%
    length() %>%
    expect_equivalent(length(spread_boxes))

  convertX(coords(spread_boxes[[1]])$left, unitTo = "npc", valueOnly = TRUE) %>%
    expect_equivalent(
      expected = 0,
      tolerance = 1e-3
    )

  convertX(coords(tail(spread_boxes, 1)[[1]])$right, unitTo = "npc", valueOnly = TRUE) %>%
    expect_equivalent(
      expected = 1,
      tolerance = 1e-3
    )

  expect_equivalent(
    distance(spread_boxes[1:2], type = "h", center = TRUE),
    distance(spread_boxes[2:3], type = "h", center = TRUE)
  )

  expect_equivalent(
    distance(spread_boxes[3:4], type = "h", center = TRUE),
    distance(spread_boxes[4:5], type = "h", center = TRUE)
  )
})


test_that("Box spread horizontally with between", {
  box1 <- boxGrob("B1 some long text", x = .2, y = .8)
  box2 <- boxGrob("B2", x = .2, y = .8)
  box3 <- boxGrob("B3", x = .2, y = .8)
  box4 <- boxGrob("B4", x = .2, y = .8)
  box5 <- boxGrob("B5", x = .2, y = .8)

  spread_boxes <- spreadHorizontal(box1,
    a = box2,
    box3,
    b = box4,
    box5,
    type = "between"
  )
  sapply(
    spread_boxes,
    function(b) coords(b)$y %>% convertX(unitTo = "npc", valueOnly = TRUE)
  ) %>%
    unique() %>%
    expect_equivalent(.8)

  sapply(
    spread_boxes,
    function(b) coords(b)$x %>% convertX(unitTo = "npc", valueOnly = TRUE)
  ) %>%
    unique() %>%
    length() %>%
    expect_equivalent(length(spread_boxes))

  convertX(coords(spread_boxes[[1]])$left, unitTo = "npc", valueOnly = TRUE) %>%
    expect_equivalent(
      expected = 0,
      tolerance = 1e-3
    )

  convertX(coords(tail(spread_boxes, 1)[[1]])$right, unitTo = "npc", valueOnly = TRUE) %>%
    expect_equivalent(
      expected = 1,
      tolerance = 1e-3
    )

  expect_equivalent(
    distance(spread_boxes[1:2], type = "h"),
    distance(spread_boxes[2:3], type = "h")
  )

  expect_equivalent(
    distance(spread_boxes[3:4], type = "h"),
    distance(spread_boxes[4:5], type = "h")
  )
})

test_that("spread resolves position references for subelement spans", {
  fc <- flowchart(
    target = boxGrob("Target", y = .5, height = unit(.3, "npc")),
    side = list(
      boxGrob("A"),
      boxGrob("B"),
      boxGrob("C")
    )
  ) |>
    spread(
      axis = "y",
      subelement = "side",
      from = position("target", position = "top", type = "y"),
      to = position("target", position = "bottom", type = "y"),
      margin = unit(0, "npc")
    )

  expect_lte(
    convertY(coords(fc$side[[1]])$top, "npc", valueOnly = TRUE),
    convertY(coords(fc$target)$top, "npc", valueOnly = TRUE)
  )
  expect_gte(
    convertY(coords(fc$side[[3]])$bottom, "npc", valueOnly = TRUE),
    convertY(coords(fc$target)$bottom, "npc", valueOnly = TRUE)
  )
  expect_equal(
    convertY(position(fc$side, position = "center", type = "y"), "npc", valueOnly = TRUE),
    convertY(coords(fc$target)$y, "npc", valueOnly = TRUE),
    tolerance = 1e-6
  )

  fc <- flowchart(
    target = boxGrob("Target", x = .5, width = unit(.5, "npc")),
    side = list(
      boxGrob("A"),
      boxGrob("B"),
      boxGrob("C")
    )
  ) |>
    spread(
      axis = "x",
      subelement = "side",
      from = position("target", position = "left", type = "x"),
      to = position("target", position = "right", type = "x"),
      margin = unit(0, "npc")
    )

  expect_gte(
    convertX(coords(fc$side[[1]])$left, "npc", valueOnly = TRUE),
    convertX(coords(fc$target)$left, "npc", valueOnly = TRUE)
  )
  expect_lte(
    convertX(coords(fc$side[[3]])$right, "npc", valueOnly = TRUE),
    convertX(coords(fc$target)$right, "npc", valueOnly = TRUE)
  )
  expect_equal(
    convertX(position(fc$side, position = "center", type = "x"), "npc", valueOnly = TRUE),
    convertX(coords(fc$target)$x, "npc", valueOnly = TRUE),
    tolerance = 1e-6
  )
})
