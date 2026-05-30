testthat::test_that("connectGrob to group containers uses first box in each group", {
    library(grid)

    # Build grouped boxes: each group is a list of boxes (header then content boxes)
    header <- function(txt) structure(txt, args = list(box_gp = gpar(fill = "white")))
    content <- function(txt) structure(txt, args = list(box_gp = gpar(fill = "grey90")))

    boxes <- list(
        common = boxGrob("TOP"),
        groups = list(
            early = list(boxGrob("Early header"), boxGrob("E1"), boxGrob("E2")),
            late  = list(boxGrob("Late header"), boxGrob("L1"))
        )
    ) |>
        spreadVertical() |>
        spreadHorizontal(subelement = "groups", from = 0.2, to = 0.8)

    # Connect common to groups: expecting one connector per group (2 connectors)
    con <- connectGrob(boxes$common, boxes$groups, type = "N")
    testthat::expect_true(is.list(con) || inherits(con, "connect_boxes"))
    if (is.list(con)) {
        testthat::expect_length(con, length(boxes$groups))
    }

    # Check that each connector's endpoints point to each group's first box
    ends <- lapply(boxes$groups, function(g) prConvert2Coords(g[[1]]))
    # For N connector, assigned end x should match group's header x
    con_list <- if (is.list(con)) con else list(con)
    assigned_end_x <- vapply(con_list, function(g) convertX(attr(g, "line")$x[4], "npc", valueOnly = TRUE), numeric(1))
    expected_x <- vapply(ends, function(e) convertX(e$x, "npc", valueOnly = TRUE), numeric(1))
    testthat::expect_equal(sort(unname(assigned_end_x)), sort(unname(expected_x)))
})
