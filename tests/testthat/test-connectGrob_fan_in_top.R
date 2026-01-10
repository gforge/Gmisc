library(testthat)

test_that("fan_in_top works with default margin and nested lists", {
    # Build a small example similar to the advanced example
    pop <- boxGrob("Population")
    treatments <- list(
        Ibuprofen = boxGrob("Ibuprofen"),
        Paracetamol = boxGrob("Paracetamol"),
        Aspirin = boxGrob("Aspirin")
    )
    followup <- paste("Follow-up 4 weeks:", " - EQ-5D 5L", " - Lab", sep = "\n") |> boxGrob(just = "left")

    boxes <- list(population = pop, treatment = treatments, followup = followup)
    boxes <- boxes |>
        spreadVertical() |>
        spreadHorizontal(.from = unit(0.1, "npc"), .to = unit(0.9, "npc"), .type = "center", .subelement = "treatment")

    # This should not error and should return a connect_boxes_list
    con <- connectGrob(boxes$treatment, boxes$followup, type = "fan_in_top")
    expect_s3_class(con, "connect_boxes_list")

    # Ensure trunks merge on the top edge with a spread (i.e., not all identical end x)
    n <- length(boxes$treatment)
    trunks <- tail(con, n)
    trunk_x_end_mm <- vapply(trunks, function(g) {
        x <- attr(g, "line")$x[2]
        if (!inherits(x, "unit")) x <- unit(x, "npc")
        convertX(x, "mm", valueOnly = TRUE)
    }, numeric(1))
    trunk_x_start_mm <- vapply(trunks, function(g) {
        x <- attr(g, "line")$x[1]
        if (!inherits(x, "unit")) x <- unit(x, "npc")
        convertX(x, "mm", valueOnly = TRUE)
    }, numeric(1))

    # end xs should be spread across the top edge
    expect_gt(max(trunk_x_end_mm) - min(trunk_x_end_mm), 0)

    # trunks should be diagonal (start x != end x) at least for one trunk
    expect_true(any(abs(trunk_x_end_mm - trunk_x_start_mm) > 1e-6))
})
