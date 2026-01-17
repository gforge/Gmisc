library(testthat)

test_that("fan_in_center creates bus and single trunk arrow", {
    pop <- boxGrob("Population")
    treatments <- list(
        A = boxGrob("A"),
        B = boxGrob("B"),
        C = boxGrob("C")
    )
    followup <- boxGrob("Follow-up")

    boxes <- list(population = pop, treatment = treatments, followup = followup)
    boxes <- boxes |>
        spreadVertical() |>
        spreadHorizontal(.from = unit(0.1, "npc"), .to = unit(0.9, "npc"), .type = "center", .subelement = "treatment")

    con <- connectGrob(boxes$treatment, boxes$followup, type = "fan_in_center")
    expect_s3_class(con, "connect_boxes_list")

    n <- length(boxes$treatment)
    # Expect stems (n), bus (1), trunk (1)
    expect_equal(length(con), n + 2)

    bus <- con[[n + 1]]
    # bus should be horizontal: both y coords equal
    y_bus <- attr(bus, "line")$y
    y_vals_mm <- vapply(y_bus, function(u) convertY(u, "mm", valueOnly = TRUE), numeric(1))
    expect_equal(y_vals_mm[1], y_vals_mm[2])

    # bus should span the start boxes (plus padding)
    x_bus <- attr(bus, "line")$x
    x_bus_mm <- vapply(x_bus, function(u) convertX(if (!inherits(u, "unit")) unit(u, "npc") else u, "mm", valueOnly = TRUE), numeric(1))
    starts_x_mm <- vapply(lapply(boxes$treatment, coords), function(s) convertX(s$x, "mm", valueOnly = TRUE), numeric(1))
    expect_true((max(x_bus_mm) - min(x_bus_mm)) + 1e-6 >= (max(starts_x_mm) - min(starts_x_mm)))

    trunk <- con[[n + 2]]
    # trunk should have an arrow defined
    expect_true(!is.null(trunk$arrow))

    # trunk should be vertical (both x coords equal) and align with end box center
    trunk_x <- attr(trunk, "line")$x
    trunk_x_vals <- vapply(trunk_x, function(u) convertX(if (!inherits(u, "unit")) unit(u, "npc") else u, "npc", valueOnly = TRUE), numeric(1))
    expect_equal(trunk_x_vals[1], trunk_x_vals[2])

    # trunk should align with the end box center, not the bus center
    e_coords <- coords(boxes$followup)
    end_center_npc <- convertX(e_coords$x, "npc", valueOnly = TRUE)
    expect_equal(trunk_x_vals[1], end_center_npc)

    # trunk should point to the end box edge (top if starts are above, bottom otherwise)
    trunk_end_y <- attr(trunk, "line")$y[2]
    trunk_end_mm <- convertY(if (!inherits(trunk_end_y, "unit")) unit(trunk_end_y, "npc") else trunk_end_y, "mm", valueOnly = TRUE)
    end_top_mm <- convertY(e_coords$top + unit(0.5, "mm"), "mm", valueOnly = TRUE)
    end_bottom_mm <- convertY(e_coords$bottom - unit(0.5, "mm"), "mm", valueOnly = TRUE)
    starts_mean_y_mm <- mean(vapply(lapply(boxes$treatment, coords), function(s) convertY(s$y, "mm", valueOnly = TRUE), numeric(1)))
    end_y_mm <- convertY(e_coords$y, "mm", valueOnly = TRUE)
    if (starts_mean_y_mm > end_y_mm) {
        # starts are above -> trunk should target top edge
        expect_true(abs(trunk_end_mm - end_top_mm) < 1e-2)
    } else {
        # starts below -> trunk should target bottom edge
        expect_true(abs(trunk_end_mm - end_bottom_mm) < 1e-2)
    }
})
