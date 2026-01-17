testthat::test_that("alignHorizontal .subelement replacement doesn't create self-referential boxes and connectors split", {
    library(grid)

    start <- boxGrob("start")
    rand <- boxGrob("rand")
    arms <- list(early = boxGrob("early"), late = boxGrob("late"))
    arm_text <- list(boxGrob("ate1"), boxGrob("ate2"))

    boxes <- spreadVertical(
        start = start,
        rand = rand,
        arms = arms,
        arm_text = arm_text,
        footer = boxGrob("footer")
    ) |>
        spreadHorizontal(.subelement = "arms", .from = 0.05, .to = 0.95) |>
        spreadHorizontal(.subelement = "arm_text", .from = 0.05, .to = 0.95) |>
        alignHorizontal(
            reference = c("arm_text", 1),
            .subelement = c("arms", "early")
        ) |>
        alignHorizontal(
            reference = c("arm_text", 2),
            .subelement = c("arms", "late")
        )

    # arms should be a plain list of boxes (not a self-referential copy of `boxes`)
    expect_false(identical(boxes$arms, boxes))
    expect_true(is.list(boxes$arms))
    expect_true(all(vapply(boxes$arms, inherits, logical(1), "box")))

    # Connecting from rand to arms (one-to-many) should produce a multi-connector
    expect_silent(con <- connectGrob(boxes$rand, boxes$arms, type = "N"))
    if (is.list(con)) {
        testthat::expect_s3_class(con, "connect_boxes_list")
    } else {
        testthat::expect_s3_class(con, "connect_boxes")
    }
})
