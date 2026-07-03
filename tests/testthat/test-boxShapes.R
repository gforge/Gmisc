library(testthat)

test_that("box shapes construct without error", {
    expect_silent(db <- boxDatabaseGrob("DB"))
    expect_silent(doc <- boxDocumentGrob("Doc"))
    expect_silent(docs <- boxDocumentsGrob("Docs"))
    expect_silent(tp <- boxTapeGrob("Tape"))
    expect_silent(srv <- boxServerGrob("Server"))

    # server and rack should be visually distinct (not identical grobs)
    expect_silent(rack <- boxRackGrob("Rack"))
    expect_false(identical(rack, srv))

    # connecting them shouldn't error
    expect_silent(con <- connectGrob(db, list(doc, srv), type = "N"))
    expect_true(inherits(con, "connect_boxes_list"))
})

test_that("non-rectangular shapes expose visible bounds for sizing and connectors", {
    tape <- boxTapeGrob("Tape",
                        width = unit(100, "mm"),
                        height = unit(50, "mm"))
    tape_coords <- coords(tape)

    expect_equal(
        convertWidth(tape_coords$width, "mm", valueOnly = TRUE),
        84,
        tolerance = 1e-6
    )
    expect_equal(
        convertHeight(tape_coords$height, "mm", valueOnly = TRUE),
        32,
        tolerance = 1e-6
    )

    plain <- boxGrob("REDCap\nform")
    ellipse <- boxEllipseGrob("REDCap\nform")
    expect_gt(
        convertWidth(attr(ellipse, "viewport_data")$width, "mm", valueOnly = TRUE),
        convertWidth(attr(plain, "viewport_data")$width, "mm", valueOnly = TRUE)
    )
    expect_gt(
        convertHeight(attr(ellipse, "viewport_data")$height, "mm", valueOnly = TRUE),
        convertHeight(attr(plain, "viewport_data")$height, "mm", valueOnly = TRUE)
    )
})

test_that("visible shape bounds survive move()/equalizeWidths() repositioning", {
    tape <- boxTapeGrob("Tape", width = unit(100, "mm"), height = unit(50, "mm"))
    visible0 <- convertWidth(coords(tape)$width, "mm", valueOnly = TRUE)
    expect_equal(visible0, 84, tolerance = 1e-6)

    moved <- moveBox(tape, x = 0.7, y = 0.3)
    expect_equal(convertWidth(coords(moved)$width, "mm", valueOnly = TRUE), 84, tolerance = 1e-6)
    expect_false(is.null(attr(moved, "box_fn_bounds")))

    # Bounds are proportional, so they scale with an equalized width (0.84 * 120).
    eq <- equalizeWidths(list(a = tape, b = boxGrob("x")), width = unit(120, "mm"))
    expect_equal(convertWidth(coords(eq$a)$width, "mm", valueOnly = TRUE), 100.8, tolerance = 1e-6)
})
