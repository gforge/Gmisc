library(testthat)
library(grid)

test_that("boxHeaderGrob creates a valid box grob", {
    box <- boxHeaderGrob(
        header = "Test Header",
        body = "Line 1\nLine 2\nLine 3"
    )

    expect_s3_class(box, "box")
    expect_s3_class(box, "gTree")
    expect_true(!is.null(attr(box, "coords")))
    expect_true(!is.null(attr(box, "viewport_data")))
})

test_that("boxHeaderGrob works with coords helper", {
    box <- boxHeaderGrob(
        header = "Header",
        body = "Body text",
        x = 0.3,
        y = 0.7
    )

    c <- coords(box)
    expect_s3_class(c, "coords")
    expect_true(!is.null(c$x))
    expect_true(!is.null(c$y))
    expect_true(!is.null(c$left))
    expect_true(!is.null(c$right))
    expect_true(!is.null(c$top))
    expect_true(!is.null(c$bottom))
})

test_that("boxHeaderGrob works with distance helper", {
    box1 <- boxHeaderGrob(
        header = "Box 1",
        body = "Content 1",
        y = 0.8
    )
    box2 <- boxHeaderGrob(
        header = "Box 2",
        body = "Content 2",
        y = 0.2
    )

    d <- distance(box1, box2, type = "v")
    expect_s3_class(d, "unit")
    expect_true(inherits(d, "Gmisc_unit"))
})

test_that("boxHeaderGrob works with connectGrob", {
    box1 <- boxHeaderGrob(
        header = "Start",
        body = "Initial step",
        y = 0.7
    )
    box2 <- boxHeaderGrob(
        header = "End",
        body = "Final step",
        y = 0.3
    )

    conn <- connectGrob(box1, box2, type = "vertical")
    expect_s3_class(conn, "connect_boxes")
})

test_that("boxHeaderGrob works with spreadVertical", {
    boxes <- list(
        early = boxHeaderGrob("Early", "Step 1\nStep 2"),
        late = boxHeaderGrob("Late", "Step A\nStep B")
    )

    result <- spreadVertical(boxes)
    expect_s3_class(result, "Gmisc_list_of_boxes")
    expect_length(result, 2)

    # Check that positions were updated
    c1 <- coords(result$early)
    c2 <- coords(result$late)
    expect_false(identical(
        convertY(c1$y, "npc", valueOnly = TRUE),
        convertY(c2$y, "npc", valueOnly = TRUE)
    ))
})

test_that("boxHeaderGrob works with alignHorizontal", {
    box1 <- boxHeaderGrob(
        header = "Box 1",
        body = "Content",
        x = 0.2,
        y = 0.7
    )
    box2 <- boxHeaderGrob(
        header = "Box 2",
        body = "Content",
        x = 0.8,
        y = 0.3
    )

    # alignHorizontal should successfully align two composite boxes
    result <- alignHorizontal(box1, box2)
    expect_s3_class(result, "Gmisc_list_of_boxes")
    expect_true(length(result) > 0)
})

test_that("boxHeaderGrob accepts custom styling", {
    box <- boxHeaderGrob(
        header = "Styled Header",
        body = "Styled body",
        header_gp = gpar(fontsize = 12, fontface = "bold", col = "blue"),
        body_gp = gpar(fontsize = 9, col = "darkgray"),
        box_gp = gpar(fill = "#E8F5E9", col = "#2E7D32", lwd = 2)
    )

    expect_s3_class(box, "box")
    # Verify grob tree contains header and body
    expect_equal(length(box$children), 3) # rect, header, body
})

test_that("boxHeaderGrob works in user's rehabilitation flowchart example", {
    # Simulate the user's use case
    early_gp_header <- list(
        box_gp = gpar(fill = "#E8F5E9", col = "#2E7D32", lwd = 1.4),
        header_gp = gpar(fontsize = 10.5, fontface = "bold"),
        body_gp = gpar(fontsize = 9)
    )

    box <- do.call(boxHeaderGrob, c(
        list(
            header = "Early rehabilitation",
            body = paste("0-1 weeks", "• Instruction", "• Pendulum + assisted ROM", sep = "\n")
        ),
        early_gp_header
    ))

    expect_s3_class(box, "box")

    # Should work with spread
    boxes <- list(
        box1 = box,
        box2 = boxHeaderGrob("Delayed", "0-3 weeks\n• Sling")
    )
    result <- spreadVertical(boxes)
    expect_length(result, 2)
})
