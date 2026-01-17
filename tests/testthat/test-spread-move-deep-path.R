testthat::test_that("spread and move accept deep .subelement paths", {
    library(grid)
    library(checkmate)
    make_stub_box <- function(label, x = 0.5, y = 0.5) {
        b <- list(label = label)
        class(b) <- "box"
        vp <- list(x = unit(x, "npc"), y = unit(y, "npc"), just = c("center", "center"))
        coords <- list(
            left = unit(x, "npc"), right = unit(x, "npc"),
            top = unit(y, "npc"), bottom = unit(y, "npc"),
            x = unit(x, "npc"), y = unit(y, "npc"),
            width = unit(0, "npc"), height = unit(0, "npc"),
            half_width = unit(0, "npc"), half_height = unit(0, "npc")
        )
        attr(b, "viewport_data") <- vp
        attr(b, "coords") <- structure(coords, class = c("box_coords", "list"))
        b
    }

    # create a nested structure of simple placeholders (we don't need real boxGrob properties)
    nested <- list(
        grp = list(
            sub = list(
                make_stub_box("A", x = 0.1, y = 0.9),
                make_stub_box("B", x = 0.1, y = 0.7)
            )
        ),
        other = list(make_stub_box("C", x = 0.8, y = 0.5))
    )

    # If a path does not exist we get informative error
    expect_error(moveBox(nested, x = 0.5, .subelement = c("no", "such")), "The .subelement 'no/such' was not found", fixed = TRUE)

    # Multiple paths: if the first path is missing we get an informative error
    expect_error(moveBox(nested, x = 0.5, .subelement = list(c("missing", "x"), c("grp", "sub", 1))), "The .subelement 'missing/x' was not found", fixed = TRUE)

    # Spread: attempting to target missing path raises same informative error
    expect_error(spreadVertical(nested, .subelement = c("no", "pe")), "The .subelement 'no -> pe' was not found", fixed = TRUE)
})
