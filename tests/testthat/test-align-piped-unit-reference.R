testthat::test_that("piped list + unit reference works (vignette case)", {
    library(grid)

    a_boxes <- paste("A", 1:3) |>
        lapply(function(x) boxGrob(x, box_gp = gpar(fill = "#E6F2FF"))) |>
        spreadHorizontal(from = unit(.1, "npc"), to = unit(1, "npc") - unit(1, "cm"))

    # This used to error with 'operator == not meaningful for units' during resolution
    expect_silent({
        res <- a_boxes |> alignVertical(position = "top", reference = unit(1, "npc"))
    })
})
