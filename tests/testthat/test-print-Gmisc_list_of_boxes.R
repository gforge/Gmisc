testthat::test_that("print handles nested plain lists of boxes", {
    library(grid)
    a <- boxGrob("A", x = .2, y = .8)
    b <- boxGrob("B", x = .4, y = .6)

    # nested as plain lists
    nested <- list(grp = list(sub = list(a, b))) |>
        prExtendClass("Gmisc_list_of_boxes")

    # Should not error when printing
    testthat::expect_silent(print(nested))
})
