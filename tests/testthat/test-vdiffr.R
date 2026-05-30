library(testthat)

context("visual regression")

if (!requireNamespace("vdiffr", quietly = TRUE)) {
    skip("vdiffr not available")
}

# simple box grob change

test_that("boxGrob basic appearance", {
    grid::grid.newpage()
    g <- boxGrob("Test", x = .5, y = .5)
    vdiffr::expect_doppelganger("basic box", g)
})
