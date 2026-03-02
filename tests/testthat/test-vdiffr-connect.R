library(testthat)

context("vdiffr connectors")

if (!requireNamespace("vdiffr", quietly = TRUE)) {
    skip("vdiffr not installed")
}

test_that("simple connector grob matches reference", {
    grid::grid.newpage()
    a <- boxGrob("A", x = .2, y = .5)
    b <- boxGrob("B", x = .8, y = .5)
    con <- connectGrob(a, b)
    vdiffr::expect_doppelganger("two-box connector", con)
})
