testthat::test_that("alignVertical handles numeric reference 0 as coordinate, not list index", {
    a <- 5
    res <- testthat::expect_error(alignVertical(
        reference = 0, .position = "bottom",
        bquote(alpha == theta[1] * .(a) + ldots) |> boxGrob(),
        paste("argument", sQuote("x"), "\nmust be non-zero") |> boxGrob()
    ), NA)
    testthat::expect_true(inherits(res, "Gmisc_list_of_boxes"))
})
