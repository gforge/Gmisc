testthat::test_that("align accepts deep subelement paths and reports missing ones", {
    # No package load needed; tests run in package context during R CMD check

    nested <- list(a = list(b = list("x")), c = 1)

    expect_error(alignHorizontal(nested, subelement = c("no", "pe")), "The subelement 'no -> pe' was not found", fixed = TRUE)
    expect_error(alignVertical(nested, subelement = list(c("no", "pe"), c("a", "b"))), "The subelement 'no -> pe' was not found", fixed = TRUE)
})
