library(testthat)

test_that("alignHorizontal accepts piped list and uses first as reference", {
    b1 <- boxGrob("b1", x = .1, y = .5)
    b2 <- boxGrob("b2", x = .4, y = .5)
    b3 <- boxGrob("b3", x = .8, y = .5)

    aList <- list(b1, b2, b3)
    alignedList <- aList |> alignHorizontal(position = "center")

    cvt <- function(x) convertX(x, unitTo = "mm", valueOnly = TRUE)
    expect_equal(cvt(attr(alignedList[[1]], "viewport_data")$x), cvt(attr(alignedList[[2]], "viewport_data")$x))
    expect_equal(cvt(attr(alignedList[[1]], "viewport_data")$x), cvt(attr(alignedList[[3]], "viewport_data")$x))
})
