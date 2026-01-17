testthat::test_that("prIsNestedNonBoxList and prIsSingleElementWrappedList work as expected", {
    # Nested list where first element is a list (should be TRUE)
    x1 <- list(list(a = 1), b = 2)
    testthat::expect_true(Gmisc:::prIsNestedNonBoxList(x1))
    testthat::expect_false(Gmisc:::prIsSingleElementWrappedList(x1))

    # Single-element wrapped list (should be TRUE)
    x2 <- list(list(a = 1))
    testthat::expect_true(Gmisc:::prIsNestedNonBoxList(x2))
    testthat::expect_true(Gmisc:::prIsSingleElementWrappedList(x2))

    # If the inner element is actually a box/grob, we shouldn't treat it as nested
    bx <- boxGrob("x")
    x3 <- list(bx)
    testthat::expect_false(Gmisc:::prIsNestedNonBoxList(x3))
    testthat::expect_false(Gmisc:::prIsSingleElementWrappedList(x3))

    # Multiple top-level elements where the first is a nested container
    x5 <- list(list(sub = list("A")), other = list())
    testthat::expect_true(Gmisc:::prIsNestedNonBoxList(x5))
    testthat::expect_true(Gmisc:::prHasNestedFirstContainer(x5))
    testthat::expect_false(Gmisc:::prIsSingleElementWrappedList(x5))

    # Non-list inner element
    x4 <- list(1)
    testthat::expect_false(Gmisc:::prIsNestedNonBoxList(x4))
    testthat::expect_false(Gmisc:::prIsSingleElementWrappedList(x4))
})
