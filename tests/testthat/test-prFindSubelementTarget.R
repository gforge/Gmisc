testthat::test_that("prFindSubelementTarget finds in top-level and nested container", {
    nested <- list(a = list(b = list("x")), c = 1)
    res1 <- Gmisc:::prFindSubelementTarget(nested, c("a", "b"))
    testthat::expect_true(!is.null(res1$target))
    testthat::expect_false(res1$container_is_first)

    # not found in top-level, but in nested first element
    containers <- list(list(sub = list("A")), other = list())
    res2 <- Gmisc:::prFindSubelementTarget(containers, c("sub"))
    testthat::expect_true(!is.null(res2$target))
    testthat::expect_true(res2$container_is_first)

    # missing path
    res3 <- Gmisc:::prFindSubelementTarget(nested, c("no", "pe"))
    testthat::expect_null(res3$target)
})
