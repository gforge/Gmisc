testthat::test_that("get/set list element by path works", {
    # Tests run inside package check; no need to load the package here

    l <- list(
        arms = list(
            early = list("E1", "E2"),
            late = list("L1")
        ),
        detail = list(list("D_early"), list("D_late"))
    )

    expect_equal(get_list_element_by_path(l, c("detail", 1))[[1]], "D_early")
    l2 <- set_list_element_by_path(l, c("detail", 1), list("NEW"))
    expect_equal(l2$detail[[1]][[1]], "NEW")

    # numeric given as character should also work
    expect_equal(get_list_element_by_path(l, c("detail", "1"))[[1]], "D_early")

    # Non-character atomic path (e.g., unit) should not error and return NULL
    library(grid)
    boxes <- list(start = list(a = 1), arms = list(early = "E"))
    expect_null(get_list_element_by_path(boxes, unit(0.25, "npc")))
})
