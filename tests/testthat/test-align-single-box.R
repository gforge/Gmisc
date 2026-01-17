testthat::test_that("align functions accept a single box as target (non-list) and return list", {
    library(grid)
    ref <- boxGrob("Ref", x = .5, y = .5)
    target <- boxGrob("T", x = .6, y = .2)

    res_h <- alignHorizontal(reference = ref, target, .position = "left")
    testthat::expect_true(inherits(res_h, "Gmisc_list_of_boxes"))

    res_v <- alignVertical(reference = ref, target, .position = "top")
    testthat::expect_true(inherits(res_v, "Gmisc_list_of_boxes"))
})
