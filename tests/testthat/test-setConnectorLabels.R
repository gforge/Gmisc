library(testthat)

test_that("setConnectorLabels attaches attributes and printing returns object", {
    a <- boxDiamondGrob("D")
    b <- boxEllipseGrob("L")
    c <- boxServerGrob("S")

    boxes <- list(decision = a, outcomes = list(b, c)) |>
        spreadHorizontal(from = unit(.1, "npc"), to = unit(.9, "npc"), subelement = "outcomes") |>
        spreadVertical()

    con_list <- connectGrob(boxes$decision, boxes$outcomes, type = "N")
    expect_null(attr(con_list, "connector_labels"))

    con2 <- setConnectorLabels(con_list, c("Local", "Server"))
    expect_equal(attr(con2, "connector_labels"), c("Local", "Server"))
    expect_true(!is.null(attr(con2, "connector_label_gp")))

    # printing returns the object invisibly
    res <- print(con2)
    expect_identical(res, con2)
})
