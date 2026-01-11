library(testthat)

test_that("labelConnector returns S3 object and print draws", {
    a <- boxDiamondGrob("D")
    b <- boxEllipseGrob("L")
    c <- boxServerGrob("S")

    boxes <- list(decision = a, outcomes = list(b, c)) |>
        spreadHorizontal(.from = unit(.1, "npc"), .to = unit(.9, "npc"), .subelement = "outcomes") |>
        spreadVertical()

    con_list <- connectGrob(boxes$decision, boxes$outcomes, type = "N")

    lbl_obj <- labelConnector(con_list, c("Local", "Server"))
    expect_s3_class(lbl_obj, "Gmisc_connector_label")
    expect_equal(lbl_obj$labels, c("Local", "Server"))
    expect_length(lbl_obj$texts, 2)

    # print method should return invisibly and be the same object
    res <- print(lbl_obj)
    expect_identical(res, lbl_obj)
})
