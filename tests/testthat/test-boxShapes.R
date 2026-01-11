library(testthat)

test_that("box shapes construct without error", {
    expect_silent(db <- boxDatabaseGrob("DB"))
    expect_silent(doc <- boxDocumentGrob("Doc"))
    expect_silent(docs <- boxDocumentsGrob("Docs"))
    expect_silent(tp <- boxTapeGrob("Tape"))
    expect_silent(srv <- boxServerGrob("Server"))

    # server and rack should be visually distinct (not identical grobs)
    expect_silent(rack <- boxRackGrob("Rack"))
    expect_false(identical(rack, srv))

    # connecting them shouldn't error
    expect_silent(con <- connectGrob(db, list(doc, srv), type = "N"))
    expect_true(inherits(con, "connect_boxes_list"))
})
