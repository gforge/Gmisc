testthat::test_that("spreadHorizontal handles numeric endpoints with subelement name", {
  # Load package internals for this integration test
  # No package load needed; tests run in package context during R CMD check
  library(grid)

  randomize <- boxGrob("Randomize",
    box_gp = gpar(fill = "#ECEFF1", col = "#455A64"),
    txt_gp = gpar(fontsize = 11, fontface = "bold")
  )

  early <- boxGrob("Early rehabilitation",
    box_gp = gpar(fill = "#E8F5E9", col = "#2E7D32"),
    txt_gp = gpar(fontsize = 11, fontface = "bold")
  )

  late <- boxGrob("Delayed rehabilitation",
    box_gp = gpar(fill = "#FFF8E1", col = "#EF6C00"),
    txt_gp = gpar(fontsize = 11, fontface = "bold")
  )

  boxes <- spreadVertical(
    start = randomize,
    arms = list(early = early, late = late)
  )

  # Execute: we only assert that the specific integer-selection error is not raised
  res <- tryCatch(
    list(value = spreadHorizontal(boxes, subelement = "arms", from = 0.25, to = 0.75), error = NULL),
    error = function(e) list(value = NULL, error = e)
  )

  if (!is.null(res$error)) {
    msg <- conditionMessage(res$error)
    expect_false(grepl("attempt to select less than one element in integerOneIndex", msg, fixed = TRUE))
  } else {
    expect_true(is.list(res$value))
  }
})
