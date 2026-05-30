testthat::test_that("connectGrob handles nested step_1 from vignette flow", {
  library(grid)

  org_cohort <- paste("Org cohort",
                      "  - N = 180 patients",
                      "  - Age 18-65 years",
                      sep = "\n"
  ) |> boxGrob(just = "left")
  surgery <- paste("Surgery",
                   "  - Direct (approx 10%)",
                   "  - Delayed (approx 10%)",
                   sep = "\n"
  ) |> boxGrob(just = "left")
  randomize <- boxGrob("Non-surgical\nRandomise")
  treatments <- list(early = boxGrob("Early rehab"), late = boxGrob("Late rehab"), obs = boxGrob("Observation"))
  early_followup <- paste("Early follow-up",
                          "  - 2 weeks [PNRS]",
                          "  - 4 weeks [PNRS]",
                          sep = "\n"
  ) |> boxGrob(just = "left")
  late_followup <- paste("Late follow-up",
                         "  - 3 months",
                         "  - 1 year",
                         "  - 2 years",
                         "  - 5 years",
                         sep = "\n"
  ) |> boxGrob(just = "left")

  boxes <- spreadVertical(
    start = org_cohort,
    step_1 = list(surgery = surgery, `non-surgical` = randomize),
    treatment = treatments,
    early_followup = early_followup,
    followup = late_followup
  ) |>
    spreadHorizontal(subelement = "step_1") |>
    spreadHorizontal(subelement = "treatment", from = 0.35) |>
    alignHorizontal(
      reference = c("treatment", "late"),
     subelement = c("step_1", "non-surgical")
    )

  # Should not error: connect from start to the step_1 group
  expect_silent(con <- connectGrob(boxes$start, boxes$step_1, type = "N"))
  # If returns a list of connectors, check class
  if (is.list(con)) {
    testthat::expect_s3_class(con, "connect_boxes_list")
  } else {
    testthat::expect_s3_class(con, "connect_boxes")
  }

  expect_silent(con <- connectGrob(boxes$step_1$`non-surgical`, boxes$treatment, type = "N"))
  if (is.list(con)) {
    testthat::expect_s3_class(con, "connect_boxes_list")
  } else {
    testthat::expect_s3_class(con, "connect_boxes")
  }
})
