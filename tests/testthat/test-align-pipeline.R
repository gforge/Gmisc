testthat::test_that("alignHorizontal pipeline with subelement works end-to-end", {
    library(grid)

    org_cohort <- paste("Proximal humerus fracture", "  - >= 18 years", "  - <= 4 weeks of trauma", "  - Not pathological", sep = "\n") |> boxGrob(just = "left")
    surgery <- paste("Surgery", "  - Direct (approx 4%)", "  - Delayed (approx 4%)", sep = "\n") |> boxGrob(just = "left")
    randomize <- boxGrob("Non-surgical\nRandomise")
    treatments <- list(early = boxGrob("Early rehab"), late = boxGrob("Late rehab"), obs = boxGrob("Observation"))
    early_followup <- paste("Early follow-up", "  - 2 weeks [PNRS]", "  - 4 weeks [PNRS]", sep = "\n") |> boxGrob(just = "left")
    late_followup <- paste("Late follow-up", "  - 2-10 months (random) [OSS, PNRS]", "  - 1 year [OSS, PNRS, accelerometer]", "  - 2 years [OSS, PNRS]", "  - 5 years [OSS, PNRS]", sep = "\n") |> boxGrob(just = "left")

    expect_silent({
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
    })
})
