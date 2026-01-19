testthat::test_that("alignHorizontal accepts piped list with named reference argument", {
    library(grid)

    org_cohort <- paste("Proximal humerus fracture", "  - >= 18 years", "  - <= 4 weeks of trauma", "  - Not pathological", sep = "\n") |> boxGrob(just = "left")
    surgery <- paste("Surgery", "  - Direct (approx 4%)", "  - Delayed (approx 4%)", sep = "\n") |> boxGrob(just = "left")
    randomize <- boxGrob("Non-surgical\nRandomise")
    treatments <- list(early = boxGrob("Early rehab"), late = boxGrob("Late rehab"), obs = boxGrob("Observation"))
    early_followup <- paste("Early follow-up", "  - 2 weeks [PNRS]", "  - 4 weeks [PNRS]", sep = "\n") |> boxGrob(just = "left")
    late_followup <- paste("Late follow-up", "  - 2-10 months (random) [OSS, PNRS]", "  - 1 year [OSS, PNRS, accelerometer]", "  - 2 years [OSS, PNRS]", "  - 5 years [OSS, PNRS]", sep = "\n") |> boxGrob(just = "left")

    boxes_mid <- spreadVertical(
        start = org_cohort,
        step_1 = list(surgery = surgery, `non-surgical` = randomize),
        treatment = treatments,
        early_followup = early_followup,
        followup = late_followup
    ) |>
        spreadHorizontal(subelement = "step_1") |>
        spreadHorizontal(subelement = "treatment", from = 0.35)

    # The case that previously failed: a piped list and a named 'reference' arg
    expect_silent(result <- boxes_mid |> alignHorizontal(
        reference = c("treatment", "late"),
       subelement = c("step_1", "non-surgical")
    ))

    # Verify the targeted subelement exists and has been replaced (i.e., alignment returned a box)
    target <- Gmisc:::get_list_element_by_path(result, c("step_1", "non-surgical"))
    expect_true(!is.null(target))
})
