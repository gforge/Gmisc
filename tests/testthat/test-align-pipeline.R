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

testthat::test_that("alignVertical accepts piped positional reference path", {
    library(grid)

    boxes <- flowchart(
        rando = "Randomised",
        groups = list("A", "B"),
        groups2 = list("C", "D")
    ) |>
        spread(axis = "y", margin = unit(0.02, "npc")) |>
        spread(subelement = "groups", axis = "x", margin = unit(.2, "npc")) |>
        spread(subelement = "groups2", axis = "x", margin = unit(.2, "npc"))

    expect_silent({
        aligned <- boxes |>
            alignVertical("groups", subelement = "groups2")
    })

    expect_s3_class(aligned, "Gmisc_list_of_boxes")
    expect_equal(length(aligned$groups2), 2)
})

testthat::test_that("alignVertical centers a subelement between two references", {
    library(grid)

    boxes <- flowchart(
        assessed = boxGrob("Assessed", y = 0.9),
        randomised = boxGrob("Randomised", y = 0.5)
    ) |>
        insert(list(excluded = boxGrob("Excluded")), after = "assessed") |>
        move(subelement = "excluded", x = 0.85) |>
        align(
            axis = "y",
            subelement = "excluded",
            references = list("assessed", "randomised"),
            position = "center"
        )

    y <- convertY(coords(boxes$excluded)$y, "npc", valueOnly = TRUE)
    expected <- mean(c(
        convertY(coords(boxes$assessed)$y, "npc", valueOnly = TRUE),
        convertY(coords(boxes$randomised)$y, "npc", valueOnly = TRUE)
    ))

    expect_equal(y, expected, tolerance = 1e-6)
    expect_equal(convertX(coords(boxes$excluded)$x, "npc", valueOnly = TRUE), 0.85, tolerance = 1e-6)
})

testthat::test_that("alignHorizontal centers a subelement between two references", {
    library(grid)

    boxes <- flowchart(
        left = boxGrob("Left", x = 0.2),
        middle = boxGrob("Middle", x = 0.9),
        right = boxGrob("Right", x = 0.8)
    ) |>
        align(
            axis = "x",
            subelement = "middle",
            references = list("left", "right"),
            position = "center"
        )

    x <- convertX(coords(boxes$middle)$x, "npc", valueOnly = TRUE)
    expected <- mean(c(
        convertX(coords(boxes$left)$x, "npc", valueOnly = TRUE),
        convertX(coords(boxes$right)$x, "npc", valueOnly = TRUE)
    ))

    expect_equal(x, expected, tolerance = 1e-6)
})

testthat::test_that("align reference pair validates invalid inputs", {
    library(grid)

    boxes <- flowchart(
        assessed = boxGrob("Assessed", y = 0.9),
        randomised = boxGrob("Randomised", y = 0.5),
        excluded = boxGrob("Excluded")
    )

    expect_error(
        boxes |> align(axis = "y", subelement = "excluded", references = list("assessed")),
        "`references` must be a list containing exactly two references.",
        fixed = TRUE
    )

    expect_error(
        boxes |> align(axis = "y", subelement = "excluded", reference = "assessed", references = list("assessed", "randomised")),
        "Use either `reference` or `references`, not both.",
        fixed = TRUE
    )

    expect_error(
        boxes |> align(axis = "y", subelement = "excluded", references = list("assessed", "missing")),
        "The reference 'missing' was not found in the provided boxes.",
        fixed = TRUE
    )
})
