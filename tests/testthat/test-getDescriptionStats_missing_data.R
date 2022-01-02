library("testthat")
library("stringr")

test_that("missing levels are handled correctly when using custom descriptive functions", {
  set.seed(1)
  trial <- data.frame(
    visit = sort(rep(c("randomisation", "week1", "week2", "week3"), 5)),
    arm = sort(rep(c("control", "treatment"))),
    outcome = rnorm(40),
    stringsAsFactors = TRUE
  )
  trial_missing_first <- trial[!((trial$visit == "randomisation") & (trial$arm == "control")), ]
  trial_missing_second <- trial[!((trial$visit == "randomisation") & (trial$arm == "treatment")), ]
  trial_missing_both <- trial[trial$visit != "week3", ]

  trial_2 <- data.frame(
    visit = sort(rep(c("randomisation", "week1", "week2", "week3"), 5)),
    arm = sort(rep(c("control", "standard treatment", "new treatment"))),
    outcome = rnorm(60),
    stringsAsFactors = TRUE
  )
  trial_2_missing_first <- trial_2[!((trial_2$visit == "randomisation") & (trial_2$arm == "control")), ]
  trial_2_missing_second <- trial_2[!((trial_2$visit == "randomisation") & (trial_2$arm == "standard treatment")), ]
  trial_2_missing_outer <- trial_2[!((trial_2$visit == "randomisation") & (trial_2$arm != "new treatment")), ]
  trial_2_missing_all <- trial_2[trial_2$visit != "week3", ]

  descriptive_function <- function(x, ...) {
    result <- c(
      describeMean(x, ...),
      describeMedian(x, ...)
    )
    return(result)
  }

  expected_no_missing <- structure(
    c(
      "0.1 (&plusmn;0.7)", "0.2 (-0.5 - 0.5)", "0.1 (&plusmn;0.5)",
      "0.2 (-0.2 - 0.5)", "0.4 (&plusmn;1.2)", "0.8 (-0.4 - 1.3)",
      "0.4 (&plusmn;0.7)", "0.4 (-0.1 - 0.9)", "0.1 (&plusmn;1.5)",
      "0.5 (-0.4 - 1.0)", "-0.2 (&plusmn;0.8)", "-0.2 (-0.7 - 0.3)",
      "-0.5 (&plusmn;1.2)", "-0.1 (-0.6 - 0.1)", "0.3 (&plusmn;0.5)",
      "0.3 (-0.1 - 0.7)"
    ),
    .Dim = c(8L, 2L),
    .Dimnames = list(
      c(
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)"
      ),
      c("control", "treatment")
    ),
    rgroup = c("randomisation", "week1", "week2", "week3"),
    n.rgroup = c(2, 2, 2, 2),
    htmlTable_args = structure(list(), .Names = character(0)),
    class = c("descMrg", class(matrix(1)))
  )

  expected_missing_first <- structure(
    c(
      "-", "-", "0.1 (&plusmn;0.5)", "0.2 (-0.2 - 0.5)",
      "0.4 (&plusmn;1.2)", "0.8 (-0.4 - 1.3)", "0.4 (&plusmn;0.7)",
      "0.4 (-0.1 - 0.9)", "0.1 (&plusmn;1.5)", "0.5 (-0.4 - 1.0)",
      "-0.2 (&plusmn;0.8)", "-0.2 (-0.7 - 0.3)", "-0.5 (&plusmn;1.2)",
      "-0.1 (-0.6 - 0.1)", "0.3 (&plusmn;0.5)", "0.3 (-0.1 - 0.7)"
    ),
    .Dim = c(8L, 2L),
    .Dimnames = list(
      c(
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)"
      ),
      c("control", "treatment")
    ),
    rgroup = c("randomisation", "week1", "week2", "week3"),
    n.rgroup = c(2, 2, 2, 2),
    htmlTable_args = structure(list(), .Names = character(0)),
    class = c("descMrg", class(matrix(1)))
  )

  expected_missing_second <- structure(
    c(
      "0.1 (&plusmn;0.7)", "0.2 (-0.5 - 0.5)", "0.1 (&plusmn;0.5)",
      "0.2 (-0.2 - 0.5)", "0.4 (&plusmn;1.2)", "0.8 (-0.4 - 1.3)",
      "0.4 (&plusmn;0.7)", "0.4 (-0.1 - 0.9)", "-", "-", "-0.2 (&plusmn;0.8)",
      "-0.2 (-0.7 - 0.3)", "-0.5 (&plusmn;1.2)", "-0.1 (-0.6 - 0.1)",
      "0.3 (&plusmn;0.5)", "0.3 (-0.1 - 0.7)"
    ),
    .Dim = c(8L, 2L),
    .Dimnames = list(
      c(
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)"
      ),
      c("control", "treatment")
    ),
    rgroup = c("randomisation", "week1", "week2", "week3"),
    n.rgroup = c(2, 2, 2, 2),
    htmlTable_args = structure(list(), .Names = character(0)),
    class = c("descMrg", class(matrix(1)))
  )

  expected_missing_both <- structure(
    c(
      "0.1 (&plusmn;0.7)", "0.2 (-0.5 - 0.5)", "0.1 (&plusmn;0.5)",
      "0.2 (-0.2 - 0.5)", "0.4 (&plusmn;1.2)", "0.8 (-0.4 - 1.3)",
      "-", "-", "0.1 (&plusmn;1.5)", "0.5 (-0.4 - 1.0)", "-0.2 (&plusmn;0.8)",
      "-0.2 (-0.7 - 0.3)", "-0.5 (&plusmn;1.2)", "-0.1 (-0.6 - 0.1)",
      "-", "-"
    ),
    .Dim = c(8L, 2L),
    .Dimnames = list(
      c(
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)"
      ),
      c("control", "treatment")
    ),
    rgroup = c("randomisation", "week1", "week2", "week3"),
    n.rgroup = c(2, 2, 2, 2),
    htmlTable_args = structure(list(), .Names = character(0)),
    class = c("descMrg", class(matrix(1)))
  )

  expected_no_missing_2 <- structure(
    c(
      "0.2 (&plusmn;0.7)", "0.0 (-0.2 - 0.6)", "0.7 (&plusmn;0.5)",
      "0.4 (0.4 - 0.9)", "0.5 (&plusmn;1.0)", "0.5 (0.3 - 1.2)",
      "0.2 (&plusmn;1.1)", "-0.4 (-0.6 - 0.6)", "-0.5 (&plusmn;0.8)",
      "-0.6 (-0.7 - -0.3)", "0.5 (&plusmn;0.4)", "0.3 (0.2 - 0.8)",
      "-0.3 (&plusmn;1.0)", "-0.7 (-1.1 - 0.4)", "-0.2 (&plusmn;0.7)",
      "-0.1 (-0.4 - 0.0)", "0.7 (&plusmn;1.0)", "0.6 (0.0 - 0.7)",
      "-0.2 (&plusmn;1.5)", "-0.3 (-0.7 - -0.1)", "0.3 (&plusmn;0.9)",
      "0.6 (-0.5 - 0.7)", "-0.5 (&plusmn;0.7)", "-0.5 (-1.0 - 0.1)"
    ),
    .Dim = c(8L, 3L),
    .Dimnames = list(
      c(
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)"
      ),
      c("control", "new treatment", "standard treatment")
    ),
    rgroup = c("randomisation", "week1", "week2", "week3"),
    n.rgroup = c(2, 2, 2, 2),
    htmlTable_args = structure(list(), .Names = character(0)),
    class = c("descMrg", class(matrix(1)))
  )

  expected_missing_first_2 <- structure(
    c(
      "-", "-", "0.7 (&plusmn;0.5)", "0.4 (0.4 - 0.9)",
      "0.5 (&plusmn;1.0)", "0.5 (0.3 - 1.2)", "0.2 (&plusmn;1.1)",
      "-0.4 (-0.6 - 0.6)", "-0.5 (&plusmn;0.8)", "-0.6 (-0.7 - -0.3)",
      "0.5 (&plusmn;0.4)", "0.3 (0.2 - 0.8)", "-0.3 (&plusmn;1.0)",
      "-0.7 (-1.1 - 0.4)", "-0.2 (&plusmn;0.7)", "-0.1 (-0.4 - 0.0)",
      "0.7 (&plusmn;1.0)", "0.6 (0.0 - 0.7)", "-0.2 (&plusmn;1.5)",
      "-0.3 (-0.7 - -0.1)", "0.3 (&plusmn;0.9)", "0.6 (-0.5 - 0.7)",
      "-0.5 (&plusmn;0.7)", "-0.5 (-1.0 - 0.1)"
    ),
    .Dim = c(8L, 3L),
    .Dimnames = list(
      c(
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)"
      ),
      c("control", "new treatment", "standard treatment")
    ),
    rgroup = c("randomisation", "week1", "week2", "week3"),
    n.rgroup = c(2, 2, 2, 2),
    htmlTable_args = structure(list(), .Names = character(0)),
    class = c("descMrg", class(matrix(1)))
  )

  expected_missing_second_2 <- structure(
    c(
      "0.2 (&plusmn;0.7)", "0.0 (-0.2 - 0.6)", "0.7 (&plusmn;0.5)",
      "0.4 (0.4 - 0.9)", "0.5 (&plusmn;1.0)", "0.5 (0.3 - 1.2)",
      "0.2 (&plusmn;1.1)", "-0.4 (-0.6 - 0.6)", "-0.5 (&plusmn;0.8)",
      "-0.6 (-0.7 - -0.3)", "0.5 (&plusmn;0.4)", "0.3 (0.2 - 0.8)",
      "-0.3 (&plusmn;1.0)", "-0.7 (-1.1 - 0.4)", "-0.2 (&plusmn;0.7)",
      "-0.1 (-0.4 - 0.0)", "-", "-", "-0.2 (&plusmn;1.5)", "-0.3 (-0.7 - -0.1)",
      "0.3 (&plusmn;0.9)", "0.6 (-0.5 - 0.7)", "-0.5 (&plusmn;0.7)",
      "-0.5 (-1.0 - 0.1)"
    ),
    .Dim = c(8L, 3L),
    .Dimnames = list(
      c(
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)"
      ),
      c("control", "new treatment", "standard treatment")
    ),
    rgroup = c("randomisation", "week1", "week2", "week3"),
    n.rgroup = c(2, 2, 2, 2),
    htmlTable_args = structure(list(), .Names = character(0)),
    class = c("descMrg", class(matrix(1)))
  )

  expected_missing_outer <- structure(
    c(
      "-", "-", "0.7 (&plusmn;0.5)", "0.4 (0.4 - 0.9)",
      "0.5 (&plusmn;1.0)", "0.5 (0.3 - 1.2)", "0.2 (&plusmn;1.1)",
      "-0.4 (-0.6 - 0.6)", "-0.5 (&plusmn;0.8)", "-0.6 (-0.7 - -0.3)",
      "0.5 (&plusmn;0.4)", "0.3 (0.2 - 0.8)", "-0.3 (&plusmn;1.0)",
      "-0.7 (-1.1 - 0.4)", "-0.2 (&plusmn;0.7)", "-0.1 (-0.4 - 0.0)",
      "-", "-", "-0.2 (&plusmn;1.5)", "-0.3 (-0.7 - -0.1)", "0.3 (&plusmn;0.9)",
      "0.6 (-0.5 - 0.7)", "-0.5 (&plusmn;0.7)", "-0.5 (-1.0 - 0.1)"
    ),
    .Dim = c(8L, 3L),
    .Dimnames = list(
      c(
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)"
      ),
      c("control", "new treatment", "standard treatment")
    ),
    rgroup = c("randomisation", "week1", "week2", "week3"),
    n.rgroup = c(2, 2, 2, 2),
    htmlTable_args = structure(list(), .Names = character(0)),
    class = c("descMrg", class(matrix(1)))
  )

  expected_missing_all <- structure(
    c(
      "0.2 (&plusmn;0.7)", "0.0 (-0.2 - 0.6)", "0.7 (&plusmn;0.5)",
      "0.4 (0.4 - 0.9)", "0.5 (&plusmn;1.0)", "0.5 (0.3 - 1.2)", "-",
      "-", "-0.5 (&plusmn;0.8)", "-0.6 (-0.7 - -0.3)", "0.5 (&plusmn;0.4)",
      "0.3 (0.2 - 0.8)", "-0.3 (&plusmn;1.0)", "-0.7 (-1.1 - 0.4)",
      "-", "-", "0.7 (&plusmn;1.0)", "0.6 (0.0 - 0.7)", "-0.2 (&plusmn;1.5)",
      "-0.3 (-0.7 - -0.1)", "0.3 (&plusmn;0.9)", "0.6 (-0.5 - 0.7)", "-", "-"
    ),
    .Dim = c(8L, 3L),
    .Dimnames = list(
      c(
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)",
        "Mean (SD)", "Median (IQR)"
      ),
      c("control", "new treatment", "standard treatment")
    ),
    rgroup = c("randomisation", "week1", "week2", "week3"),
    n.rgroup = c(2, 2, 2, 2),
    htmlTable_args = structure(list(), .Names = character(0)),
    class = c("descMrg", class(matrix(1)))
  )

  out <- mergeDesc(lapply(levels(trial$visit),
                          function(x) {
                            getDescriptionStatsBy(
                              x = trial$outcome[trial$visit == x],
                              by = trial$arm[trial$visit == x],
                              continuous_fn = descriptive_function
                            )
                          }),
                   htmlTable_args = list(
                     rgroup = levels(trial$visit),
                     n.rgroup = rep(2, 4))
  )
  expect_identical(out, expected_no_missing)

  out <- mergeDesc(lapply(levels(trial_missing_first$visit),
                          function(x) {
                            getDescriptionStatsBy(
                              x = trial_missing_first$outcome[trial_missing_first$visit == x],
                              by = trial_missing_first$arm[trial_missing_first$visit == x],
                              continuous_fn = descriptive_function
                            )
                          }),
                   htmlTable_args = list(
                     rgroup = levels(trial_missing_first$visit),
                     n.rgroup = rep(2, 4))
  )
  expect_identical(out, expected_missing_first)

  out <- mergeDesc(lapply(levels(trial_missing_second$visit),
                          function(x) {
                            getDescriptionStatsBy(
                              x = trial_missing_second$outcome[trial_missing_second$visit == x],
                              by = trial_missing_second$arm[trial_missing_second$visit == x],
                              continuous_fn = descriptive_function
                            )
                          }),
                   htmlTable_args = list(
                     rgroup = levels(trial_missing_second$visit),
                     n.rgroup = rep(2, 4)
                   )
  )
  expect_identical(out, expected_missing_second)

  out <- mergeDesc(lapply(levels(trial_missing_both$visit),
                          function(x) {
                            getDescriptionStatsBy(
                              x = trial_missing_both$outcome[trial_missing_both$visit == x],
                              by = trial_missing_both$arm[trial_missing_both$visit == x],
                              continuous_fn = descriptive_function,
                              names_of_missing = c("Mean (SD)", "Median (IQR)")
                            )
                          }),
                   htmlTable_args = list(
                     rgroup = levels(trial_missing_both$visit),
                     n.rgroup = rep(2, 4)
                   )
  )
  expect_identical(out, expected_missing_both)

  out <- mergeDesc(lapply(levels(trial_2$visit),
                          function(x) {
                            getDescriptionStatsBy(
                              x = trial_2$outcome[trial_2$visit == x],
                              by = trial_2$arm[trial_2$visit == x],
                              continuous_fn = descriptive_function
                            )
                          }),
                   htmlTable_args = list(
                     rgroup = levels(trial_2$visit),
                     n.rgroup = rep(2, 4)
                   )
  )
  expect_identical(out, expected_no_missing_2)

  out <- mergeDesc(lapply(levels(trial_2_missing_first$visit),
                          function(x) {
                            getDescriptionStatsBy(
                              x = trial_2_missing_first$outcome[trial_2_missing_first$visit == x],
                              by = trial_2_missing_first$arm[trial_2_missing_first$visit == x],
                              continuous_fn = descriptive_function
                            )
                          }),
                   htmlTable_args = list(
                     rgroup = levels(trial_2_missing_first$visit),
                     n.rgroup = rep(2, 4)
                   )
  )
  expect_identical(out, expected_missing_first_2)

  out <- mergeDesc(lapply(levels(trial_2_missing_second$visit),
                          function(x) {
                            getDescriptionStatsBy(
                              x = trial_2_missing_second$outcome[trial_2_missing_second$visit == x],
                              by = trial_2_missing_second$arm[trial_2_missing_second$visit == x],
                              continuous_fn = descriptive_function,
                              names_of_missing = c("Mean (SD)", "Median (IQR)")
                            )
                          }),
                   htmlTable_args = list(
                     rgroup = levels(trial_2_missing_second$visit),
                     n.rgroup = rep(2, 4)
                   )
  )
  expect_identical(out, expected_missing_second_2)

  out <- mergeDesc(lapply(levels(trial_2_missing_outer$visit),
                          function(x) {
                            getDescriptionStatsBy(
                              x = trial_2_missing_outer$outcome[trial_2_missing_outer$visit == x],
                              by = trial_2_missing_outer$arm[trial_2_missing_outer$visit == x],
                              continuous_fn = descriptive_function,
                              names_of_missing = c("Mean (SD)", "Median (IQR)")
                            )
                          }),
                   htmlTable_args = list(
                     rgroup = levels(trial_2_missing_outer$visit),
                     n.rgroup = rep(2, 4)
                   )
  )
  expect_identical(out, expected_missing_outer)

  out <- mergeDesc(lapply(levels(trial_2_missing_all$visit),
                          function(x) {
                            getDescriptionStatsBy(trial_2_missing_all$outcome[trial_2_missing_all$visit == x],
                                                  trial_2_missing_all$arm[trial_2_missing_all$visit == x],
                                                  continuous_fn = descriptive_function,
                                                  names_of_missing = c("Mean (SD)", "Median (IQR)")
                            )
                          }),
                   htmlTable_args = list(
                     rgroup = levels(trial_2_missing_all$visit),
                     n.rgroup = rep(2, 4)
                   )
  )
  expect_identical(out, expected_missing_all)

  Hmisc::label(trial_2, self = FALSE) <- c("Study Visit", "Treatment Arm", "Outcome Measure")
  trial_2_missing_first <- trial_2[!((trial_2$visit == "randomisation") & (trial_2$arm == "control")), ]
  trial_2_missing_second <- trial_2[!((trial_2$visit == "randomisation") & (trial_2$arm == "standard treatment")), ]
  trial_2_missing_outer <- trial_2[!((trial_2$visit == "randomisation") & (trial_2$arm != "new treatment")), ]
  trial_2_missing_all <- trial_2[trial_2$visit != "week3", ]

  out <- mergeDesc(lapply(levels(trial_2$visit),
                          function(x) {
                            getDescriptionStatsBy(
                              x = trial_2$outcome[trial_2$visit == x],
                              by = trial_2$arm[trial_2$visit == x],
                              continuous_fn = descriptive_function
                            )
                          }),
                   htmlTable_args = list(
                     rgroup = levels(trial_2$visit),
                     n.rgroup = rep(2, 4)
                   )
  )
  expect_equivalent(out, expected_no_missing_2)

  out <- mergeDesc(lapply(levels(trial_2_missing_first$visit),
                          function(x) {
                            getDescriptionStatsBy(
                              x = trial_2_missing_first$outcome[trial_2_missing_first$visit == x],
                              by = trial_2_missing_first$arm[trial_2_missing_first$visit == x],
                              continuous_fn = descriptive_function
                            )
                          }),
                   htmlTable_args = list(
                     rgroup = levels(trial_2_missing_first$visit),
                     n.rgroup = rep(2, 4)
                   )
  )
  expect_equivalent(out, expected_missing_first_2)

  out <- mergeDesc(lapply(levels(trial_2_missing_second$visit),
                          function(x) {
                            getDescriptionStatsBy(
                              x = trial_2_missing_second$outcome[trial_2_missing_second$visit == x],
                              by = trial_2_missing_second$arm[trial_2_missing_second$visit == x],
                              continuous_fn = descriptive_function,
                              names_of_missing = c("Mean (SD)", "Median (IQR)")
                            )
                          }),
                   htmlTable_args = list(
                     rgroup = levels(trial_2_missing_second$visit),
                     n.rgroup = rep(2, 4)
                   )
  )
  expect_equivalent(out, expected_missing_second_2)

  out <- mergeDesc(lapply(levels(trial_2_missing_outer$visit),
                          function(x) {
                            getDescriptionStatsBy(
                              x = trial_2_missing_outer$outcome[trial_2_missing_outer$visit == x],
                              by = trial_2_missing_outer$arm[trial_2_missing_outer$visit == x],
                              continuous_fn = descriptive_function,
                              names_of_missing = c("Mean (SD)", "Median (IQR)")
                            )
                          }),
                   htmlTable_args = list(
                     rgroup = levels(trial_2_missing_outer$visit),
                     n.rgroup = rep(2, 4)
                   )
  )
  expect_equivalent(out, expected_missing_outer)

  out <- mergeDesc(lapply(levels(trial_2_missing_all$visit),
                          function(x) {
                            getDescriptionStatsBy(trial_2_missing_all$outcome[trial_2_missing_all$visit == x],
                                                  trial_2_missing_all$arm[trial_2_missing_all$visit == x],
                                                  continuous_fn = descriptive_function,
                                                  names_of_missing = c("Mean (SD)", "Median (IQR)")
                            )
                          }),
                   htmlTable_args = list(
                     rgroup = levels(trial_2_missing_all$visit),
                     n.rgroup = rep(2, 4)
                   )
  )
  expect_equivalent(out, expected_missing_all)
})


test_that("Error when one category has no missing in it", {
  set.seed(1)
  aa <- factor(sample(c("A", "B"), size = 50, replace = TRUE))
  aa[sample(1:50, size = 5)] <- NA
  aaa <- factor(sample(1:3, size = 50, replace = TRUE))
  aa[aaa == 2 & is.na(aa)] <- "B"
  ret <-
    getDescriptionStatsBy(x = aa, by = aaa, add_total_col = TRUE)

  expect_match(ret["A", "2"], sprintf("^%d", table(aa, aaa)["A", "2"]),
               info = "The value does not seem to match the raw table"
  )
  expect_match(ret["B", "2"], sprintf("^%d", table(aa, aaa)["B", "2"]),
               info = "The value does not seem to match the raw table"
  )
  expect_match(ret["Missing", "2"], "^0")

  ret <-
    getDescriptionStatsBy(x = aa, by = aaa, useNA = "no", add_total_col = TRUE)
  expect_match(ret[1, "2"], sprintf("^%d", table(aa, aaa)["A", "2"]),
               info = "The value does not seem to match the raw table"
  )
  expect_equal(nrow(ret), 1)

  ret <-
    getDescriptionStatsBy(
      x = aa, by = aaa,
      add_total_col = TRUE,
      useNA = "no",
      prop_fn = describeFactors
    )
  expect_match(ret["A", "2"], sprintf("^%d", table(aa, aaa)["A", "2"]),
               info = "The value does not seem to match the raw table"
  )
  expect_match(ret["B", "2"], sprintf("^%d", table(aa, aaa)["B", "2"]),
               info = "The value does not seem to match the raw table"
  )

  cont <- rnorm(length(aa))
  cont[sample(1:50, size = 5)] <- NA
  cont[aaa == 2 & is.na(cont)] <- 0
  ret <- getDescriptionStatsBy(x = cont, by = aaa, useNA = "no", add_total_col = TRUE)
  expect_equal(nrow(ret), 1)

  aa <- factor(sample(LETTERS[1:3], size = 50, replace = TRUE))
  aa[sample(1:50, size = 5)] <- NA
  aaa <- factor(sample(1:2, size = 50, replace = TRUE))
  aa[aaa == 2] <- "B"
  ret <- getDescriptionStatsBy(x = aa, by = aaa, useNA = "no", add_total_col = TRUE)
  expect_match(ret["A", "2"], sprintf("^%d", table(aa, aaa)["A", "2"]),
               info = "The value does not seem to match the raw table"
  )
  expect_match(ret["B", "2"], sprintf("^%d", table(aa, aaa)["B", "2"]),
               info = "The value does not seem to match the raw table"
  )
})

test_that("Error when one continuous variable has no missing in it", {
  set.seed(1)
  aa <- runif(50)
  aa[sample(1:50, size = 5)] <- NA
  aaa <- factor(sample(1:3, size = 50, replace = TRUE))
  aa[aaa == 2 & is.na(aa)] <- 1
  ret <-
    getDescriptionStatsBy(x = aa, by = aaa, html = TRUE)

  expect_match(
    ret["Missing", "2"],
    sprintf("^%d", sum(is.na(aa[aaa == 2])))
  )
})

test_that("Error when a factor variable has an empty level", {
  set.seed(1)
  variable <- factor(sample(LETTERS[1:2], size = 50, replace = TRUE),
                     levels = LETTERS[1:3]
  )
  variable[sample(1:50, size = 5)] <- NA
  by <- factor(sample(1:2, size = 50, replace = TRUE))
  ret <- getDescriptionStatsBy(x = variable, by = by, add_total_col = TRUE, useNA = "no")

  expect_match(
    ret["B", "2"],
    sprintf("^%d", sum(variable[by == 2] == "B", na.rm = TRUE))
  )
})
