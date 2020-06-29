library("testthat")
library("stringr")
# I need to include this for unknown reason or the test fails in R CMD check mode
context("getDescriptionStatsBy")

data("Loblolly")

set.seed(1)
Loblolly$young <- Loblolly$age < 10
Loblolly$young <- factor(Loblolly$young, label = c("Yes", "No"))
Loblolly$fvar <- factor(sample(letters[1:3], size = nrow(Loblolly), replace = TRUE))
Loblolly$young_w_missing <- Loblolly$young
Loblolly$young_w_missing[sample(1:nrow(Loblolly), size = 4)] <- NA
Loblolly$fvar_w_missing <- Loblolly$fvar
Loblolly$fvar_w_missing[sample(1:nrow(Loblolly), size = 4)] <- NA
Loblolly$height_w_missing <- Loblolly$height
Loblolly$height_w_missing[sample(1:nrow(Loblolly), size = 4)] <- NA

test_that("Check mean function", {
  stats <- by(Loblolly$height, Loblolly$young, mean)
  a <- getDescriptionStatsBy(Loblolly$height, Loblolly$young,
    statistics = TRUE,
    digits = 2, statistics.sig_lim = 10^-4
  )
  # Check that it contains the true mean
  expect_true(grepl(round(stats[["No"]], 2), a[1, "No"]),
    info = "Expected the mean"
  )
  expect_true(grepl(round(stats[["Yes"]], 2), a[1, "Yes"]),
    info = "Expected the mean"
  )

  # Check that it contains the sd
  stats <- by(Loblolly$height, Loblolly$young, sd)
  expect_true(grepl(round(stats[["No"]], 2), a[1, "No"]),
    info = "Expected the sd"
  )
  expect_true(grepl(round(stats[["Yes"]], 2), a[1, "Yes"]),
    info = "Expected the sd"
  )

  true_wilc_pv <- txtPval(wilcox.test(Loblolly$height ~ Loblolly$young)$p.value,
    statistics.sig_lim = 10^-4
  )
  expect_equal(
    as.character(a[1, "P-value"]),
    true_wilc_pv
  )

  # Check P-value without truncation
  a <- getDescriptionStatsBy(Loblolly$height, Loblolly$age == 10,
    statistics = TRUE,
    digits = 2, statistics.sig_lim = 10^-4
  )
  true_wilc_pv <- txtPval(wilcox.test(Loblolly$height ~ Loblolly$age == 10)$p.value,
    statistics.sig_lim = 10^-4
  )
  expect_equal(
    as.character(a[1, "P-value"]),
    true_wilc_pv
  )
})

test_that("Check median function", {
  stats <- by(Loblolly$height, Loblolly$young, median)
  a <- getDescriptionStatsBy(Loblolly$height, Loblolly$young,
    continuous_fn = describeMedian,
    statistics = TRUE,
    digits = 2, statistics.sig_lim = 10^-4
  )
  # Check that it contains the true mean
  expect_true(grepl(round(stats[["No"]], 2), a[1, "No"]),
    info = "Expected the median"
  )
  expect_true(grepl(round(stats[["Yes"]], 2), a[1, "Yes"]),
    info = "Expected the median"
  )

  # Check that it contains the sd
  stats <- by(
    Loblolly$height, Loblolly$young,
    function(x) {
      str_trim(paste(format(quantile(x, probs = c(.25, .75)),
        digits = 2,
        nsmall = 2
      ), collapse = " - "))
    }
  )
  expect_true(grepl(stats[["No"]], a[1, "No"]),
    info = "Expected the iqr range"
  )
  expect_true(grepl(stats[["Yes"]], a[1, "Yes"]),
    info = "Expected the iqr range"
  )

  true_wilc_pv <- txtPval(wilcox.test(Loblolly$height ~ Loblolly$young)$p.value,
    statistics.sig_lim = 10^-4
  )
  expect_equal(
    as.character(a[1, "P-value"]),
    true_wilc_pv
  )

  a <- getDescriptionStatsBy(Loblolly$height, Loblolly$young,
    continuous_fn = function(...) {
      describeMedian(..., iqr = FALSE)
    },
    statistics = TRUE,
    digits = 2, statistics.sig_lim = 10^-4
  )

  # Check that it contains the sd
  stats <- by(
    Loblolly$height, Loblolly$young,
    function(x) paste(round(range(x), 2), collapse = " - ")
  )
  expect_true(grepl(stats[["No"]], a[1, "No"]),
    info = "Expected the range"
  )
  expect_true(grepl(stats[["Yes"]], a[1, "Yes"]),
    info = "Expected the range"
  )
})

test_that("Check small proportions", {
  n <- 1e4
  a <- getDescriptionStatsBy(LETTERS[c(rep(1, times = (n - 4)), rep(2, times = 4))],
    letters[c(rep(1, times = (n - 10)), rep(2, times = 10))],
    hrzl_prop = TRUE,
    statistics = TRUE,
    digits = 0,
    digits.nonzero = 2,
    statistics.sig_lim = 10^-4
  )
  expect_equivalent(as.character(a["A", "b"]), "6 (0.06%)")
  a <- getDescriptionStatsBy(LETTERS[c(rep(1, times = (n - 4)), rep(2, times = 4))],
    letters[c(rep(1, times = (n - 10)), rep(2, times = 10))],
    hrzl_prop = TRUE,
    statistics = TRUE,
    digits = 0,
    statistics.sig_lim = 10^-4
  )
  expect_equivalent(as.character(a["A", "b"]), "6 (0%)")
})

test_that("Check factor function", {
  stats <- table(Loblolly$fvar, Loblolly$young)
  a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young,
    continuous_fn = describeMedian,
    statistics = TRUE,
    digits = 2, statistics.sig_lim = 10^-4
  )
  # Check that it contains the true mean
  for (rn in rownames(a)) {
    for (cn in levels(Loblolly$young)) {
      expect_match(a[rn, cn], as.character(stats[rn, cn]),
        info = "Factor count don't match"
      )
    }
  }

  vertical_perc_stats <- format(apply(stats, 2, function(x) {
    x / sum(x) * 100
  }), nsmall = 2, digits = 2)
  horizontal_perc_stats <- t(format(apply(stats, 1, function(x) {
    x / sum(x) * 100
  }), nsmall = 2, digits = 2))
  for (rn in rownames(a)) {
    for (cn in levels(Loblolly$young)) {
      expect_match(a[rn, cn], sprintf("%s%%", vertical_perc_stats[rn, cn]),
        info = "Factor percentagess don't match in vertical mode"
      )
    }
  }

  a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young,
    hrzl_prop = TRUE,
    continuous_fn = describeMedian,
    statistics = TRUE,
    digits = 2, statistics.sig_lim = 10^-4
  )
  for (rn in rownames(a)) {
    for (cn in levels(Loblolly$young)) {
      expect_match(a[rn, cn], sprintf("%s%%", horizontal_perc_stats[rn, cn]),
        info = "Factor percentagess don't match in horizontal mode"
      )
    }
  }
})

test_that("Statistics work as expected", {
  a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young,
    hrzl_prop = TRUE,
    continuous_fn = describeMedian,
    statistics = TRUE,
    digits = 2, statistics.sig_lim = 10^-4
  )

  true_fisher_pval <- txtPval(fisher.test(Loblolly$fvar, Loblolly$young)$p.value,
    statistics.sig_lim = 10^-4
  )

  expect_equivalent(
    as.character(a[1, "P-value"]),
    true_fisher_pval
  )

  a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young,
    hrzl_prop = TRUE,
    continuous_fn = describeMedian,
    statistics = list("factor" = getPvalFisher),
    digits = 2, statistics.sig_lim = 10^-4
  )

  expect_equivalent(
    as.character(a[1, "P-value"]),
    true_fisher_pval
  )

  expect_error(getDescriptionStatsBy(Loblolly$fvar, Loblolly$young,
    hrzl_prop = TRUE,
    continuous_fn = describeMedian,
    statistics = list("factor" = getNonExistentPvalueFunction),
    digits = 2, statistics.sig_lim = 10^-4
  ))

  a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young,
    hrzl_prop = TRUE,
    continuous_fn = describeMedian,
    statistics = function(x, by) {
      return(.2)
    },
    digits = 2, statistics.sig_lim = 10^-4
  )

  expect_equivalent(
    as.character(a[1, "P-value"]),
    "0.20", "Custom p-value function problem"
  )

  a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young,
    hrzl_prop = TRUE,
    continuous_fn = describeMedian,
    statistics = function(x, by) {
      a <- "test"
      attr(a, "colname") <- "test"
      return(a)
    },
    digits = 2, statistics.sig_lim = 10^-4
  )

  expect_equivalent(
    as.character(a[1, "test"]),
    "test", "Errror when adding a string p-value alternative"
  )

  expect_error(getDescriptionStatsBy(Loblolly$fvar, Loblolly$young,
    hrzl_prop = TRUE,
    continuous_fn = describeMedian,
    statistics = function(x, by) {
      a <- "test"
      return(a)
    },
    digits = 2, statistics.sig_lim = 10^-4
  ),
  info = "The colname attribute must be present"
  )
})

test_that("Check total column position", {
  a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young,
    hrzl_prop = TRUE, add_total_col = TRUE,
    continuous_fn = describeMedian,
    statistics = TRUE,
    digits = 2, statistics.sig_lim = 10^-4
  )
  expect_equivalent(colnames(a)[1], "Total")
  expect_equivalent(ncol(a), 4)

  a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young,
    hrzl_prop = TRUE,
    add_total_col = "last",
    continuous_fn = describeMedian,
    digits = 2, statistics.sig_lim = 10^-4
  )
  expect_equivalent(tail(colnames(a), 1), "Total",
    info = "The last column without statistics should be the total column when the add_total_col is set to last"
  )
  expect_equivalent(ncol(a), 3)

  a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young,
    statistics = TRUE,
    hrzl_prop = TRUE,
    add_total_col = "last"
  )
  expect_equivalent(tail(colnames(a), 2)[1], "Total",
    info = "The last should be the p-value if statistics is specified"
  )
  expect_equivalent(ncol(a), 4)
})

test_that("Check factor function with missing", {
  stats <- table(Loblolly$fvar, Loblolly$young_w_missing, useNA = "ifany")
  expect_warning(a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young_w_missing,
    statistics = TRUE,
    digits = 2, statistics.sig_lim = 10^-4
  ))

  for (rn in rownames(a)) {
    for (cn in levels(Loblolly$young)) {
      expect_match(a[rn, cn], as.character(stats[rn, cn]),
        info = "Factor count don't match"
      )
    }
  }


  stats <- table(Loblolly$fvar, Loblolly$young_w_missing, useNA = "no")
  vertical_perc_stats <-
    format(apply(stats, 2, function(x) {
      x / sum(x) * 100
    }), nsmall = 2, digits = 2)
  horizontal_perc_stats <-
    t(format(apply(stats, 1, function(x) {
      x / sum(x) * 100
    }), nsmall = 2, digits = 2))

  for (rn in rownames(a)) {
    for (cn in levels(Loblolly$young)) {
      expect_match(a[rn, cn], sprintf("%s%%", vertical_perc_stats[rn, cn]),
        info = "Factor vertical percentages don't match"
      )
    }
  }

  suppressWarnings(a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young_w_missing,
    hrzl_prop = TRUE,
    statistics = TRUE,
    digits = 2, statistics.sig_lim = 10^-4
  ))

  for (rn in rownames(a)) {
    for (cn in levels(Loblolly$young)) {
      expect_match(a[rn, cn], sprintf("%s%%", horizontal_perc_stats[rn, cn]),
        info = "Factor percentages don't match in horizontal mode"
      )
    }
  }

  suppressWarnings(a <- getDescriptionStatsBy(Loblolly$fvar_w_missing, Loblolly$young_w_missing,
    useNA = "no",
    digits = 2, statistics.sig_lim = 10^-4
  ))
  stats <- table(Loblolly$fvar_w_missing, Loblolly$young_w_missing, useNA = "no")
  vertical_perc_stats <-
    format(apply(stats, 2, function(x) {
      x / sum(x) * 100
    }), nsmall = 2, digits = 2)

  for (rn in rownames(a)) {
    for (cn in levels(Loblolly$young)) {
      expect_match(a[rn, cn], as.character(stats[rn, cn]),
        info = sprintf(
          "Factor '%s':'%s' count don't match",
          rn, cn
        )
      )
      expect_match(a[rn, cn], sprintf("%s%%", vertical_perc_stats[rn, cn]),
        info = sprintf(
          "Factor '%s':'%s' vertical percentages don't match",
          rn, cn
        )
      )
    }
  }

  suppressWarnings(a <- getDescriptionStatsBy(Loblolly$fvar_w_missing,
    Loblolly$young_w_missing,
    digits = 2, statistics.sig_lim = 10^-4
  ))
  stats <- table(Loblolly$fvar_w_missing, Loblolly$young_w_missing, useNA = "ifany")
  stats <- stats[, !is.na(colnames(stats))]
  rownames(stats)[is.na(rownames(stats))] <- "Missing"
  vertical_perc_stats <-
    format(apply(stats, 2, function(x) {
      x / sum(x) * 100
    }), nsmall = 2, digits = 2)
  for (rn in rownames(a)) {
    for (cn in levels(Loblolly$young)) {
      expect_match(a[rn, cn], as.character(stats[rn, cn]),
        info = sprintf(
          "Factor '%s':'%s' count don't match",
          rn, cn
        )
      )
      expect_match(a[rn, cn], sprintf("%s%%", str_trim(vertical_perc_stats[rn, cn])),
        info = sprintf(
          "Factor '%s':'%s' vertical percentages don't match",
          rn, cn
        )
      )
    }
  }

  suppressWarnings(a <- getDescriptionStatsBy(Loblolly$fvar_w_missing, Loblolly$young_w_missing,
    hrzl_prop = TRUE,
    digits = 2, statistics.sig_lim = 10^-4
  ))
  horizontal_perc_stats <-
    t(format(apply(stats, 1, function(x) {
      x / sum(x) * 100
    }), nsmall = 2, digits = 2))


  for (rn in rownames(a)) {
    for (cn in levels(Loblolly$young)) {
      expect_match(a[rn, cn], sprintf("%s%%", str_trim(horizontal_perc_stats[rn, cn])),
        info = "Factor vertical percentages don't match"
      )
    }
  }

  # When
  # - `x` has exactly 2 levels and some NAs
  # - add_total_col = TRUE
  # - show_missing = "no"
  # - show_all_values = FALSE
  # Then prGetStatistics should return the count of just the first factor level
  # use example:
  #   a <- getDescriptionStatsBy(Loblolly$young_w_missing, Loblolly$fvar,
  #                              useNA = "no", digits = 2,
  #                              add_total_col = TRUE)
  a <- prGetStatistics(Loblolly$young_w_missing,
    useNA = "no", show_all_values = FALSE
  )
  lvl <- levels(Loblolly$young_w_missing)[1]
  target <- sum(stats[, lvl])
  names(target) <- lvl
  expect_equal(a, target)
})

test_that("Check txtInt application", {
  # Check for factors
  set.seed(10)
  test_var <- factor(sample(1:3, size = 10^4, replace = TRUE))
  out <- prGetStatistics(test_var,
    useNA = "no", show_all_values = FALSE
  )
  expect_true(any(grepl(",", out)))
})

test_that("Problem with boolean x", {
  set.seed(1)
  aa <- factor(sample(c("No", "Yes"), size = 50, replace = TRUE))
  aaa <- sample(c(TRUE, FALSE), size = 50, replace = TRUE)
  ret <- getDescriptionStatsBy(x = aaa, by = aa, numbers_first = TRUE)
  expect_equivalent(nrow(ret), 1,
    info = "There should only be one alternative returned"
  )
  expect_equivalent(ncol(ret), 2,
    info = "There should be two columns"
  )
  expect_match(ret[TRUE, "No"], sprintf("^%d", table(aaa, aa)["TRUE", "No"]),
    info = "The value does not seem to match the raw table"
  )
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
  ret <-
    getDescriptionStatsBy(x = cont, by = aaa, useNA = "no", add_total_col = TRUE)
  expect_equal(nrow(ret), 1)

  aa <- factor(sample(LETTERS[1:3], size = 50, replace = TRUE))
  aa[sample(1:50, size = 5)] <- NA
  aaa <- factor(sample(1:2, size = 50, replace = TRUE))
  aa[aaa == 2] <- "B"
  ret <-
    getDescriptionStatsBy(x = aa, by = aaa, useNA = "no", add_total_col = TRUE)
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
  ret <-
    getDescriptionStatsBy(x = variable, by = by, add_total_col = TRUE, useNA = "no")

  expect_match(
    ret["B", "2"],
    sprintf("^%d", sum(variable[by == 2] == "B", na.rm = TRUE))
  )
})


test_that("test header", {
  data(mtcars)

  mtcars$am <- factor(mtcars$am, levels = 0:1, labels = c("Automatic", "Manual"))
  Hmisc::label(mtcars$am) <- "Transmission"
  set.seed(666)
  mtcars$col <- factor(sample(c("red", "black", "silver"),
    size = NROW(mtcars), replace = TRUE
  ))

  out <- getDescriptionStatsBy(
    x = mtcars$col,
    by = mtcars$am,
    header_count = TRUE,
    add_total_col = TRUE,
    statistics = TRUE
  )
  expect_match(
    colnames(out)[1],
    sprintf("No. %d", nrow(mtcars))
  )
  expect_match(
    colnames(out)[2],
    sprintf(
      "No. %d",
      sum(mtcars$am == levels(mtcars$am)[1])
    )
  )
  expect_match(
    colnames(out)[3],
    sprintf(
      "No. %d",
      sum(mtcars$am != levels(mtcars$am)[1])
    )
  )


  out <- getDescriptionStatsBy(
    x = mtcars$col,
    by = mtcars$am,
    header_count = "(n = %s)", # custom header
    add_total_col = TRUE,
    statistics = TRUE
  )

  expect_match(
    colnames(out)[1],
    sprintf("\\(n = %d\\)", nrow(mtcars))
  )
  expect_match(
    colnames(out)[2],
    sprintf(
      "\\(n = %d\\)",
      sum(mtcars$am == levels(mtcars$am)[1])
    )
  )
  expect_match(
    colnames(out)[3],
    sprintf(
      "\\(n = %d\\)",
      sum(mtcars$am != levels(mtcars$am)[1])
    )
  )
})


test_that("Test use_units", {
  data(mtcars)

  mtcars$am <- factor(mtcars$am, levels = 0:1, labels = c("Automatic", "Manual"))
  Hmisc::label(mtcars$am) <- "Transmission"
  set.seed(666)
  units(mtcars$mpg) <- "mpg"

  out1 <- suppressWarnings(
    getDescriptionStatsBy(
      x = mtcars$mpg,
      by = mtcars$am,
      header_count = TRUE,
      add_total_col = TRUE,
      statistics = TRUE
    )
  )

  out2 <- suppressWarnings(
    getDescriptionStatsBy(
      x = mtcars$mpg,
      by = mtcars$am,
      use_units = TRUE,
      header_count = TRUE,
      add_total_col = TRUE,
      statistics = TRUE
    )
  )

  expect_equal(ncol(out1) + 1, ncol(out2))

  out3 <- suppressWarnings(
    getDescriptionStatsBy(
      x = mtcars$mpg,
      by = mtcars$am,
      use_units = "name",
      header_count = TRUE,
      add_total_col = TRUE,
      statistics = TRUE
    )
  )
  expect_match(Hmisc::label(out3), "\\(mpg\\)")
  expect_false(grepl("\\(mpg\\)", label(out2)))
})

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

  out <- mergeDesc(lapply(levels(trial$visit), function(x) {
    getDescriptionStatsBy(
      x = trial$outcome[trial$visit == x],
      by = trial$arm[trial$visit == x],
      continuous_fn = descriptive_function
    )
  }),
  htmlTable_args = list(
    rgroup = levels(trial$visit),
    n.rgroup = rep(2, 4)
  )
  )
  expect_identical(out, expected_no_missing)

  out <- mergeDesc(lapply(levels(trial_missing_first$visit), function(x) {
    getDescriptionStatsBy(
      x = trial_missing_first$outcome[trial_missing_first$visit == x],
      by = trial_missing_first$arm[trial_missing_first$visit == x],
      continuous_fn = descriptive_function
    )
  }),
  htmlTable_args = list(
    rgroup = levels(trial_missing_first$visit),
    n.rgroup = rep(2, 4)
  )
  )
  expect_identical(out, expected_missing_first)

  out <- mergeDesc(lapply(levels(trial_missing_second$visit), function(x) {
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

  out <- mergeDesc(lapply(levels(trial_missing_both$visit), function(x) {
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

  out <- mergeDesc(lapply(levels(trial_2$visit), function(x) {
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

  out <- mergeDesc(lapply(levels(trial_2_missing_first$visit), function(x) {
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

  out <- mergeDesc(lapply(levels(trial_2_missing_second$visit), function(x) {
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

  out <- mergeDesc(lapply(levels(trial_2_missing_outer$visit), function(x) {
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

  out <- mergeDesc(lapply(levels(trial_2_missing_all$visit), function(x) {
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

  out <- mergeDesc(lapply(levels(trial_2$visit), function(x) {
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

  out <- mergeDesc(lapply(levels(trial_2_missing_first$visit), function(x) {
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

  out <- mergeDesc(lapply(levels(trial_2_missing_second$visit), function(x) {
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

  out <- mergeDesc(lapply(levels(trial_2_missing_outer$visit), function(x) {
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

  out <- mergeDesc(lapply(levels(trial_2_missing_all$visit), function(x) {
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

data("mtcars")
test_that("Non-factor variables where values are missing in only one of the by-groups", {
  table(as.character(mtcars$am), mtcars$gear)
  retAll <- getDescriptionStatsBy(
    as.character(mtcars$am), mtcars$gear,
    show_all_values = TRUE
  )
  retOne <- getDescriptionStatsBy(
    as.character(mtcars$am), mtcars$gear,
    show_all_values = FALSE
  )

  retAllRowDefault <- retAll["0", ]
  retAllRowOther <- retAll["1", ]
  # Delete label as the
  attributes(retAllRowDefault) <- NULL
  attributes(retAllRowOther) <- NULL
  attributes(retOne) <- NULL
  expect_equal(retAllRowDefault, retOne)
  expect_false(all(retAllRowOther == retOne))
})

### checks for issue #32: display of p-values for multi-row summaries
cars_missing <- mtcars
cars_missing$mpg[3] <- NA
test_that("p-values are displayed in multi-row summaries when rgroup and n.rgroup are specified", {
  expected <- structure(
    c(
      "27.1 (&plusmn;4.6)", "1 (9.1%)", "19.7 (&plusmn;1.5)",
      "0 (0%)", "15.1 (&plusmn;2.6)", "0 (0%)", "", ""
    ),
    .Dim = c(2L, 4L),
    .Dimnames = list(
      c("Mean (SD)", "Missing"),
      c("4", "6", "8", "P-value")
    ),
    rgroup = structure("Gas",
      add = structure(list(`1` = "&lt; 0.0001"),
        .Names = "1"
      )
    ),
    n.rgroup = 2,
    htmlTable_args = structure(list(), .Names = character(0)),
    class = c("descMrg", class(matrix(1)))
  )

  out <- mergeDesc(getDescriptionStatsBy(
    x = cars_missing$mpg,
    by = cars_missing$cyl,
    statistics = TRUE
  ),
  htmlTable_args = list(rgroup = c("Gas"), n.rgroup = 2)
  )
  expect_equivalent(out, expected)
})

test_that("p-vlues are displayed in multi-row summaries when rgroup and n.rgroup are not specified", {
  expected <- structure(
    c(
      "27.1 (&plusmn;4.6)", "1 (9.1%)", "19.7 (&plusmn;1.5)",
      "0 (0%)", "15.1 (&plusmn;2.6)", "0 (0%)", "", ""
    ),
    .Dim = c(2L, 4L),
    .Dimnames = list(
      c("Mean (SD)", "Missing"),
      c("4", "6", "8", "P-value")
    ),
    rgroup = structure("cars_missing$mpg",
      add = structure(list(`1` = "&lt; 0.0001"),
        .Names = "1"
      )
    ),
    n.rgroup = 2L,
    class = c("descMrg", class(matrix(1)))
  )

  out <- mergeDesc(getDescriptionStatsBy(
    x = cars_missing$mpg,
    by = cars_missing$cyl,
    statistics = TRUE
  ))
  expect_equivalent(out, expected)
})

test_that("p-values are displayed in the rgroup title for both multi- and one-row summaries when rgroup and n.rgroup are specified", {
  expected <- structure(
    c(
      "27.1 (&plusmn;4.6)", "1 (9.1%)", "105.1 (&plusmn;26.9)",
      "19.7 (&plusmn;1.5)", "0 (0%)", "183.3 (&plusmn;41.6)",
      "15.1 (&plusmn;2.6)", "0 (0%)", "353.1 (&plusmn;67.8)", "", "", ""
    ),
    .Dim = 3:4,
    .Dimnames = list(
      c("Mean (SD)", "Missing", "cars_missing$disp"),
      c("4", "6", "8", "P-value")
    ),
    rgroup = structure(c("Gas", "Displacement"),
      add = list("&lt; 0.0001", "&lt; 0.0001")
    ),
    n.rgroup = c(2, 1),
    htmlTable_args = structure(list(), .Names = character(0)),
    class = c("descMrg", class(matrix(1)))
  )

  out <- mergeDesc(getDescriptionStatsBy(
    x = cars_missing$mpg,
    by = cars_missing$cyl,
    statistics = TRUE
  ),
  getDescriptionStatsBy(
    x = cars_missing$disp,
    by = cars_missing$cyl,
    statistics = TRUE
  ),
  htmlTable_args = list(
    rgroup = c("Gas", "Displacement"),
    n.rgroup = c(2, 1)
  )
  )
  expect_equivalent(out, expected)
})

test_that("p-values are displayed for both multi- and one-row summaries when rgroup and n.rgroup are not specified", {
  expected <- structure(
    c(
      "27.1 (&plusmn;4.6)", "1 (9.1%)", "105.1 (&plusmn;26.9)",
      "19.7 (&plusmn;1.5)", "0 (0%)", "183.3 (&plusmn;41.6)",
      "15.1 (&plusmn;2.6)", "0 (0%)", "353.1 (&plusmn;67.8)",
      "", "", "&lt; 0.0001"
    ),
    .Dim = 3:4,
    .Dimnames = list(
      c("Mean (SD)", "Missing", "cars_missing$disp"),
      c("4", "6", "8", "P-value")
    ),
    rgroup = structure(c("cars_missing$mpg", ""),
      add = structure(list(`1` = "&lt; 0.0001"), .Names = "1")
    ),
    n.rgroup = c(2, 1),
    class = c("descMrg", class(matrix(1)))
  )

  out <- mergeDesc(
    getDescriptionStatsBy(
      x = cars_missing$mpg,
      by = cars_missing$cyl,
      statistics = TRUE
    ),
    getDescriptionStatsBy(
      x = cars_missing$disp,
      by = cars_missing$cyl,
      statistics = TRUE
    )
  )
  expect_equivalent(out, expected)
})
