library("testthat")
library("stringr")


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

data("mtcars")

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

  out <- mergeDesc(getDescriptionStatsBy(x = cars_missing$mpg,
                                         by = cars_missing$cyl,
                                         statistics = TRUE),
                   getDescriptionStatsBy(x = cars_missing$disp,
                                         by = cars_missing$cyl,
                                         statistics = TRUE),
                   htmlTable_args = list(rgroup = c("Gas", "Displacement"),
                                         n.rgroup = c(2, 1)))
  expect_equivalent(out, expected)
})


test_that("p-values are displayed for both multi- and one-row summaries when rgroup and n.rgroup are not specified", {
  expected <- structure(
    c(
      "27.1 (&plusmn;4.6)", "1 (9.1%)", "105.1 (&plusmn;26.9)",
      "19.7 (&plusmn;1.5)", "0 (0%)", "183.3 (&plusmn;41.6)",
      "15.1 (&plusmn;2.6)", "0 (0%)", "353.1 (&plusmn;67.8)",
      "", "", "&lt; 0.0001"),
    .Dim = 3:4,
    .Dimnames = list(c("Mean (SD)", "Missing", "cars_missing$disp"),
                     c("4", "6", "8", "P-value")),
    rgroup = structure(c("cars_missing$mpg", ""),
                       add = structure(list(`1` = "&lt; 0.0001"), .Names = "1")),
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
