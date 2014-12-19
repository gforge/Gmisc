library('testthat')
library('stringr')
library('Hmisc') # I need to include this for unknown reason or the test fails in R CMD check mode
context('getDescriptionStatsBy')

data("Loblolly")

set.seed(1)
Loblolly$young <- Loblolly$age < 10
Loblolly$young <- factor(Loblolly$young, label=c("Yes", "No"))
Loblolly$fvar <- factor(sample(letters[1:3], size=nrow(Loblolly), replace=TRUE))
Loblolly$young_w_missing <- Loblolly$young
Loblolly$young_w_missing[sample(1:nrow(Loblolly), size=4)] <- NA
Loblolly$fvar_w_missing <- Loblolly$fvar
Loblolly$fvar_w_missing[sample(1:nrow(Loblolly), size=4)] <- NA
Loblolly$height_w_missing <- Loblolly$height
Loblolly$height_w_missing[sample(1:nrow(Loblolly), size=4)] <- NA

test_that("Check mean function",
{
  stats <- by(Loblolly$height, Loblolly$young, mean)
  a <- getDescriptionStatsBy(Loblolly$height, Loblolly$young,
                             statistics=TRUE,
                             digits=2, statistics.sig_lim=10^-4)
  # Check that it contains the true mean
  expect_true(grepl(round(stats[["No"]], 2), a[1,"No"]),
              info="Expected the mean")
  expect_true(grepl(round(stats[["Yes"]], 2), a[1,"Yes"]),
              info="Expected the mean")

  # Check that it contains the sd
  stats <- by(Loblolly$height, Loblolly$young, sd)
  expect_true(grepl(round(stats[["No"]], 2), a[1,"No"]),
              info="Expected the sd")
  expect_true(grepl(round(stats[["Yes"]], 2), a[1,"Yes"]),
              info="Expected the sd")

  true_wilc_pv <- txtPval(wilcox.test(Loblolly$height ~ Loblolly$young)$p.value,
                          statistics.sig_lim=10^-4)
  expect_equal(as.character(a[1, "P-value"]),
               true_wilc_pv)

  # Check P-value without truncation
  a <- getDescriptionStatsBy(Loblolly$height, Loblolly$age == 10,
                             statistics=TRUE,
                             digits=2, statistics.sig_lim=10^-4)
  true_wilc_pv <- txtPval(wilcox.test(Loblolly$height ~ Loblolly$age == 10)$p.value,
                          statistics.sig_lim=10^-4)
  expect_equal(as.character(a[1, "P-value"]),
               true_wilc_pv)
})

test_that("Check median function",
{
  stats <- by(Loblolly$height, Loblolly$young, median)
  a <- getDescriptionStatsBy(Loblolly$height, Loblolly$young,
                             continuous_fn=describeMedian,
                             statistics=TRUE,
                             digits=2, statistics.sig_lim=10^-4)
  # Check that it contains the true mean
  expect_true(grepl(round(stats[["No"]], 2), a[1,"No"]),
              info="Expected the median")
  expect_true(grepl(round(stats[["Yes"]], 2), a[1,"Yes"]),
              info="Expected the median")

  # Check that it contains the sd
  stats <- by(Loblolly$height, Loblolly$young,
              function(x) str_trim(paste(format(quantile(x, probs=c(.25, .75)),
                                                digits=2,
                                                nsmall=2), collapse=" - ")))
  expect_true(grepl(stats[["No"]], a[1,"No"]),
              info="Expected the iqr range")
  expect_true(grepl(stats[["Yes"]], a[1,"Yes"]),
              info="Expected the iqr range")

  true_wilc_pv <- txtPval(wilcox.test(Loblolly$height ~ Loblolly$young)$p.value,
                                  statistics.sig_lim=10^-4)
  expect_equal(as.character(a[1, "P-value"]),
               true_wilc_pv)

  a <- getDescriptionStatsBy(Loblolly$height, Loblolly$young,
                             continuous_fn=function(...)
                               describeMedian(..., iqr = FALSE),
                             statistics=TRUE,
                             digits=2, statistics.sig_lim=10^-4)

  # Check that it contains the sd
  stats <- by(Loblolly$height, Loblolly$young,
              function(x) paste(round(range(x), 2), collapse=" - "))
  expect_true(grepl(stats[["No"]], a[1,"No"]),
              info="Expected the range")
  expect_true(grepl(stats[["Yes"]], a[1,"Yes"]),
              info="Expected the range")
})

test_that("Check factor function",
{
  stats <- table(Loblolly$fvar, Loblolly$young)
  a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young,
                             continuous_fn=describeMedian,
                             statistics=TRUE,
                             digits=2, statistics.sig_lim=10^-4)
  # Check that it contains the true mean
  for (rn in rownames(a)){
    for (cn in levels(Loblolly$young))
      expect_match(a[rn, cn], as.character(stats[rn, cn]),
                   info="Factor count don't match")
  }

  vertical_perc_stats <- format(apply(stats, 2, function(x){
    x/sum(x)*100
  }), nsmall=2, digits=2)
  horizontal_perc_stats <- t(format(apply(stats, 1, function(x){
    x/sum(x)*100
  }), nsmall=2, digits=2))
  for (rn in rownames(a)){
    for (cn in levels(Loblolly$young))
      expect_match(a[rn, cn], sprintf("%s%%", vertical_perc_stats[rn, cn]),
                  info="Factor percentagess don't match in vertical mode")
  }

  a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young, hrzl_prop=TRUE,
                             continuous_fn=describeMedian,
                             statistics=TRUE,
                             digits=2, statistics.sig_lim=10^-4)
  for (rn in rownames(a)){
    for (cn in levels(Loblolly$young))
      expect_match(a[rn, cn], sprintf("%s%%", horizontal_perc_stats[rn, cn]),
                  info="Factor percentagess don't match in horizontal mode")
  }

  true_fisher_pval <- txtPval(fisher.test(Loblolly$fvar, Loblolly$young)$p.value,
                              statistics.sig_lim=10^-4)

  expect_equivalent(as.character(a[1, "P-value"]),
                    true_fisher_pval)

})

test_that("Check total column position",{
  a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young,
                             hrzl_prop=TRUE,add_total_col = TRUE,
                             continuous_fn=describeMedian,
                             statistics=TRUE,
                             digits=2, statistics.sig_lim=10^-4)
  expect_equivalent(colnames(a)[1], "Total")
  expect_equivalent(ncol(a), 4)

  a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young,
                             hrzl_prop=TRUE,
                             add_total_col = "last",
                             continuous_fn=describeMedian,
                             digits=2, statistics.sig_lim=10^-4)
  expect_equivalent(tail(colnames(a),1), "Total",
                    info="The last column without statistics should be the total column when the add_total_col is set to last")
  expect_equivalent(ncol(a), 3)

  a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young,
                             statistics = TRUE,
                             hrzl_prop=TRUE,
                             add_total_col = "last")
  expect_equivalent(tail(colnames(a),2)[1], "Total",
                    info = "The last should be the p-value if statistics is specified")
  expect_equivalent(ncol(a), 4)
})

test_that("Check factor function with missing",
{
  stats <- table(Loblolly$fvar, Loblolly$young_w_missing, useNA="ifany")
  expect_warning(a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young_w_missing,
                                            statistics=TRUE,
                                            digits=2, statistics.sig_lim=10^-4))

  for (rn in rownames(a)){
    for (cn in levels(Loblolly$young))
      expect_match(a[rn, cn], as.character(stats[rn, cn]),
                   info="Factor count don't match")
  }


  stats <- table(Loblolly$fvar, Loblolly$young_w_missing, useNA="no")
  vertical_perc_stats <-
    format(apply(stats, 2, function(x){
      x/sum(x)*100
    }), nsmall=2, digits=2)
  horizontal_perc_stats <-
    t(format(apply(stats, 1, function(x){
      x/sum(x)*100
    }), nsmall=2, digits=2))

  for (rn in rownames(a)){
    for (cn in levels(Loblolly$young))
      expect_match(a[rn, cn], sprintf("%s%%", vertical_perc_stats[rn, cn]),
                  info="Factor vertical percentages don't match")
  }

  suppressWarnings(a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young_w_missing,
                                              hrzl_prop=TRUE,
                                              statistics=TRUE,
                                              digits=2, statistics.sig_lim=10^-4))

  for (rn in rownames(a)){
    for (cn in levels(Loblolly$young))
      expect_match(a[rn, cn], sprintf("%s%%", horizontal_perc_stats[rn, cn]),
                  info="Factor percentages don't match in horizontal mode")
  }

  suppressWarnings(a <- getDescriptionStatsBy(Loblolly$fvar_w_missing, Loblolly$young_w_missing,
                                              useNA="no",
                                              digits=2, statistics.sig_lim=10^-4))
  stats <- table(Loblolly$fvar_w_missing, Loblolly$young_w_missing, useNA="no")
  vertical_perc_stats <-
    format(apply(stats, 2, function(x){
      x/sum(x)*100
    }), nsmall=2, digits=2)

  for (rn in rownames(a)){
    for (cn in levels(Loblolly$young)){
      expect_match(a[rn, cn], as.character(stats[rn, cn]),
                  info=sprintf("Factor '%s':'%s' count don't match",
                               rn, cn))
      expect_match(a[rn, cn], sprintf("%s%%", vertical_perc_stats[rn, cn]),
                   info=sprintf("Factor '%s':'%s' vertical percentages don't match",
                                rn, cn))
    }
  }

  suppressWarnings(a <- getDescriptionStatsBy(Loblolly$fvar_w_missing,
                                              Loblolly$young_w_missing,
                                              digits=2, statistics.sig_lim=10^-4))
  stats <- table(Loblolly$fvar_w_missing, Loblolly$young_w_missing, useNA="ifany")
  stats <- stats[,!is.na(colnames(stats))]
  rownames(stats)[is.na(rownames(stats))] <- "Missing"
  vertical_perc_stats <-
    format(apply(stats, 2, function(x){
      x/sum(x)*100
    }), nsmall=2, digits=2)
  for (rn in rownames(a)){
    for (cn in levels(Loblolly$young)){
      expect_match(a[rn, cn], as.character(stats[rn, cn]),
                   info=sprintf("Factor '%s':'%s' count don't match",
                                rn, cn))
      expect_match(a[rn, cn], sprintf("%s%%", str_trim(vertical_perc_stats[rn, cn])),
                   info=sprintf("Factor '%s':'%s' vertical percentages don't match",
                                rn, cn))
    }
  }

  suppressWarnings(a <- getDescriptionStatsBy(Loblolly$fvar_w_missing, Loblolly$young_w_missing, hrzl_prop = TRUE,
                             digits=2, statistics.sig_lim=10^-4))
  horizontal_perc_stats <-
    t(format(apply(stats, 1, function(x){
      x/sum(x)*100
    }), nsmall=2, digits=2))


  for (rn in rownames(a)){
    for (cn in levels(Loblolly$young)){
      expect_match(a[rn, cn], sprintf("%s%%", str_trim(horizontal_perc_stats[rn, cn])),
                  info="Factor vertical percentages don't match")
    }
  }

  
  # When
  # - `x` has exactly 2 levels and some NAs
  # - add_total_col=TRUE
  # - show_missing="no"
  # - show_all_values=F
  # Then prGetStatistics should return the count of just the first factor level
  # use example:
  #   a <- getDescriptionStatsBy(Loblolly$young_w_missing, Loblolly$fvar,
  #                              useNA="no", digits=2, 
  #                              add_total_col=TRUE)
  a <- prGetStatistics(Loblolly$young_w_missing, 
                       useNA ="no", show_all_values=FALSE)
  lvl = levels(Loblolly$young_w_missing)[1]
  target = sum(stats[, lvl])
  names(target) = lvl
  expect_equal(a, target)
})

test_that("Problem with boolean x", {
  set.seed(1)
  aa <- factor(sample(c("No", "Yes"), size = 50, replace = TRUE))
  aaa <- sample(c(TRUE, FALSE), size = 50, replace = TRUE)
  ret <- getDescriptionStatsBy(x = aaa, by=aa, numbers_first = TRUE)
  expect_equivalent(nrow(ret), 2,
                    info="There should only be one alternative returned")
  expect_equivalent(ncol(ret), 2,
                    info="There should be two columns")
  expect_match(ret["TRUE", "No"], sprintf("^%d", table(aaa, aa)["TRUE", "No"]),
               info="The value does not seem to match the raw table")
})


test_that("Error when one category has no missing in it", {
  set.seed(1)
  aa <- factor(sample(c("A", "B"), size = 50, replace = TRUE))
  aa[sample(1:50, size = 5)] <- NA
  aaa <- factor(sample(1:3, size = 50, replace = TRUE))
  aa[aaa == 2 & is.na(aa)] <- "B"
  ret <-
    getDescriptionStatsBy(x = aa, by=aaa, html=TRUE)

  expect_match(ret["A","2"], sprintf("^%d", table(aa, aaa)["A","2"]),
               info="The value does not seem to match the raw table")
  expect_match(ret["Missing","2"], "^0")
})

test_that("Error when one continuous variable has no missing in it", {
  set.seed(1)
  aa <- runif(50)
  aa[sample(1:50, size = 5)] <- NA
  aaa <- factor(sample(1:3, size = 50, replace = TRUE))
  aa[aaa == 2 & is.na(aa)] <- 1
  ret <-
    getDescriptionStatsBy(x = aa, by=aaa, html=TRUE)

  expect_match(ret["Missing","2"],
               sprintf("^%d", sum(is.na(aa[aaa == 2]))))
})
