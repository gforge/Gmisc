library('testthat')
library('stringr')
library('Hmisc') # I need to include this for unknown reason or the test fails in R CMD check mode
context('descriptionStats')

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

test_that("Check describeMean",{
  expect_match(describeMean(Loblolly$height, html=TRUE, digits=1), 
               sprintf("^%.1f", mean(Loblolly$height)))
  expect_match(describeMean(Loblolly$height, html=TRUE, digits=2), 
               sprintf("^%.2f", mean(Loblolly$height)))
  expect_match(describeMean(Loblolly$height_w_missing, html=TRUE, digits=2), 
               sprintf("^%.2f", mean(Loblolly$height_w_missing, na.rm=TRUE)))
  
  dm <- describeMean(Loblolly$height_w_missing, html=TRUE, digits=2, show_missing="ifany")
  expect_match(dm[1], 
               sprintf("^%.2f", mean(Loblolly$height_w_missing, na.rm=TRUE)))
  expect_match(dm[2], 
               sprintf("^%d", 
                       sum(is.na(Loblolly$height_w_missing))))
  expect_match(dm[2], 
               sprintf("\\(%.2f %%\\)", 
                       mean(is.na(Loblolly$height_w_missing))*100))
  
  dm <- describeMean(Loblolly$height_w_missing, html=TRUE, 
                     digits=2, show_missing="ifany", show_missing_digits=1)
  expect_match(dm[1], 
               sprintf("^%.2f", mean(Loblolly$height_w_missing, na.rm=TRUE)))
  expect_match(dm[2], 
               sprintf("^%d", 
                       sum(is.na(Loblolly$height_w_missing))))
  expect_match(dm[2], 
               sprintf("\\(%.1f %%\\)", 
                       mean(is.na(Loblolly$height_w_missing))*100))
})

test_that("Check describeMedian",{
  expect_match(describeMedian(Loblolly$height, html=TRUE, digits=1), 
               sprintf("^%.1f", median(Loblolly$height)))
  expect_match(describeMedian(Loblolly$height, html=TRUE, digits=2), 
               sprintf("^%.2f", median(Loblolly$height)))
  expect_match(describeMedian(Loblolly$height_w_missing, html=TRUE, digits=2), 
               sprintf("^%.2f", median(Loblolly$height_w_missing, na.rm=TRUE)))
  
  dm <- describeMedian(Loblolly$height_w_missing, html=TRUE, 
                       digits=2, show_missing="ifany")
  expect_match(dm[1], 
               sprintf("^%.2f", median(Loblolly$height_w_missing, na.rm=TRUE)))
  expect_match(dm[2], 
               sprintf("^%d", 
                       sum(is.na(Loblolly$height_w_missing))))
  expect_match(dm[2], 
               sprintf("\\(%.2f %%\\)", 
                       mean(is.na(Loblolly$height_w_missing))*100))
  
  dm <- describeMedian(Loblolly$height_w_missing, html=TRUE, 
                       digits=2, show_missing="ifany", show_missing_digits=1)
  expect_match(dm[1], 
               sprintf("^%.2f", median(Loblolly$height_w_missing, na.rm=TRUE)))
  expect_match(dm[2], 
               sprintf("^%d", 
                       sum(is.na(Loblolly$height_w_missing))))
  expect_match(dm[2], 
               sprintf("\\(%.1f %%\\)", 
                       mean(is.na(Loblolly$height_w_missing))*100))
  
})

test_that("Check describeFactors",{
  d_f <- describeFactors(Loblolly$fvar, html=TRUE, digits=1)
  t_f <- table(Loblolly$fvar)
  for (n in names(t_f)){
    expect_match(d_f[n,], 
                 sprintf("^%d", t_f[n]))
    expect_match(d_f[n,], 
                 sprintf("\\(%.1f %%\\)", prop.table(t_f)[n]*100))
  }
  
  d_f <- describeFactors(Loblolly$fvar, html=TRUE, digits=1, number_first=FALSE)
  for (n in names(t_f)){
    expect_match(d_f[n,], 
                 sprintf("\\(%d\\)", t_f[n]))
    expect_match(d_f[n,], 
                 sprintf("^%.1f %%", prop.table(t_f)[n]*100))
  }

  d_f <- describeFactors(Loblolly$fvar_w_missing, html=TRUE, digits=1,
                         show_missing="ifany")
  t_f <- table(Loblolly$fvar_w_missing, useNA="ifany")
  for (n in names(t_f)){
    if (is.na(n)){
      row_no_df <- which(rownames(d_f) == "Missing")
      row_no_tf <- which(is.na(names(t_f)))
    }else{
      row_no_df <- which(rownames(d_f) == n)
      row_no_tf <- which(names(t_f) == n)
    }
    expect_match(d_f[row_no_df,], 
                 sprintf("^%d", t_f[row_no_tf]))
    expect_match(d_f[row_no_df,], 
                 sprintf("\\(%.1f %%\\)", prop.table(t_f)[row_no_tf]*100))
  }
  
  d_f <- describeFactors(Loblolly$fvar_w_missing, html=TRUE, digits=2,
                         show_missing_digits=1,
                         show_missing="ifany")
  t_f <- table(Loblolly$fvar_w_missing, useNA="ifany")
  for (n in names(t_f)){
    if (is.na(n)){
      row_no_df <- which(rownames(d_f) == "Missing")
      row_no_tf <- which(is.na(names(t_f)))
      prop <- sprintf("%.1f", prop.table(t_f)[is.na(names(t_f))]*100)
    }else{
      row_no_df <- which(rownames(d_f) == n)
      row_no_tf <- which(names(t_f) == n)
      prop <- sprintf("%.2f", prop.table(t_f)[which(names(t_f) == n)]*100)
    }
    expect_match(d_f[row_no_df,], 
                 sprintf("^%d", t_f[row_no_tf]))
    expect_match(d_f[row_no_df,], 
                 sprintf("\\(%s %%\\)", prop))
  }
})

test_that("Check describeProporions",{
  d_f <- describeProp(Loblolly$young, html=TRUE, digits=1)
  expect_equal(length(d_f), 1)
  
  t_f <- table(Loblolly$young)
  expect_match(d_f, 
               sprintf("^%d", t_f["Yes"]))
  expect_match(d_f, 
               sprintf("\\(%.1f %%\\)", prop.table(t_f)["Yes"]*100))

  # Should default to factor function if there are misssing
  d_f <- describeProp(Loblolly$young_w_missing, html=TRUE, digits=1, show_missing="ifany")
  expect_equal(length(d_f), 3)
}

test_that("Check mean function", 
{ 
  stats <- by(Loblolly$height, Loblolly$young, mean)
  a <- getDescriptionStatsBy(Loblolly$height, Loblolly$young, 
                             statistics=TRUE,
                             html=TRUE, digits=2, sig.limit=10^-4)
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
  
  true_wilc_pv <- pvalueFormatter(wilcox.test(Loblolly$height ~ Loblolly$young)$p.value,
                                  sig.limit=10^-4)
  expect_equal(as.character(a[1, "p-value"]), 
               true_wilc_pv)
  
  # Check p-value without truncation
  a <- getDescriptionStatsBy(Loblolly$height, Loblolly$age == 10, 
                             statistics=TRUE,
                             html=TRUE, digits=2, sig.limit=10^-4)
  true_wilc_pv <- pvalueFormatter(wilcox.test(Loblolly$height ~ Loblolly$age == 10)$p.value,
                                  sig.limit=10^-4)
  expect_equal(as.character(a[1, "p-value"]), 
               true_wilc_pv)
})

test_that("Check median function", 
{ 
  stats <- by(Loblolly$height, Loblolly$young, median)
  a <- getDescriptionStatsBy(Loblolly$height, Loblolly$young, 
                             continuous_fn=describeMedian,
                             statistics=TRUE,
                             html=TRUE, digits=2, sig.limit=10^-4)
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
  
  true_wilc_pv <- pvalueFormatter(wilcox.test(Loblolly$height ~ Loblolly$young)$p.value,
                                  sig.limit=10^-4)
  expect_equal(as.character(a[1, "p-value"]), 
               true_wilc_pv)
  
  a <- getDescriptionStatsBy(Loblolly$height, Loblolly$young, 
                             continuous_fn=function(...) 
                               describeMedian(..., iqr = FALSE),
                             statistics=TRUE,
                             html=TRUE, digits=2, sig.limit=10^-4)
  
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
                             html=TRUE, digits=2, sig.limit=10^-4)
  # Check that it contains the true mean
  for (rn in rownames(a)){
    for (cn in levels(Loblolly$young))
      expect_true(grepl(stats[rn, cn], a[rn, cn]),
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
      expect_true(grepl(sprintf("%s %%", vertical_perc_stats[rn, cn]), a[rn, cn]),
                  info="Factor percentagess don't match in vertical mode")
  }
  
  a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young, hrzl_prop=TRUE,
                             continuous_fn=describeMedian,
                             statistics=TRUE,
                             html=TRUE, digits=2, sig.limit=10^-4)
  for (rn in rownames(a)){
    for (cn in levels(Loblolly$young))
      expect_true(grepl(sprintf("%s %%", horizontal_perc_stats[rn, cn]), a[rn, cn]),
                  info="Factor percentagess don't match in horizontal mode")
  }
  
  true_fisher_pval <-pvalueFormatter(fisher.test(Loblolly$fvar, Loblolly$young)$p.value, 
                              sig.limit=10^-4)
  
  expect_equal(as.character(a[1, "p-value"]), 
               true_fisher_pval)
  
})

test_that("Check factor function with missing", 
{ 
  stats <- table(Loblolly$fvar, Loblolly$young_w_missing, useNA="ifany")
  a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young_w_missing, 
                             statistics=TRUE,
                             html=TRUE, digits=2, sig.limit=10^-4)
  
  for (rn in rownames(a)){
    for (cn in levels(Loblolly$young))
      expect_true(grepl(stats[rn, cn], a[rn, cn]),
                  info="Factor count don't match")
  }
  
  
  stats <- table(Loblolly$fvar, Loblolly$young_w_missing, useNA="no")
  vertical_perc_stats <- format(apply(stats, 2, function(x){
    x/sum(x)*100
  }), nsmall=2, digits=2)
  horizontal_perc_stats <- t(format(apply(stats, 1, function(x){
    x/sum(x)*100
  }), nsmall=2, digits=2))
  
  for (rn in rownames(a)){
    for (cn in levels(Loblolly$young))
      expect_true(grepl(sprintf("%s %%", vertical_perc_stats[rn, cn]), a[rn, cn]),
                  info="Factor vertical percentages don't match")
  }

  a <- getDescriptionStatsBy(Loblolly$fvar, Loblolly$young_w_missing, 
                             hrzl_prop=TRUE,
                             statistics=TRUE,
                             html=TRUE, digits=2, sig.limit=10^-4)

  for (rn in rownames(a)){
    for (cn in levels(Loblolly$young))
      expect_true(grepl(sprintf("%s %%", horizontal_perc_stats[rn, cn]), a[rn, cn]),
                  info="Factor percentages don't match in horizontal mode")
  }
  
  a <- getDescriptionStatsBy(Loblolly$fvar_w_missing, Loblolly$young_w_missing, 
                             html=TRUE, digits=2, sig.limit=10^-4)
  stats <- table(Loblolly$fvar_w_missing, Loblolly$young_w_missing, useNA="no")
  vertical_perc_stats <- format(apply(stats, 2, function(x){
    x/sum(x)*100
  }), nsmall=2, digits=2)
  
  for (rn in rownames(a)){
    for (cn in levels(Loblolly$young)){
      expect_true(grepl(stats[rn, cn], a[rn, cn]),
                  info="Factor count don't match")
      expect_true(grepl(sprintf("%s %%", vertical_perc_stats[rn, cn]), a[rn, cn]),
                  info="Factor vertical percentages don't match")
    }
  }

  a <- getDescriptionStatsBy(Loblolly$fvar_w_missing, Loblolly$young_w_missing, 
                             show_missing="ifany",
                             html=TRUE, digits=2, sig.limit=10^-4)
  stats <- table(Loblolly$fvar_w_missing, Loblolly$young_w_missing, useNA="ifany")
  stats <- stats[,!is.na(colnames(stats))]
  rownames(stats)[is.na(rownames(stats))] <- "Missing"
  vertical_perc_stats <- format(apply(stats, 2, function(x){
    x/sum(x)*100
  }), nsmall=2, digits=2)
  for (rn in rownames(a)){
    for (cn in levels(Loblolly$young)){
      expect_true(grepl(stats[rn, cn], a[rn, cn]),
                  info="Factor count don't match")
      expect_true(grepl(sprintf("%s %%", str_trim(vertical_perc_stats[rn, cn])), a[rn, cn]),
                  info="Factor vertical percentages don't match")
    }
  }
  
  a <- getDescriptionStatsBy(Loblolly$fvar_w_missing, Loblolly$young_w_missing, 
                             show_missing="ifany", hrzl_prop = TRUE,
                             html=TRUE, digits=2, sig.limit=10^-4)
  horizontal_perc_stats <- t(format(apply(stats, 1, function(x){
    x/sum(x)*100
  }), nsmall=2, digits=2))
  
  
  for (rn in rownames(a)){
    for (cn in levels(Loblolly$young)){
      expect_true(grepl(sprintf("%s %%", str_trim(horizontal_perc_stats[rn, cn])), a[rn, cn]),
                  info="Factor vertical percentages don't match")
    }
  }
})