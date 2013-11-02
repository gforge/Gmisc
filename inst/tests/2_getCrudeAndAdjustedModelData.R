library("testthat")
context("getCrudeAndAdjustedModelData")

library("survival")

s <- Surv(ds$ftime, ds$fstatus == 1)
fit1 <- coxph(s ~ x1 + x2 + x3, data=ds)

# Check that it doesn't include the rcs() spline since this doesn't
# make sense 
fit2 <- coxph(s ~ x1 + ns(x2, 4) + strata(x3), data=ds)

test_that("Correct number of rows and columns", {
    source(sprintf(gsub("Gmisc/.*", "Gmisc/R/%s", getwd()), "getCrudeAndAdjustedModelData.R"))
    data_matrix <- getCrudeAndAdjustedModelData(fit1)
    expect_that(NROW(data_matrix), equals(4))
    expect_that(NCOL(data_matrix), equals(6))
    
    data_matrix <- getCrudeAndAdjustedModelData(fit2)
    expect_that(NROW(data_matrix), equals(1))
    expect_that(NCOL(data_matrix), equals(6))
})
