set.seed(10)
ds <- data.frame(
  ftime = rexp(200),
  fstatus = sample(0:1,200,replace=TRUE),
  x = factor(sample(LETTERS[1:4], size=200, replace=TRUE)))

library(rms)
dd <- datadist(ds)
options(datadist="dd")

s <- Surv(ds$ftime, ds$fstatus == 1)
fit <- cph(s ~ x, data=ds)

context("printCrudeAndAdjustedModel")
test_that("Check position of reference", {
    a <- printCrudeAndAdjustedModel(fit, add_references=TRUE)
    expect_match(a[1,2], "ref")
    
    # Getting the name wrong should not change the reference
    a <- printCrudeAndAdjustedModel(fit, add_references=TRUE, add_references_pos=list(a=2))
    expect_match(a[1,2], "ref")
    
    # This should move the reference
    a <- printCrudeAndAdjustedModel(fit, add_references=TRUE, add_references_pos=list(x=2))
    expect_match(a[2,2], "ref")
    
    # Should end up at first position if referenced outside
    a <- printCrudeAndAdjustedModel(fit, add_references=TRUE, add_references_pos=list(x=5))
    expect_match(a[1,2], "ref")
    
    # Should end up at first position if referenced outside
    a <- printCrudeAndAdjustedModel(fit, add_references=TRUE, add_references_pos=list(x=-5))
    expect_match(a[1,2], "ref")
  })