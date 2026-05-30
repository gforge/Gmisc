library(testthat)

context("prAddEmptyVals order and NA handling")

# exercise the internal helper directly

# create a fake list with row names out of order and an NA
t1 <- list(a = c(b = 1, a = 2), c = c(c = 3))
# call using exported function indirectly through getDescriptionStatsBy? we can
# call the helper by using :: but it's not exported; use getFromNamespace
prAddEmptyVals_fun <- getFromNamespace("prAddEmptyVals", "Gmisc")

res <- prAddEmptyVals_fun(t1, missing_value = 0)

# the names should preserve the order of appearance: b,a,c
expect_equal(names(res$a), c("b", "a", "c"))
expect_equal(names(res$c), c("b", "a", "c"))

# when NA present, ensure it is last
inch <- list(
    x = setNames(c(1, 2), c("head", NA)),
    y = setNames(3, "foot")
)
res2 <- prAddEmptyVals_fun(inch, missing_value = -1)
expect_equal(tail(names(res2$x), 1), NA_character_)
# when one element has NA name the helper adds the NA row for all entries
expect_equal(tail(names(res2$y), 1), NA_character_)
