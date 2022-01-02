library("testthat")
# I need to include this for unknown reason or the test fails in R CMD check mode

data("Loblolly")

set.seed(1)
Loblolly$young <- Loblolly$age < 10
Loblolly$young <- factor(Loblolly$young, label = c("Yes", "No"))
Loblolly$fvar <- factor(sample(letters[1:3], size = nrow(Loblolly), replace = TRUE))

test_that("Check basic tidyselect functionality", {
  out <- Loblolly %>%
    getDescriptionStatsBy(height,
                          age,
                          by = young,
                          statistics = TRUE,
                          digits = 2,
                          statistics.sig_lim = 10^-4)

  expect_length(out, 2)
})
