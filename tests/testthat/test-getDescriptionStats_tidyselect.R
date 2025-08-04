library("testthat")
library("dplyr")
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

  expect_equal(length(out), nrow(Loblolly))

  class(out) <- tail(class(out), -1)
  expect_equal(length(out), 2)
})

test_that("Using default_ref in with tidyselect", {
  set.seed(1)
  n = 20
  d <- data.frame(a = sample(LETTERS[1:2], size = n, replace = TRUE),
                  b = sample(LETTERS[1:2], size = n, replace = TRUE),
                  by = sample(letters[1:2], size = n, replace = TRUE)) %>%
    mutate(across(!by, \(x) factor(x, levels = LETTERS[1:2])),
           across(by, \(x) factor(x, levels = letters[1:2])))


  out_standard <- d %>%
    getDescriptionStatsBy(a,
                          b,
                          by = by)

  out_with_A <- d %>%
    getDescriptionStatsBy(a,
                          b,
                          default_ref = "A",
                          by = by)
  expect_true(identical(out_with_A, out_standard))

  out_with_B <- d %>%
    getDescriptionStatsBy(a,
                          b,
                          default_ref = "B",
                          by = by)
  expect_false(identical(out_with_B, out_standard))

  out_with_a_as_B <- d %>%
    getDescriptionStatsBy(a,
                          b,
                          default_ref = c(a = "B"),
                          by = by)
  expect_identical(out_with_a_as_B[[1]], out_with_B[[1]])
  expect_identical(out_with_a_as_B[[2]], out_standard[[2]])
})
