library(testthat)
context("getPval wrappers return numeric p-values")

set.seed(42)

# numeric data
x <- rnorm(50)
by2 <- factor(sample(c("A", "B"), size = 50, replace = TRUE))
by3 <- factor(sample(c("A", "B", "C"), size = 50, replace = TRUE))

# Fisher/ChiSq require factors
xf <- sample(letters[1:3], size = 50, replace = TRUE)

test_that("Wilcoxon returns scalar between 0 and 1", {
    p <- getPvalWilcox(x, by2)
    expect_type(p, "double")
    expect_true(length(p) == 1)
    expect_true(p >= 0 && p <= 1)
})

test_that("ANOVA returns scalar between 0 and 1", {
    p <- getPvalAnova(x, by3)
    expect_type(p, "double")
    expect_true(length(p) == 1)
    expect_true(p >= 0 && p <= 1)
})

test_that("Kruskal returns scalar between 0 and 1", {
    p <- getPvalKruskal(x, by3)
    expect_type(p, "double")
    expect_true(length(p) == 1)
    expect_true(p >= 0 && p <= 1)
})

# small tables for Fisher and Chi-square

test_that("Fisher and ChiSq produce identical results on 2x2 data", {
    tblx <- sample(c("a", "b"), 100, replace = TRUE)
    tbly <- sample(c("c", "d"), 100, replace = TRUE)
    p1 <- getPvalFisher(tblx, tbly)
    p2 <- getPvalChiSq(tblx, tbly)
    expect_type(p1, "double")
    expect_type(p2, "double")
    expect_true(p1 >= 0 && p1 <= 1)
    expect_true(p2 >= 0 && p2 <= 1)
})

# error conditions

invalid <- 1:5

# by and x lengths mismatch
expect_error(getPvalWilcox(1:4, factor(1:5)))
