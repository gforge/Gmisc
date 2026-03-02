library(testthat)
context("transitionPlot basic functionality")

# simple 2x2 transition matrix
mat <- matrix(c(10, 5, 3, 2), nrow = 2)
rownames(mat) <- c("A", "B")
colnames(mat) <- c("A", "B")

# running should not error and return invisibly NULL
expect_silent(res <- transitionPlot(mat, type_of_arrow = "grid", new_page = FALSE))
expect_null(res)

# dimension check: non-square should error
mat2 <- matrix(1:6, nrow = 2)
expect_error(transitionPlot(mat2), "square")

# check 3D matrix error path
mat3 <- array(1:12, dim = c(2, 2, 3))
expect_error(transitionPlot(mat3), "third dimension")
