library(testthat)
context("deprecated functions")

# forestplot2 should run with minimal arguments without error
expect_error(suppressWarnings(forestplot2(labeltext = 1, mean = 1, lower = 0, upper = 2)), NA)
