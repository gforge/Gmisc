library(testthat)
context("getSvdMostInfluential")

# simple matrix with known structure
m <- diag(1, 5)
colnames(m) <- paste0("v", 1:5)

res <- getSvdMostInfluential(m,
    quantile = 0.5, similarity_threshold = 0.5,
    plot_selection = FALSE
)

# result should be the special S3 class and contain expected elements
expect_s3_class(res, "Gmisc_svd_analysis")
expect_true(is.list(res))
expect_true("most_influential" %in% names(res))
expect_true("svd" %in% names(res))

# check that most_influential is integer vector
expect_type(res$most_influential, "integer")

# ensure missing values produce an error
m2 <- m
m2[1, 1] <- NA
expect_error(getSvdMostInfluential(m2, plot_selection = FALSE), "Missing values are not allowed")
