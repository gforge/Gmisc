library("testthat")
library("stringr")
context("descriptionStats")

data("Loblolly")

set.seed(1)
Loblolly$young <- Loblolly$age < 10
Loblolly$young <- factor(Loblolly$young, label = c("Yes", "No"))
Loblolly$fvar <- factor(sample(letters[1:3], size = nrow(Loblolly), replace = TRUE))
Loblolly$young_w_missing <- Loblolly$young
Loblolly$young_w_missing[sample(1:nrow(Loblolly), size = 4)] <- NA
Loblolly$fvar_w_missing <- Loblolly$fvar
Loblolly$fvar_w_missing[sample(1:nrow(Loblolly), size = 4)] <- NA
Loblolly$height_w_missing <- Loblolly$height
Loblolly$height_w_missing[sample(1:nrow(Loblolly), size = 4)] <- NA

test_that("Check describeMean", {
  expect_match(
    describeMean(Loblolly$height, html = TRUE, digits = 1),
    sprintf("^%.1f", mean(Loblolly$height))
  )
  expect_match(
    describeMean(Loblolly$height, html = TRUE, digits = 2),
    sprintf("^%.2f", mean(Loblolly$height))
  )
  expect_match(
    describeMean(Loblolly$height_w_missing, html = TRUE, digits = 2, useNA = "no"),
    sprintf("^%.2f", mean(Loblolly$height_w_missing, na.rm = TRUE))
  )

  dm <- describeMean(Loblolly$height_w_missing, html = TRUE, digits = 2)
  expect_match(
    dm[1],
    sprintf("^%.2f", mean(Loblolly$height_w_missing, na.rm = TRUE))
  )
  expect_match(
    dm[2],
    sprintf(
      "^%d",
      sum(is.na(Loblolly$height_w_missing))
    )
  )
  expect_match(
    dm[2],
    sprintf(
      "\\(%.2f%%\\)",
      mean(is.na(Loblolly$height_w_missing)) * 100
    )
  )

  dm <- describeMean(Loblolly$height_w_missing,
    html = TRUE,
    digits = 2, useNA.digits = 1
  )
  expect_match(
    dm[1],
    sprintf("^%.2f", mean(Loblolly$height_w_missing, na.rm = TRUE))
  )
  expect_match(
    dm[2],
    sprintf(
      "^%d",
      sum(is.na(Loblolly$height_w_missing))
    )
  )
  expect_match(
    dm[2],
    sprintf(
      "\\(%.1f%%\\)",
      mean(is.na(Loblolly$height_w_missing)) * 100
    )
  )
})

test_that("Check describeMedian", {
  expect_match(
    describeMedian(Loblolly$height, html = TRUE, digits = 1),
    sprintf("^%.1f", median(Loblolly$height))
  )
  expect_match(
    describeMedian(Loblolly$height, html = TRUE, digits = 2),
    sprintf("^%.2f", median(Loblolly$height))
  )
  expect_match(
    describeMedian(Loblolly$height_w_missing, html = TRUE, digits = 2, useNA = "no"),
    sprintf("^%.2f", median(Loblolly$height_w_missing, na.rm = TRUE))
  )

  dm <- describeMedian(Loblolly$height_w_missing,
    html = TRUE,
    digits = 2
  )
  expect_match(
    dm[1],
    sprintf("^%.2f", median(Loblolly$height_w_missing, na.rm = TRUE))
  )
  expect_match(
    dm[2],
    sprintf(
      "^%d",
      sum(is.na(Loblolly$height_w_missing))
    )
  )
  expect_match(
    dm[2],
    sprintf(
      "\\(%.2f%%\\)",
      mean(is.na(Loblolly$height_w_missing)) * 100
    )
  )

  dm <- describeMedian(Loblolly$height_w_missing,
    html = TRUE,
    digits = 2, useNA.digits = 1
  )
  expect_match(
    dm[1],
    sprintf("^%.2f", median(Loblolly$height_w_missing, na.rm = TRUE))
  )
  expect_match(
    dm[2],
    sprintf(
      "^%d",
      sum(is.na(Loblolly$height_w_missing))
    )
  )
  expect_match(
    dm[2],
    sprintf(
      "\\(%.1f%%\\)",
      mean(is.na(Loblolly$height_w_missing)) * 100
    )
  )
})

test_that("Check describeFactors", {
  d_f <- describeFactors(Loblolly$fvar, html = TRUE, digits = 1)
  t_f <- table(Loblolly$fvar)
  for (n in names(t_f)) {
    expect_match(
      d_f[n, ],
      sprintf("^%d", t_f[n])
    )
    expect_match(
      d_f[n, ],
      sprintf("\\(%.1f%%\\)", prop.table(t_f)[n] * 100)
    )
  }

  d_f <- describeFactors(Loblolly$fvar, html = TRUE, digits = 1, number_first = FALSE)
  for (n in names(t_f)) {
    expect_match(
      d_f[n, ],
      sprintf("\\(%d\\)", t_f[n])
    )
    expect_match(
      d_f[n, ],
      sprintf("^%.1f%%", prop.table(t_f)[n] * 100)
    )
  }

  d_f <- describeFactors(Loblolly$fvar_w_missing, html = TRUE, digits = 1)
  t_f <- table(Loblolly$fvar_w_missing, useNA = "ifany")
  for (n in names(t_f)) {
    if (is.na(n)) {
      row_no_df <- which(rownames(d_f) == "Missing")
      row_no_tf <- which(is.na(names(t_f)))
    } else {
      row_no_df <- which(rownames(d_f) == n)
      row_no_tf <- which(names(t_f) == n)
    }
    expect_match(
      d_f[row_no_df, ],
      sprintf("^%d", t_f[row_no_tf])
    )
    expect_match(
      d_f[row_no_df, ],
      sprintf("\\(%.1f%%\\)", prop.table(t_f)[row_no_tf] * 100)
    )
  }

  d_f <- describeFactors(Loblolly$fvar_w_missing,
    html = TRUE, digits = 2,
    useNA.digits = 1
  )
  t_f <- table(Loblolly$fvar_w_missing, useNA = "ifany")
  for (n in names(t_f)) {
    if (is.na(n)) {
      row_no_df <- which(rownames(d_f) == "Missing")
      row_no_tf <- which(is.na(names(t_f)))
      prop <- sprintf("%.1f", prop.table(t_f)[is.na(names(t_f))] * 100)
    } else {
      row_no_df <- which(rownames(d_f) == n)
      row_no_tf <- which(names(t_f) == n)
      prop <- sprintf("%.2f", prop.table(t_f)[which(names(t_f) == n)] * 100)
    }
    expect_match(
      d_f[row_no_df, ],
      sprintf("^%d", t_f[row_no_tf])
    )
    expect_match(
      d_f[row_no_df, ],
      sprintf("\\(%s%%\\)", prop)
    )
  }
})

test_that("Check describeProp", {
  d_f <- describeProp(Loblolly$young, html = TRUE, digits = 1)
  expect_equal(length(d_f), 1)

  t_f <- table(Loblolly$young)
  expect_match(
    d_f,
    sprintf("^%d", t_f["Yes"])
  )
  expect_match(
    d_f,
    sprintf("\\(%.1f%%\\)", prop.table(t_f)["Yes"] * 100)
  )

  # Should default to factor function if there are misssing
  d_f <- describeProp(Loblolly$young_w_missing, html = TRUE, digits = 1)
  expect_equal(length(d_f), 3)
})