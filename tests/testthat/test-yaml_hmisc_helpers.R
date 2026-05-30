library(testthat)
context("yamlDump and Hmisc helpers")

# yamlDump behaviour - capture printed output
simple_list <- list(a = 1, b = list(c = 2))
out <- capture.output(yamlDump(simple_list))
expect_true(is.character(out))
expect_true(any(grepl("a: 1", out, fixed = TRUE)))
expect_true(any(grepl("c: 2", out, fixed = TRUE)))

# character input (JSON)
json <- '{"x": [1,2], "y": {"z":"foo"}}'
out2 <- capture.output(yamlDump(json))
expect_true(is.character(out2))
expect_true(any(grepl("x:", out2, fixed = TRUE)))
expect_true(any(grepl("z: foo", out2, fixed = TRUE)))

# set_column_labels & units
library(magrittr)
df <- data.frame(mpg = 1:3, cyl = 4:6)

labeled <- set_column_labels(df, mpg = "gas", cyl = "cylindrical")
expect_equal(Hmisc::label(labeled$mpg), "gas")
expect_equal(Hmisc::label(labeled$cyl), "cylindrical")

unitdf <- set_column_units(df, mpg = "mpg", cyl = "cyl")
expect_equal(Hmisc::units(unitdf$mpg), "mpg")
expect_equal(Hmisc::units(unitdf$cyl), "cyl")
