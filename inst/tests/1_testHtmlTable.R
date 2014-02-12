library('testthat')
library('XML')
context('htmlTable')

# A simple example
mx <- matrix(1:6, ncol=3) 
colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])


test_that("With empty rownames(mx) it should skip those", { 
    table_str <- htmlTable(mx, output=FALSE)
    expect_false(grepl("<tr>[^>]+>NA</td>", table_str))
  })

test_that("The variable name should not be in the tables first row if no rownames(mx)", { 
    expect_false(grepl("<thead>[^<]*<tr>[^>]+>mx</th>", table_str))
  })

test_that("The rowname should be ignored if no row names", { 
    table_str <- htmlTable(mx, output=FALSE, rowlabel="not_mx")
    expect_false(grepl("<thead>[^<]*<tr>[^>]+>not_mx</th>", table_str))
  })

# Add rownames
rownames(mx) <- LETTERS[1:NROW(mx)] 
test_that("The rowname should appear", { 
    table_str <- htmlTable(mx, output=FALSE)
    expect_true(grepl("<tr>[^>]+>A</td>", table_str))
  })

test_that("The variable name should be in the tables first row", { 
    expect_true(grepl("<thead>[^<]*<tr>[^>]+>mx</th>", table_str))
  })

test_that("It should take the row name if there are rownames in the matrix", 
{ 
  table_str <- htmlTable(mx, output=FALSE, rowlabel="not_mx")
  expect_true(grepl("<thead>[^<]*<tr>[^>]+>not_mx</th>", table_str))
})

test_that("Check that basic output are the same as the provided matrix",
{
  table_str <- htmlTable(mx, output=FALSE)
  parsed_table <- readHTMLTable(table_str)[[1]]
  expect_equal(ncol(parsed_table), ncol(mx), info="Cols did not match")
  expect_equal(nrow(parsed_table), nrow(mx), info="Rows did not match")
  expect_true(all(mx == parsed_table),
              info="Some cells don't match the inputted cells")
})


test_that("Check that dimensions are correct with cgroup usage",
{
  table_str <- htmlTable(mx, 
                         cgroup=c("a", "b"),
                         n.cgroup=c(1, 2),
                         output=FALSE)
  parsed_table <- readHTMLTable(table_str)[[1]]
  expect_identical(ncol(parsed_table), ncol(mx) + 1, 
                   info="Cols did not match")
  expect_equal(nrow(parsed_table), 
               nrow(mx), info="Rows did not match")
  
  table_str <- htmlTable(mx, 
                         cgroup=rbind(c("aa", NA), 
                                      c("a", "b")),
                         n.cgroup=rbind(c(2, NA), 
                                        c(1, 2)),
                         output=FALSE)
  parsed_table <- readHTMLTable(table_str)[[1]]
  expect_equal(ncol(parsed_table), ncol(mx) + 1, 
               info="Cols did not match for multilevel cgroup")
  

  table_str <- htmlTable(mx, 
                         cgroup=rbind(c("aa", "bb"), 
                                      c("a", "b")),
                         n.cgroup=rbind(c(2, 1), 
                                        c(1, 2)),
                         output=FALSE)
  parsed_table <- readHTMLTable(table_str)[[1]]
  expect_equal(ncol(parsed_table), ncol(mx) + 2, 
               info="Cols did not match for multilevel cgroup")
})