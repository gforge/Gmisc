library('testthat')
library('XML')
context('htmlTable')

# A simple example
test_that("With empty rownames(mx) it should skip those",
{
  mx <- matrix(1:6, ncol=3)
  table_str <- htmlTable(mx)
  expect_false(grepl("</th>", table_str))
  expect_false(grepl("<tr>[^>]+>NA</td>", table_str))

  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  table_str <- htmlTable(mx)
  expect_true(grepl("</th>", table_str))
  expect_false(grepl("<tr>[^>]+>NA</td>", table_str))
})


test_that("Empty cell names should be replaced with ''",
{
  mx <- matrix(1:6, ncol=3)
  mx[1,1] <- NA
  table_str <- htmlTable(mx)
  expect_false(grepl("<tr>[^>]+>NA</td>", table_str))
})

test_that("The variable name should not be in the tables first row if no rownames(mx)",
{
  mx <- matrix(1:6, ncol=3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  table_str <- htmlTable(mx)
  expect_false(grepl("<thead>[^<]*<tr>[^>]+>mx</th>", table_str))
})

test_that("A rowlabel without rownames indicates some kind of error and should throw an error",
{
  mx <- matrix(1:6, ncol=3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  expect_error(htmlTable(mx, rowlabel="not_mx"))
})

# Add rownames
test_that("The rowname should appear",
{
  mx <- matrix(1:6, ncol=3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  rownames(mx) <- LETTERS[1:NROW(mx)]
  table_str <- htmlTable(mx)
  parsed_table <- readHTMLTable(table_str)[[1]]
  expect_equal(ncol(parsed_table), ncol(mx) + 1)
  expect_match(table_str, "<tr[^>]*>[^>]+>A</td>")
  expect_match(table_str, "<tr[^>]*>[^>]+>B</td>")
})

test_that("Check that basic output are the same as the provided matrix",
{
  mx <- matrix(1:6, ncol=3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  table_str <- htmlTable(mx)
  parsed_table <- readHTMLTable(table_str)[[1]]
  expect_equal(ncol(parsed_table), ncol(mx), info="Cols did not match")
  expect_equal(nrow(parsed_table), nrow(mx), info="Rows did not match")
  expect_true(all(mx == parsed_table),
              info="Some cells don't match the inputted cells")
})

test_that("Check that dimensions are correct with rgroup usage",
{
  mx <- matrix(1:6, ncol=3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  table_str <- htmlTable(mx,
                         rgroup=c("test1", "test2"),
                         n.rgroup=c(1,1))
  parsed_table <- readHTMLTable(table_str)[[1]]
  expect_equal(ncol(parsed_table), ncol(mx), info="Cols did not match")
  expect_equal(nrow(parsed_table), nrow(mx) + 2, info="Rows did not match")
  expect_equal(as.character(parsed_table[1,1]),
               "test1", info="The rgroup did not match")
  expect_equal(as.character(parsed_table[3,1]),
               "test2", info="The rgroup did not match")
  expect_equal(as.character(parsed_table[2,1]),
               as.character(mx[1,1]), info="The row values did not match")
  expect_equal(as.character(parsed_table[4,1]),
               as.character(mx[2,1]), info="The row values did not match")


  expect_warning(htmlTable(mx,
                           rgroup=c("test1", "test2", "test3"),
                           n.rgroup=c(1,1, 0)))

  expect_error(htmlTable(mx,
                           rgroup=c("test1", "test2", "test3"),
                           n.rgroup=c(1,1, 10)))

  mx[2,1] <- "second row"
  table_str <- htmlTable(mx,
                         rgroup=c("test1", ""),
                         n.rgroup=c(1,1))
  expect_match(table_str, "<td[^>]*>second row",
              info="The second row should not have any spacers")

  parsed_table <- readHTMLTable(table_str)[[1]]
  expect_equal(nrow(parsed_table), nrow(mx) + 1, info="Rows did not match")
})

test_that("Check that dimensions are correct with cgroup usage",
{
  mx <- matrix(1:6, ncol=3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  table_str <- htmlTable(mx,
                         cgroup=c("a", "b"),
                         n.cgroup=c(1, 2),
                         output=FALSE)
  parsed_table <- readHTMLTable(table_str)[[1]]
  expect_equal(ncol(parsed_table), ncol(mx) + 1,
                   info="Cols did not match")
  expect_equal(nrow(parsed_table),
               nrow(mx), info="Rows did not match")

  expect_warning(htmlTable(mx,
                           cgroup=c("a", "b", "c"),
                           n.cgroup=c(1, 2, 0)))

  expect_error(htmlTable(mx,
                         cgroup=c("a", "b", "c"),
                         n.cgroup=c(1, 2, 10),
                         output=FALSE))

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

  table_str <- htmlTable(mx,
                         cgroup=c("a", "b"),
                         n.cgroup=c(2, 1),
                         output=FALSE, tspanner=c("First spanner",
                                                  "Secon spanner"),
                         n.tspanner=c(1,1))
  expect_match(table_str, "td[^>]*colspan='4'[^>]*>First spanner",
              info="The expected number of columns should be 4")
  expect_match(table_str, "td[^>]*colspan='4'[^>]*>Secon spanner",
              info="The expected number of columns should be 4")

  expect_error(htmlTable(mx,
                         cgroup=c("a", "b"),
                         n.cgroup=c(2, 1),
                         output=FALSE, tspanner=c("First spanner",
                                                  "Secon spanner"),
                         n.tspanner=c(1,2)))


  mx <- rbind(mx,
              mx,
              mx,
              mx)
  table_str <- htmlTable(mx,
                         cgroup=rbind(c("aa", "bb"),
                                      c("a", "b")),
                         n.cgroup=rbind(c(2, 1),
                                        c(1, 2)),
                         rgroup=paste(1:4, "rgroup"),
                         n.rgroup=rep(2, 4),
                         tspanner=c("First tspanner",
                                    "Second tspanner"),
                         n.tspanner=c(4,4),
                         output=FALSE)

  expect_match(table_str, "td[^>]*colspan='5'[^>]*>1 rgroup",
              info="The expected number of columns should be 5")
  expect_match(table_str, "td[^>]*colspan='5'[^>]*>2 rgroup",
              info="The expected number of columns should be 5")

  parsed_table <- readHTMLTable(table_str)[[1]]
  expect_equal(as.character(parsed_table[1,1]),
               "First tspanner")
  expect_equal(as.character(parsed_table[2,1]),
               "1 rgroup")
  expect_equal(as.character(parsed_table[8,1]),
               "Second tspanner")
  expect_equal(as.character(parsed_table[9,1]),
               "3 rgroup")
})

test_that("rowname = FALSE it should skip those",
{
  mx <- matrix(1:6, ncol=3)
  rownames(mx) <- c("Row A", "Row B")
  table_str <- htmlTable(mx, rowname = FALSE)
  expect_false(grepl("FALSE", table_str))
  expect_false(grepl("Row A", table_str))
})


test_that("Test style formatter", {
  styles <- c(background = "black", border ="1px solid grey")
  expect_equivalent(length(prHtGetStyle(styles)), 1)
  expect_match(prHtGetStyle(styles), "background: black;")
  expect_match(prHtGetStyle(styles), "border: [^;]+grey;")
  expect_match(prHtGetStyle(styles), "border: [^;]+grey;")
  expect_match(prHtGetStyle(styles, a=2), "border: [^;]+grey;")

  expect_error(prHtGetStyle(styles, "invalid style"))
  expect_error(prHtGetStyle(styles, "invalid style:"))
  expect_error(prHtGetStyle(styles, ":invalid style"))

  expect_match(prHtGetStyle(styles, "valid: style"), "valid: style;")
  expect_match(prHtGetStyle(styles, c(valid= "style")), "valid: style;")
  expect_match(prHtGetStyle(styles, c(valid= "style", valid1= "style")), "valid: style; valid1: style;")
  expect_match(prHtGetStyle(styles, c(valid= "style1", valid= "style2")), "valid: style2;")
  expect_match(prHtGetStyle(styles, c(valid= "style1", valid= "style2"), "valid: style3"), "valid: style3;")

})