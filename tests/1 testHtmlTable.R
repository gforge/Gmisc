# A simple example
mx <- matrix(1:6, ncol=3) 
colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])

# Without rownames it should skip those
table_str <- htmlTable(mx, output=FALSE)
expect_false(grep("<tr>[^>]+>NA</td>", table_str))

rownames(mx) <- LETTERS[1:NROW(mx)] 
table_str <- htmlTable(mx, output=FALSE)
expect_true(grep("<tr>[^>]+>A</td>", table_str))

# The variable name should be in the tables first row
expect_true(grep("<thead>[^<]*<tr>[^>]+>mx</th>", table_str))

table_str <- htmlTable(mx, output=FALSE, rowlabel="not_mx")
expect_true(grep("<thead>[^<]*<tr>[^>]+>not_mx</th>", table_str))