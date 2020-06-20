org_par <- par(ask = TRUE)
set.seed(12345)
# Simulate data with a pattern
dataMatrix <- matrix(rnorm(15 * 160), ncol = 15)
colnames(dataMatrix) <-
  c(
    paste("Pos.3:", 1:3, sep = " #"),
    paste("Neg.Decr:", 4:6, sep = " #"),
    paste("No pattern:", 7:8, sep = " #"),
    paste("Pos.Incr:", 9:11, sep = " #"),
    paste("No pattern:", 12:15, sep = " #")
  )
for (i in 1:nrow(dataMatrix)) {
  # flip a coin
  coinFlip1 <- rbinom(1, size = 1, prob = 0.5)
  coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
  coinFlip3 <- rbinom(1, size = 1, prob = 0.5)

  # if coin is heads add a common pattern to that row
  if (coinFlip1) {
    cols <- grep("Pos.3", colnames(dataMatrix))
    dataMatrix[i, cols] <- dataMatrix[i, cols] + 3
  }

  if (coinFlip2) {
    cols <- grep("Neg.Decr", colnames(dataMatrix))
    dataMatrix[i, cols] <- dataMatrix[i, cols] - seq(from = 5, to = 15, length.out = length(cols))
  }

  if (coinFlip3) {
    cols <- grep("Pos.Incr", colnames(dataMatrix))
    dataMatrix[i, cols] <- dataMatrix[i, cols] + seq(from = 3, to = 15, length.out = length(cols))
  }
}

# Illustrate data
heatmap(dataMatrix, Colv = NA, Rowv = NA, margins = c(7, 2), labRow = "")

svd_out <- svd(scale(dataMatrix))

library(lattice)
b_clr <- c("steelblue", "darkred")
key <- simpleKey(
  rectangles = TRUE, space = "top", points = FALSE,
  text = c("Positive", "Negative")
)
key$rectangles$col <- b_clr

b1 <- barchart(as.table(svd_out$v[, 1]),
  main = "First column",
  horizontal = FALSE, col = ifelse(svd_out$v[, 1] > 0,
    b_clr[1], b_clr[2]
  ),
  ylab = "Impact value",
  scales = list(x = list(rot = 55, labels = colnames(dataMatrix), cex = 1.1)),
  key = key
)

b2 <- barchart(as.table(svd_out$v[, 2]),
  main = "Second column",
  horizontal = FALSE, col = ifelse(svd_out$v[, 2] > 0,
    b_clr[1], b_clr[2]
  ),
  ylab = "Impact value",
  scales = list(x = list(rot = 55, labels = colnames(dataMatrix), cex = 1.1)),
  key = key
)

b3 <- barchart(as.table(svd_out$v[, 3]),
  main = "Third column",
  horizontal = FALSE, col = ifelse(svd_out$v[, 3] > 0,
    b_clr[1], b_clr[2]
  ),
  ylab = "Impact value",
  scales = list(x = list(rot = 55, labels = colnames(dataMatrix), cex = 1.1)),
  key = key
)

b4 <- barchart(as.table(svd_out$v[, 4]),
  main = "Fourth column",
  horizontal = FALSE, col = ifelse(svd_out$v[, 4] > 0,
    b_clr[1], b_clr[2]
  ),
  ylab = "Impact value",
  scales = list(x = list(rot = 55, labels = colnames(dataMatrix), cex = 1.1)),
  key = key
)

# Note that the fourth has the no pattern columns as the
# chosen pattern, probably partly because of the previous
# patterns already had been identified
print(b1, position = c(0, 0.5, .5, 1), more = TRUE)
print(b2, position = c(0.5, 0.5, 1, 1), more = TRUE)
print(b3, position = c(0, 0, .5, .5), more = TRUE)
print(b4, position = c(0.5, 0, 1, .5))

# Let's look at how well the SVD identifies
# the most influential columns
getSvdMostInfluential(dataMatrix,
  quantile = .8,
  similarity_threshold = .9,
  plot_threshold = .05,
  plot_selection = TRUE
)
par(org_par)