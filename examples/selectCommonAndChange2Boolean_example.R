n=1000
# Simulate a possible survival dataset
data <- data.frame(x1 = sample(c(NA, letters[1:2]), size=n, replace=TRUE),
  x2 = sample(c(rep(NA, length=30), 1:3), size=n, replace=TRUE),
  x3 = sample(c(letters[1:4]), size=n, replace=TRUE),
  fstatus = rbinom(n, 1, .2),
  ftime = runif(n)*10)

selectCommonAndChange2Boolean(data[,1:3],
  data$fstatus,
  min = 100,
  min_with_occurences = 10)


