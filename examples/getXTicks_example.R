test_data <- data.frame(coef=c(2, 0.5),
  low=c(1.5, 0.05),
  high=c(3, 0.75),
  boxsize=c(0.5, 0.5))

# Exponential form where the exponent base i 2 for easier understanding
getXTicks(low = test_data$low, 
  high = test_data$high, 
  clip=c(-Inf, Inf), 
  exp=TRUE)

# Non exponential form with using pretty
getXTicks(low = test_data$low, 
  high = test_data$high, 
  clip=c(-Inf, Inf), 
  exp=FALSE)
