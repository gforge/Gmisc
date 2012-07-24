test_data <- data.frame(coef=c(2, 0.5),
  low=c(1.5, 0.05),
  high=c(3, 0.75),
  boxsize=c(0.5, 0.5))

# Exponential form where the exponent base i 2 for easier understanding
getTicks(low = test_data$low, 
  high = test_data$high, 
  clip=c(-Inf, Inf), 
  exp=TRUE)

# Non exponential form with using pretty
getTicks(low = test_data$low, 
  high = test_data$high, 
  clip=c(-Inf, Inf), 
  exp=FALSE)


# A very simple example
getTicks(1:5*2.33, 
  exp=FALSE)

# A slightly more advanced exponential version
getTicks(1:10*.33, 
  digits=2,
  exp=TRUE)
