set.seed(1)
library(magrittr)
library(Gmisc)

names <-
  strsplit("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation",
         " ")[[1]] %>%
  gsub("[,.]", " ", .)

n <- 100
data <-
  data.frame(
    Binary = sample(c("Alt 1", "Alt 2"),
                 size = n,
                 replace = TRUE,
                 prob = c(.4, .6)),
    Var_a = sample(names[1:3],
                   size = n,
                   replace = TRUE),
    Var_b = sample(names[4:7],
                   size = n,
                   replace = TRUE),
    Var_c = sapply(8:16, function(x) paste(names[x*2 + 1:2],
                                           collapse = " - ")) %>%
      sample(size = n,
             replace = TRUE),
    Var_d = sample(names[15:16],
                   size = n,
                   replace = TRUE)
  )

transitions <- table(data$Var_a, data$Var_b) %>%
  getRefClass("Transition")$new(label=c("Var a", "Var b"))
transitions$render()
