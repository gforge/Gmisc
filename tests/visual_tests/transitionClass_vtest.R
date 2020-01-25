set.seed(1)
library(magrittr)
library(Gmisc)

names <-
  strsplit("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation",
         " ")[[1]] %>%
  gsub("[,.]", " ", .)

n <- 100
my_data <-
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
    Var_c = sapply(8:13, function(x) paste(names[x*2 + 1:2],
                                           collapse = " - ")) %>%
      sample(size = n,
             replace = TRUE),
    Var_d = sample(names[15:16],
                   size = n,
                   replace = TRUE)
  )

transitions <- with(my_data,
                    table(Var_a, Var_b)) %>%
  getRefClass("Transition")$new(label=c("Var a", "Var b"))

with(my_data,
     table(Var_b, Var_c)) %>%
  transitions$addTransitions()
transitions$render()


# From issue #43

transMatrix <- as.table(matrix(c(16, 4, 16, 64),ncol=2,byrow=T))
transitions <- transMatrix %>%
  getRefClass("Transition")$new(label=c("Step 1", "Step 2"))
transitions$lwd_prop_type = "box"
transitions$render()
