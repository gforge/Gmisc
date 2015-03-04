## ------------------------------------------------------------------------
set.seed(1)
library(magrittr)
n <- 100
data <- 
  data.frame(
    Sex = sample(c("Male", "Female"),
                 size = n,
                 replace = TRUE,
                 prob = c(.4, .6)),
    Charnley_class = sample(c("A", "B", "C"), 
                            size = n, 
                            replace = TRUE))
getProbs <- function(Chrnl_name){
  prob <- data.frame(
    A = 1/3 +
      (data$Sex == "Male") * .25 +
      (data$Sex != "Male") * -.25 +
      (data[[Chrnl_name]] %in% "B") * -.5 +
      (data[[Chrnl_name]] %in% "C") * -2 ,
    B = 1/3 +
      (data$Sex == "Male") * .1 + 
      (data$Sex != "Male") * -.05 +
      (data[[Chrnl_name]] == "C") * -2,
    C = 1/3 +
      (data$Sex == "Male") * -.25 +
      (data$Sex != "Male") * .25)
  
  # Remove negative probabilities
  t(apply(prob, 1, function(x) {
    if (any(x < 0)){
      x <- x - min(x) + .05
      }
    x
    }))
}
  
Ch_classes <- c("Charnley_class")
Ch_classes %<>% c(sprintf("%s_%dyr", Ch_classes, c(1,2,6)))
for (i in 1:length(Ch_classes)){
  if (i == 1)
    next;

  data[[Ch_classes[i]]] <- 
    apply(getProbs(Ch_classes[i-1]), 1, function(p)
      sample(c("A", "B", "C"), 
             size = 1, 
             prob = p)) %>%
    factor(levels = c("A", "B", "C"))
}

## ----, echo=FALSE--------------------------------------------------------
knitr::opts_chunk$set(dev.args = list(type="cairo"), 
                      message = FALSE, 
                      warnings = FALSE)
knitr::opts_chunk$set(fig.height = 5, fig.width=5)

## ------------------------------------------------------------------------
library(Gmisc)
transitions <- table(data$Charnley_class, data$Charnley_class_1yr) %>%
  getRefClass("Transition")$new(label=c("Before surgery", "1 year after"))
transitions$render()

## ------------------------------------------------------------------------
transitions <- table(data$Charnley_class, data$Charnley_class_1yr) %>%
  getRefClass("Transition")$new(label=c("Before surgery", "1 year after"))
transitions$title <- "Charnley class in relation to THR"
transitions$arrow_type <- "simple"
transitions$box_label_pos <- "bottom"
transitions$render()

## ------------------------------------------------------------------------
transitions <- table(data$Charnley_class, data$Charnley_class_1yr, data$Sex) %>%
  getRefClass("Transition")$new(label=c("Before surgery", "1 year after"))
transitions$title <- "Charnley class in relation to THR"
transitions$clr_bar <- "bottom"
transitions$render()

## ----, echo=FALSE--------------------------------------------------------
knitr::opts_chunk$set(fig.height = 5, fig.width=7)

## ------------------------------------------------------------------------
transitions <- table(data$Charnley_class, data$Charnley_class_1yr, data$Sex) %>%
  getRefClass("Transition")$new(label=c("Before surgery", "1 year after"))
transitions$title <- "Charnley class in relation to THR"
transitions$arrow_type <- "simple"
table(data$Charnley_class_1yr, data$Charnley_class_2yr, data$Sex) %>%
  transitions$addTransitions(label="2 years after")
library(grid)
transitions$max_lwd <- unit(.05, "npc")
transitions$render()

