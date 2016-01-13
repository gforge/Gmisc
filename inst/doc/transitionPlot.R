## ------------------------------------------------------------------------
set.seed(1)
library(magrittr)
n <- 100
data <- 
  data.frame(
    Sex = sample(c("Male", "Female"),
                 size = n,
                 replace = TRUE),
    Charnley_class = sample(c("A", "B", "C"), 
                            size = n, 
                            replace = TRUE))
prob <- data.frame(
  A = 1/3 +
    (data$Sex == "Male") * .25 +
    (data$Sex != "Male") * -.25 +
    (data$Charnley_class %in% "B") * -.5 +
    (data$Charnley_class %in% "C") * -2 ,
  B = 1/3 +
    (data$Sex == "Male") * .1 + 
    (data$Sex != "Male") * -.05 +
    (data$Charnley_class == "C") * -.5,
  C = 1/3 +
    (data$Sex == "Male") * -.25 +
    (data$Sex != "Male") * .25)

# Remove negative probabilities
prob <- t(apply(prob, 1, function(x) {
  if (any(x < 0)){
    x <- x - min(x) + .05
    }
  x
  }))

data$Charnley_class_after <- 
  apply(prob, 1, function(p)
    sample(c("A", "B", "C"), 
           size = 1, 
           prob = p)) %>%
  factor(levels = c("A", "B", "C"))

# Create the transition matrix that 
# is the basis for the transition plot
trn_mtrx <-
  with(data,
       table(Charnley_class, 
             Charnley_class_after))
  
rm(prob)

## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(dev.args=list(type="cairo"), 
                      message=FALSE, 
                      warnings=FALSE)

## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.height = 5, fig.width=5)

## ------------------------------------------------------------------------
library(Gmisc)
transitionPlot(trn_mtrx)

## ------------------------------------------------------------------------
transitionPlot(trn_mtrx, 
               type_of_arrow = "simple")

## ------------------------------------------------------------------------
transitionPlot(trn_mtrx, 
               type_of_arrow = "gradient")

## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.height = 6)

## ------------------------------------------------------------------------
output_perc <- 
  function(txt, n) sprintf("%s\n[%.0f%%]", txt, n)
box_txt <- 
  cbind(mapply(output_perc, 
               txt = c("A", "B", "C"), 
               n = prop.table(rowSums(trn_mtrx))*100),
        mapply(output_perc, 
               txt = c("A", "B", "C"), 
               n = prop.table(colSums(trn_mtrx))*100))
transitionPlot(trn_mtrx, 
               main = "Charnley class changess",
               box_label = c("Before", "After"),
               box_txt = box_txt,
               cex = 1.2,
               type_of_arrow = "simple")

## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.height = 5.5)

## ------------------------------------------------------------------------
trn_mtrx_3D <-
  with(data,
       table(Charnley_class, 
             Charnley_class_after,
             Sex))

transitionPlot(trn_mtrx_3D, 
               fill_start_box = c("#5C246E", "#00688B"),
               type_of_arrow = "simple")

## ------------------------------------------------------------------------
transitionPlot(trn_mtrx_3D, 
               fill_start_box = c("#5C246E", "#00688B"),
               type_of_arrow = "gradient")

## ------------------------------------------------------------------------
transitionPlot(trn_mtrx_3D, 
               txt_start_clr = c("white", "black"),
               fill_start_box = c("#5C246E", "#BFEFFF"),
               type_of_arrow = "gradient")

