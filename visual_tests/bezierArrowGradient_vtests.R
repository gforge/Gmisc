# A bug that I found through the transitionPlot
ag <- bezierArrowGradient(x = c(0.25, 0.5, 0.5, 0.75), 
                          y = c(0.125925925925926, 
                                0.125925925925926, 
                                0.121283950617284, 
                                0.121283950617284),
                          width = 0.039509010185427, 
                          arrow = list(length = 0.0625, 
                                       base = 0.069829997839748), 
                          clr = "#000000", 
                          grdt_type = "triangle", 
                          grdt_clr_prop = 0.5, 
                          grdt_start_prop = 0.3, 
                          grdt_decrease_prop = 0.3, 
                          grdt_clr = "#C49696")
grid.newpage()
grid.draw(eval(as.call(problematic_arrow)))
