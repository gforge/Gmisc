library(grid)
library(magrittr)
# A bug that I found through the transitionPlot
grid.newpage()
bezierArrowGradient(x = c(0.25, 0.5, 0.5, 0.75),
                    y = c(0.125925925925926,
                          0.125925925925926,
                          0.121283950617284,
                          0.121283950617284) + .3,
                    width = 0.039509010185427,
                    arrow = list(length = 0.0625,
                                 base = 0.069829997839748),
                    clr = "#000000",
                    grdt_type = "triangle",
                    grdt_prop = 0.8,
                    grdt_clr_prop = 0.5,
                    grdt_decrease_prop = 0.3,
                    grdt_clr = "#C49696") %>%
  grid.draw

# How does the proportions behave close to the arrow
grid.newpage()
bezierArrowGradient(x = c(0.25, 0.5, 0.5, 0.75),
                    y = rep(.5, 4),
                    width = .3,
                    arrow = list(length = unit(.3, "npc")),
                    clr = "#000000",
                    grdt_type = "triangle",
                    grdt_clr_prop = 0.5,
                    grdt_decrease_prop = 0.3,
                    grdt_clr = "#C49696") %>%
  grid.draw

# Step off bug
grid.newpage()
bezierArrowGradient(x = c(0.25, 0.5, 0.5, 0.75),
                    y = rep(.5, 4),
                    width = 0.4,
                    arrow = list(length = 0.2,
                                 base = .5),
                    clr = "#000000",
                    grdt_type = "triangle",
                    grdt_prop = 0.8,
                    grdt_clr_prop = 0.5,
                    grdt_decrease_prop = 0.3,
                    grdt_clr = "#C49696") %>%
  grid.draw


# Problematic start with X shape and a triangle before the arrow at the bottom
grid.newpage()
bezierArrowGradient(x = c(0.25, 0.5, 0.5, 0.75),
                    grdt_line_width = unit(2, "cm"),
                    y = c(0.25, 0.5, 0.75, 0.75),
                    width = unit(5, "cm"),
                    arrow = list(length = 0.2,
                                 base = .5),
                    clr = "#000000",
                    grdt_type = "triangle",
                    grdt_prop = 0.8,
                    grdt_clr_prop = 0.5,
                    grdt_decrease_prop = 0.3,
                    grdt_clr = "#C49696") %>%
  grid.draw