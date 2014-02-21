# Settings
no_boxes <- 3
# Generate test setting
transition_matrix <- matrix(NA, nrow=no_boxes, ncol=no_boxes)
transition_matrix[1,] <- 200*c(.5, .25, .25)
transition_matrix[2,] <- 540*c(.75, .10, .15)
transition_matrix[3,] <- 340*c(0, .2, .80)

grid.newpage()
transitionPlot(transition_matrix,
               box_txt = c("First", "Second", "Third"),
               type_of_arrow = "simple",
               min_lwd = unit(1, "mm"),
               max_lwd = unit(6, "mm"),
               overlap_add_width = unit(1, "mm"))


# Setup proportions
box_prop <- cbind(c(1,0,0.5), c(.52,.2,.8))
# From the Set2 Colorbrewer
start_box_clr <- c("#8DA0CB", "#FC8D62")
# Darken the colors slightly
end_box_clr <- c(colorRampPalette(c(start_box_clr[1], "#000000"))(10)[2],
                 colorRampPalette(c(start_box_clr[2], "#000000"))(10)[2])
# Create a new grid
grid.newpage()
transitionPlot(transition_matrix, box_prop=box_prop,
               fill_start_box=start_box_clr, fill_end_box=end_box_clr,
               txt_start_clr = c("#FFFFFF", "#000000"), txt_end_clr = c("#FFFFFF", "#000000"),
               box_txt = c("First", "Second", "Third"),
               type_of_arrow = "gradient",
               min_lwd = unit(1, "mm"),
               max_lwd = unit(10, "mm"),
               overlap_add_width = unit(1, "mm"))

# Settings
no_boxes <- 3
# Generate test setting
transition_matrix <- matrix(NA, nrow=no_boxes, ncol=no_boxes)
transition_matrix[1,] <- 200*c(.7, .05, .25)
transition_matrix[2,] <- 540*c(.75, .05, .2)
transition_matrix[3,] <- 340*c(0, .02, .98)

grid.newpage()
transitionPlot(transition_matrix,
               box_txt = c("First", "Second", "Third"),
               type_of_arrow = "simple",
               min_lwd = unit(1, "mm"),
               max_lwd = unit(6, "mm"),
               overlap_add_width = unit(1, "mm"))
