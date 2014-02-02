# Currently not invoked due to that this neeed to be do not rung
# for package compatibility
par_org <- par(ask=TRUE) 
library(grid)
# Settings
no_boxes <- 3
# Generate test setting
transition_matrix <- matrix(NA, nrow=no_boxes, ncol=no_boxes)
transition_matrix[1,] <- 200*c(.5, .25, .25)
transition_matrix[2,] <- 540*c(.75, .10, .15)
transition_matrix[3,] <- 340*c(0, .2, .80)

transitionPlot(transition_matrix, 
               box_txt = c("First", "Second", "Third"),
               type_of_arrow = "simple",
               min_lwd = unit(1, "mm"),
               max_lwd = unit(6, "mm"),
               overlap_add_width = unit(1, "mm"),
               new_page=TRUE)

# Another plot with the label/title options set
transitionPlot(transition_matrix, 
               box_label=c("First\ncolumn", "Second\ncolumn"), 
               box_label_pos="bottom", box_label_cex=1,
               main="A nice title",
               box_txt = c("First", "Second", "Third"),
               type_of_arrow = "simple",
               min_lwd = unit(1, "mm"),
               max_lwd = unit(6, "mm"),
               overlap_add_width = unit(1, "mm"),
               new_page=TRUE)


# Setup proportions
box_prop <- cbind(c(1,0,0.5), c(.52,.2,.8))
# From the Set2 Colorbrewer
start_box_clr <- c("#8DA0CB", "#FC8D62")
# Darken the colors slightly
end_box_clr <- c(colorRampPalette(c(start_box_clr[1], "#000000"))(10)[2],
  colorRampPalette(c(start_box_clr[2], "#000000"))(10)[2])
# Create a new grid
transitionPlot(transition_matrix, 
               box_prop=box_prop,
               fill_start_box=start_box_clr, 
               fill_end_box=end_box_clr, 
               txt_start_clr = c("#FFFFFF", "#000000"), 
               txt_end_clr = c("#FFFFFF", "#000000"),
               box_txt = c("First", "Second", "Third"),
               type_of_arrow = "gradient",
               min_lwd = unit(1, "mm"),
               max_lwd = unit(10, "mm"),
               overlap_add_width = unit(1, "mm"),
               new_page=TRUE)
par(par_org)
