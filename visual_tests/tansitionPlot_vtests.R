# Plain test - all should be printed
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


# Add proportions - not much should change

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



# Set one of the boxes below the printing threshold for the text

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

# Check for different proportions
# Setup proportions
box_prop <- cbind(c(1,0,0.5), c(.52,.2,.8))
# From the Set2 Colorbrewer
start_box_clr <- c("#8DA0CB", "#FC8D62")
# Darken the colors slightly
end_box_clr <- c(colorRampPalette(c(start_box_clr[1], "#000000"))(10)[2],
                 colorRampPalette(c(start_box_clr[2], "#000000"))(10)[2])
transitionPlot(transition_matrix, box_prop=box_prop, new_page= TRUE,
               fill_start_box=start_box_clr, fill_end_box=end_box_clr,
               txt_start_clr = c("#FFFFFF", "#000000"), txt_end_clr = c("#FFFFFF", "#000000"),
               box_txt = c("First", "Second", "Third"),
               type_of_arrow = "gradient",
               min_lwd = unit(1, "mm"),
               max_lwd = unit(10, "mm"),
               overlap_add_width = unit(1, "mm"))

box_prop <- cbind(c(1,0,0.5), c(.52,.8,.8))
transitionPlot(transition_matrix, box_prop=box_prop, new_page= TRUE,
               fill_start_box=start_box_clr, fill_end_box=end_box_clr,
               txt_start_clr = c("#FFFFFF", "#000000"), txt_end_clr = c("#FFFFFF", "#000000"),
               box_txt = c("First", "Second", "Third"),
               type_of_arrow = "gradient",
               min_lwd = unit(1, "mm"),
               max_lwd = unit(10, "mm"),
               overlap_add_width = unit(1, "mm"))

# Test 3D table
set.seed(1)
pre <- sample(LETTERS[c(1:3, 1, 1)], size=100, replace=TRUE)
post <- sample(LETTERS[c(1:3, 2, 3, 3)], size=100, replace=TRUE)
split <- sample(c("Male", "Female"), size=100, replace=TRUE)
start_box_clr <- c("#8DA0CB", "#FC8D62")
# Darken the colors slightly
end_box_clr <- c(colorRampPalette(c(start_box_clr[1], "#000000"))(10)[2],
                 colorRampPalette(c(start_box_clr[2], "#000000"))(10)[2])

transitionPlot(table(pre, post, split), new_page= TRUE,
               fill_start_box=start_box_clr, fill_end_box=end_box_clr,
               txt_start_clr = c("#FFFFFF", "#000000"), txt_end_clr = c("#FFFFFF", "#000000"),
               box_txt = c("First", "Second", "Third"),
               type_of_arrow = "gradient",
               min_lwd = unit(1, "mm"),
               max_lwd = unit(10, "mm"),
               overlap_add_width = unit(1, "mm"))

# Check with color label
transitionPlot(table(pre, post, split), new_page= TRUE, 
               fill_start_box=start_box_clr, fill_end_box=end_box_clr,
               txt_start_clr = c("#FFFFFF", "#000000"), txt_end_clr = c("#FFFFFF", "#000000"),
               box_txt = c("First", "Second", "Third"),
               type_of_arrow = "gradient",
               min_lwd = unit(1, "mm"),
               max_lwd = unit(10, "mm"),
               overlap_add_width = unit(1, "mm"),
               color_bar_subspace=.3, 
               color_bar_cex=1,
               color_bar_lab = c(" Males", "Females "))

rm(list=ls())
start <- sample(LETTERS[c(1,1,1,2)], size=20, replace=TRUE)
end <- sample(LETTERS[c(1,2,2,2,2)], size=20, replace=TRUE)
transitionPlot(table(start, end), new_page= TRUE, 
               type_of_arrow = "gradient", max_lwd=unit(20, "mm"),
               overlap_add_width = unit(1, "mm"))