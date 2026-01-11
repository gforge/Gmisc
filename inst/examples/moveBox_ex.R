library(grid)
grid.newpage()

pushViewport(viewport(y = 0.85, height = unit(.3, "npc")))
grid.rect(gp = gpar(col = "steelblue"))
grid.text("Example 1", y = .9, x = unit(10, "mm"), just = "left")
box <- boxGrob("A simple box", x = .5, y = .5) # Start at the middle
moveBox(box, x = -.2, space = "relative") # Move to the left
popViewport()


# Advanced example: create a nested list of treatment boxes, spread them horizontally,
# then move a single nested element (`Ibuprofen`) using `.subelement` via a pipe.
pushViewport(viewport(y = 0.35, height = unit(.7, "npc") - unit(2, "mm")))
grid.rect(gp = gpar(col = "lightgreen"))

boxes <- list(
    population = boxGrob("Population"),
    treatment = list(
        Ibuprofen = boxGrob("Ibuprofen"),
        Paracetamol = boxGrob("Paracetamol"),
        Aspirin = boxGrob("Aspirin")
    ),
    followup = paste("Follow-up 4 weeks:",
        " - EQ-5D 5L",
        " - Lab",
        sep = "\n"
    ) |>
        boxGrob(just = "left")
) |>
    alignHorizontal(.position = "center") |>
    spreadVertical() |>
    spreadHorizontal(
        .from = unit(0.1, "npc"),
        .to = unit(0.9, "npc"),
        .type = "center",
        .subelement = "treatment"
    ) |>
    moveBox(
        y = 0.1,
        space = "relative",
        .subelement = c("treatment", "Ibuprofen")
    )

boxes
connectGrob(boxes$population, boxes$treatment, type = "N")
connectGrob(boxes$treatment, boxes$followup, type = "N")
