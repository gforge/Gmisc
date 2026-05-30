library(Gmisc)
library(grid)

# Example 1: Simple header + body
grid.newpage()
box1 <- boxHeaderGrob(
    header = "Early rehabilitation",
    body = paste("0-1 weeks", "• Instruction", "• Pendulum + assisted ROM", sep = "\n"),
    header_gp = gpar(fontsize = 11, fontface = "bold", col = "black"),
    body_gp = gpar(fontsize = 9, col = "black"),
    box_gp = gpar(fill = "#E8F5E9", col = "#2E7D32", lwd = 1.4),
    y = 0.7
)

box2 <- boxHeaderGrob(
    header = "Delayed rehabilitation",
    body = paste("0-3 weeks", "• Sling immobilisation", "• Hygiene + distal ROM", sep = "\n"),
    header_gp = gpar(fontsize = 11, fontface = "bold", col = "black"),
    body_gp = gpar(fontsize = 9, col = "black"),
    box_gp = gpar(fill = "#FFF8E1", col = "#EF6C00", lwd = 1.4),
    y = 0.3
)

grid.draw(box1)
grid.draw(box2)
connectGrob(box1, box2, type = "vertical")

# Example 2: Works seamlessly with spread and align
grid.newpage()
boxes <- list(
    early = boxHeaderGrob(
        "Early rehab",
        "• Week 1-2: instruction\n• Week 2-6: active ROM",
        box_gp = gpar(fill = "#E8F5E9", col = "#2E7D32")
    ),
    late = boxHeaderGrob(
        "Delayed rehab",
        "• Week 0-3: immobilisation\n• Week 3-6: active ROM",
        box_gp = gpar(fill = "#FFF8E1", col = "#EF6C00")
    ),
    followup = boxHeaderGrob(
        "Follow-up",
        "• 2 months\n• 1 year\n• 2 years",
        box_gp = gpar(fill = "#E3F2FD", col = "#1976D2")
    )
)

boxes <- boxes |>
    spreadVertical() |>
    print()

# Connect them
connectGrob(boxes$early, boxes$followup, type = "N")
connectGrob(boxes$late, boxes$followup, type = "N")
