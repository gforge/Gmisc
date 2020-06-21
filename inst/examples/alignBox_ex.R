library(grid)
grid.newpage()

box <- boxGrob("A cool\nreference\nbox",
               x = .5, y = .8,
               box_gp = gpar(fill = "#ADB5C7"))
another_box <- boxGrob("A horizontal box", x = .1, y =  .5)
yet_another_box <- boxGrob("Another horizontal box", x = .8, y = .3)

alignedBoxes <- alignHorizontal(box,
                                another_box,
                                yet_another_box,
                                .position = "right")

box
for (b in alignedBoxes) {
  print(b)
}


vert_box <- boxGrob("Vert", 
                    x = .8, y = .3,
                    box_gp = gpar(fill = "darkgreen"),
                    txt_gp = gpar(col = "white"))
another_vert_box <- boxGrob("Another vertical", 
                            x = .1, y =  .5,
                            box_gp = gpar(fill = "darkgreen"),
                            txt_gp = gpar(col = "white"))

alignedBoxes <- alignVertical(box,
                              vert_box,
                              another_vert_box,
                              .position = "bottom")
for (b in alignedBoxes) {
  print(b)
}
