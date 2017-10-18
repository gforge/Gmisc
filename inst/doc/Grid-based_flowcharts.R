## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----basic_plot, fig.height=2, fig.width=2-------------------------------
# Load the grid library
# part of standard R libraries so no need installing
library(grid)  

# Create a new graph
grid.newpage()

pushViewport(viewport(width=.5, height=.8))

grid.rect(gp=gpar(fill="#D8F0D1"))

popViewport()

## ----relative_lines, fig.height=3, fig.width=3---------------------------
grid.newpage()
pushViewport(viewport(width=.5, height=.8, clip="on"))
grid.rect(gp=gpar(lty=2, fill="lightyellow"))
lg <- linesGrob(x = unit(c(.2, 1), "npc"),
                y = unit(c(.2, 1), "npc"),
                gp = gpar(lwd=2))
grid.draw(lg)
pushViewport(viewport(x=0, y=.6, just="left", width=.4, height=.4, angle = 20))
grid.rect(gp=gpar(fill="lightblue")) # A translucent box to indicate the new viewport
grid.draw(lg)
popViewport()

## ----absolute_lines, fig.height=3, fig.width=3---------------------------
grid.newpage()
pushViewport(viewport(width=.5, height=.8, clip="on"))
grid.rect(gp=gpar(lty=2, fill="lightyellow"))
lg <- linesGrob(x = unit(c(2, 10), "mm"),
                y = unit(c(2, 10), "mm"),
                gp = gpar(lwd=2))
grid.draw(lg)
pushViewport(viewport(x=0, y=.6, just="left", width=.4, height=.4, angle = 20))
grid.rect(gp=gpar(fill="lightblue")) # A translucent box to indicate the new viewport
grid.draw(lg)
popViewport()

## ----basic_box, fig.height=1.5, fig.width=3, message=FALSE---------------
library(Gmisc)
grid.newpage()
txt <- 
"Just a plain box
with some text
- Note that newline is OK"
boxGrob(txt)

## ----styled_box, fig.height=3, fig.width=3-------------------------------
grid.newpage()
boxGrob("A large\noffset\nyellow\nbox", 
        width=.8, height=.8, 
        x=0, y=0, 
        bjust = c(0,0),
        txt_gp = gpar(col="darkblue", cex=2),
        box_gp = gpar(fill="lightyellow", col="darkblue"))

## ----prop_box, fig.height=2, fig.width=4---------------------------------
grid.newpage()
boxPropGrob("A box with proportions", 
            "Left side", "Right side",
            prop=.7)

## ---- fig.height=3, fig.width=4------------------------------------------
grid.newpage()
smpl_bx <- boxGrob(
  label = "A simple box",
  x = .5,
  y = .9,
  just = "center")

prop_bx <- boxPropGrob(
  label = "A split box",
  label_left = "Left side",
  label_right = "Right side",
  x = .5,
  y = .3,
  prop = .3,
  just = "center")

plot(smpl_bx)
plot(prop_bx)

smpl_bx_coords <- attr(smpl_bx, "coords")
grid.circle(y = smpl_bx_coords$y, x= smpl_bx_coords$x, r = unit(2, "mm"), gp=gpar(fill="#FFFFFF99", col="black"))
grid.circle(y = smpl_bx_coords$bottom, x= smpl_bx_coords$right, r = unit(1, "mm"), gp=gpar(fill="red"))
grid.circle(y = smpl_bx_coords$top, x = smpl_bx_coords$right, r = unit(1, "mm"), gp=gpar(fill="purple"))
grid.circle(y = smpl_bx_coords$bottom, x = smpl_bx_coords$left, r = unit(1, "mm"), gp=gpar(fill="blue"))
grid.circle(y = smpl_bx_coords$top, x = smpl_bx_coords$left, r = unit(1, "mm"), gp=gpar(fill="orange"))

prop_bx_coords <- attr(prop_bx, "coords")
grid.circle(y = prop_bx_coords$y, x= prop_bx_coords$x, r = unit(2, "mm"), gp=gpar(fill="#FFFFFF99", col="black"))
grid.circle(y = prop_bx_coords$bottom, x= prop_bx_coords$right_x, r = unit(1, "mm"), gp=gpar(fill="red"))
grid.circle(y = prop_bx_coords$top, x = prop_bx_coords$right_x, r = unit(1, "mm"), gp=gpar(fill="purple"))
grid.circle(y = prop_bx_coords$bottom, x = prop_bx_coords$left_x, r = unit(1, "mm"), gp=gpar(fill="blue"))
grid.circle(y = prop_bx_coords$top, x = prop_bx_coords$left_x, r = unit(1, "mm"), gp=gpar(fill="orange"))

grid.circle(y = prop_bx_coords$bottom, x= prop_bx_coords$right, r = unit(2, "mm"), gp=gpar(fill="red"))
grid.circle(y = prop_bx_coords$top, x = prop_bx_coords$right, r = unit(2, "mm"), gp=gpar(fill="purple"))
grid.circle(y = prop_bx_coords$bottom, x = prop_bx_coords$left, r = unit(2, "mm"), gp=gpar(fill="blue"))
grid.circle(y = prop_bx_coords$top, x = prop_bx_coords$left, r = unit(2, "mm"), gp=gpar(fill="orange"))


## ----"Connected boxes", fig.width=7, fig.height=5------------------------
grid.newpage()

# Initiate the boxes that we want to connect
side <- boxPropGrob("Side", "Left", "Right", 
                    prop=.3, 
                    x=0, y=.9,
                    bjust = c(0,1))
start <- boxGrob("Top", 
                 x=.6, y=coords(side)$y, 
                 box_gp = gpar(fill = "yellow"))
bottom <- boxGrob("Bottom", x=.6, y=0, 
                  bjust="bottom")


sub_side_left <- boxGrob("Left", 
                         x = coords(side)$left_x, 
                         y = 0,
                         bjust = "bottom")
sub_side_right <- boxGrob("Right", 
                          x = coords(side)$right_x, 
                          y = 0,
                          bjust = "bottom")

odd <- boxGrob("Odd\nbox", 
               x=coords(side)$right, 
               y=.5)

odd2 <- boxGrob("Also odd", 
               x=coords(odd)$right + 
                 distance(bottom, odd, type="h", half=TRUE) -
                 unit(2, "mm"), 
               y=0,
               bjust = c(1,0))

exclude <- boxGrob("Exclude:\n - Too sick\n - Prev. surgery", 
                   x=1, y=coords(bottom)$top + 
                     distance(start, bottom, 
                              type="v", half=TRUE), 
                   just="left", bjust = "right")

# Connect the boxes and print/plot them
connectGrob(start, bottom, "vertical")
connectGrob(start, side, "horizontal")
connectGrob(bottom, odd, "Z", "l")
connectGrob(odd, odd2, "N", "l")
connectGrob(side, sub_side_left, "v", "l")
connectGrob(side, sub_side_right, "v", "r")
connectGrob(start, exclude, "-", 
            lty_gp = gpar(lwd=2, col="darkred", fill="darkred"))

# Print the grobs
start
bottom
side
exclude
sub_side_left
sub_side_right
odd
odd2

