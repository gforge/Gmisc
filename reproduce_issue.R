
devtools::load_all()

# Create dummy boxes with 2 groups to avoid length=1 unpacking issue
b1 <- boxGrob("A")
b2 <- boxGrob("B")
fc <- list(g1 = list(b1), g2 = list(b2))
class(fc) <- c("Gmisc_list_of_boxes", "list")

# Try to spread using subelement
tryCatch({
  # We spread only one group
  # Note: spread returns the modified list
  res <- spread(fc, subelement = "g1", axis = "x")
  print("Success!")
}, error = function(e) {
  print(e)
})
