##################
# Knitr settings #
##################

knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  echo = FALSE,
  dpi = 96,
  fig.width = 4, fig.height = 4, # Default figure widths
  dev = "png", dev.args = list(type = "cairo"), # The png device
  # Change to dev = "postscript" if you want the EPS-files
  # for submitting. Also remove the dev.args() as the postscript
  # doesn't accept the type = "cairo" argument.
  error = FALSE
)

# Evaluate the figure caption after the plot
knitr::opts_knit$set(eval.after = "fig.cap")

# Use the table counter that the htmlTable() provides
options(table_counter = TRUE)

#################
# Load_packages #
#################
library(rms) # I use the cox regression from this package
library(boot) # The melanoma data set is used in this exampe
library(Gmisc) # Stuff I find convenient
library(Greg) # You need to get this from my GitHub see http://gforge.se/Gmisc

##################
# Munge the data #
##################

# Here we go through and setup the variables so that
# they are in the proper format for the actual output

# Load the dataset - usually you would use read.csv
# or something similar
data("melanoma")

# Set time to years instead of days
melanoma$time_years <-
  melanoma$time / 365.25

# Factor the basic variables that
# we're interested in
melanoma$status <-
  factor(melanoma$status,
    levels = c(2, 1, 3),
    labels = c(
      "Alive", # Reference
      "Melanoma death",
      "Non-melanoma death"
    )
  )
melanoma$sex <-
  factor(melanoma$sex,
    labels = c(
      "Male", # Reference
      "Female"
    )
  )

melanoma$ulcer <-
  factor(melanoma$ulcer,
    levels = 0:1,
    labels = c(
      "Absent", # Reference
      "Present"
    )
  )