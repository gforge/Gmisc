##################
# Knitr settings #
##################

knitr::opts_chunk$set(warning=FALSE,
                      message=FALSE,
                      echo=FALSE,
                      dpi=96,
                      fig.width=4, fig.height=4, # Default figure widths
                      dev="png", dev.args=list(type="cairo"), # The png device
                      # Change to dev="postscript" if you want the EPS-files
                      # for submitting. Also remove the dev.args() as the postscript
                      # doesn't accept the type="cairo" argument.
                      error=FALSE)

# Evaluate the figure caption after the plot
knitr::opts_knit$set(eval.after='fig.cap')

# Add a figure counter function
knitr::knit_hooks$set(plot = function(x, options) {
  fig_fn = paste0(knitr::opts_knit$get("base.url"),
                  paste(x, collapse = "."))

  # Some stuff from the default definition
  fig.cap <- knitr:::.img.cap(options)

  # Style and additional options that should be included in the img tag
  style=c("display: block",
          sprintf("margin: %s;",
                  switch(options$fig.align,
                         left = 'auto auto auto 0',
                         center = 'auto',
                         right = 'auto 0 auto auto')))
  # Certain arguments may not belong in style,
  # for instance the width and height are usually
  # outside if the do not have a unit specified
  addon_args = ""

  # This is perhaps a little overly complicated prepared
  # with the loop but it allows for a more out.parameters if necessary
  if (any(grepl("^out.(height|width)", names(options)))){
    on <- names(options)[grep("^out.(height|width)", names(options))]
    for(out_name in on){
      dimName <- substr(out_name, 5, nchar(out_name))
      if (grepl("[0-9]+(em|px|%|pt|pc|in|cm|mm)", out_name))
        style=append(style, paste0(dimName, ": ", options[[out_name]]))
      else if (length(options$out.width) > 0)
        addon_args = paste0(addon_args, dimName, "='", options[[out_name]], "'")
    }
  }

  # Add counter if wanted
  fig_number_txt <- ""
  cntr <- getOption("figure_counter", FALSE)
  if (cntr != FALSE){
    if (is.logical(cntr))
      cntr <- 1
    # The figure_counter_str allows for custom
    # figure text, you may for instance want it in
    # bold: <b>Figure %s:</b>
    # The %s is so that you have the option of setting the
    # counter manually to 1a, 1b, etc if needed
    fig_number_txt <-
      sprintf(getOption("figure_counter_str", "Figure %s: "),
              ifelse(getOption("figure_counter_roman", FALSE),
                     as.character(as.roman(cntr)), as.character(cntr)))

    if (is.numeric(cntr))
      options(figure_counter = cntr + 1)
  }

  # Put it all together
  paste0("<figure><img src='", fig_fn, "'",
         " ", addon_args,
         paste0(" style='", paste(style, collapse="; "), "'"),
         ">",
         "<figcaption>", fig_number_txt, fig.cap, "</figcaption></figure>")
})

# Use the table counter that the htmlTable() provides
options(table_counter = TRUE)

# Use the figure counter that we declare below
options(figure_counter = TRUE)
# Use roman letters (I, II, III, etc) for figures
options(figure_counter_roman = TRUE)

# Adding the figure number is a little tricky when the format is roman
getNextFigureNo <- function() as.character(as.roman(as.numeric(options("figure_counter"))))

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
         levels=c(2, 1, 3),
         labels=c("Alive", # Reference
                  "Melanoma death",
                  "Non-melanoma death"))
melanoma$sex <-
  factor(melanoma$sex,
         labels=c("Male", # Reference
                  "Female"))

melanoma$ulcer <-
  factor(melanoma$ulcer,
         levels=0:1,
         labels=c("Absent", # Reference
                  "Present"))
