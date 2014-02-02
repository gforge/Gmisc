# This file contains all the helper funcitons that the outer exported
# functions utilize. I try to have a pr at the start of the name for all
# the private functions.
#
# Author: max
###############################################################################

#' Get statistics according to the type
#' 
#' A simple function applied by the \code{\link{getDescriptionStatsBy}}
#' for the total column. 
#' 
#' @param x The variable that we want the statistics for 
#' @param show_perc If this is a factor/proportion variable then we
#'  might want to show the percentages 
#' @param html If the output should be in html or LaTeX formatting
#' @param digits Number of decimal digits
#' @param numbers_first If number is to be prior to the percentage
#' @param show_missing If missing should be included 
#' @param show_all_values This is by default false as for instance if there is
#'  no missing and there is only one variable then it is most sane to only show 
#'  one option as the other one will just be a complement to the first. For instance
#'  sex - if you know gender then automatically you know the distribution of the 
#'  other sex as it's 100 \% - other \%. 
#' @param continuous_fn A function for describing continuous variables
#'  defaults to \code{\link{describeMean}} 
#' @param prop_fn A function for describing proportions, defaults to
#'  the factor function
#' @param factor_fn A function for describing factors, defaults to
#'  \code{\link{describeFactors}}
#' @param percentage_sign If you want to suppress the percentage sign you
#'  can set this variable to FALSE. You can also choose something else that
#'  the default % if you so wish by setting this variable.
#' @return A matrix or a vector depending on the settings
#' 
#' @author max
prGetStatistics <- function(x, 
  show_perc = FALSE, 
  html = TRUE, 
  digits = 1, 
  numbers_first = TRUE, 
  show_missing = TRUE, 
  show_all_values = FALSE,
  continuous_fn = describeMean, 
  factor_fn = describeFactors,
  prop_fn = factor_fn,
  percentage_sign = percentage_sign)
{
  show_missing <- prConvertShowMissing(show_missing)
  if (is.factor(x) || 
        is.logical(x) ||
        is.character(x)){
    if (length(unique(x)) == 2){
      if (show_perc){
        total_table <- prop_fn(x, 
            html=html, 
            digits=digits,
            number_first=numbers_first, 
            show_missing = show_missing,
            percentage_sign = percentage_sign)
      }else{
        total_table <- table(x, useNA=show_missing)
        names(total_table)[is.na(names(total_table))] <- "Missing"
        # Choose only the reference level
        if (show_all_values == FALSE)
          total_table <- total_table[names(total_table) %in% c(levels(x)[1], "Missing")]
      }
      
    } else {
      if (show_perc)
        total_table <- factor_fn(x, 
            html=html, 
            digits=digits,
            number_first=numbers_first, 
            show_missing = show_missing,
            percentage_sign = percentage_sign)
      else{
        total_table <- table(x, useNA=show_missing)
        names(total_table)[is.na(names(total_table))] <- "Missing"
      }
    }
  }else{
    total_table <- continuous_fn(x, 
      html=html, digits=digits, 
      number_first=numbers_first, 
      show_missing = show_missing)
    
    # If a continuous variable has two rows then it's assumed that the second is the missing
    if (length(total_table) == 2 &&
      show_perc == FALSE)
      total_table[2] <- sum(is.na(x))
  }
  return(total_table)
}


#' A functuon for converting a show_missing variable
#' 
#' The variable is suppose to be directly compatible with
#' table(..., useNA=show_missing). It throughs an error
#' if not compatible
#' 
#' @param show_missing Boolean or "no", "ifany", "always" 
#' @return string 
#' 
#' @author max
prConvertShowMissing <- function(show_missing){
  if (show_missing == FALSE || show_missing == "no")
    show_missing <- "no"
  else if (show_missing == TRUE)
    show_missing <- "ifany"
  
  if (!show_missing %in% c("no", "ifany", "always"))
    stop(sprintf("You have set an invalid option for show_missing variable, '%s' ,it should be boolean or one of the options: no, ifany or always.", show_missing))
  
  return(show_missing)
}

#' A helper function for the description stats
#' 
#' @param x The variable of interest with the levels
#' @param default_ref The default reference, either first,
#'  the level name or a number within the levels
#' @return integer The level number of interest 
#' 
#' @author max
prGetAndValidateDefaultRef <- function(x, default_ref){
  if (default_ref == "First"){
    default_ref <- 1
  }else if (is.character(default_ref)){
    if (default_ref %in% levels(x))
      default_ref <- which(default_ref == levels(x))
    else
      stop("You have provided an invalid default reference, '", 
        default_ref, "' can not be found among: ", paste(levels(x), collapse=", "))
  }else if (!default_ref %in% 1:length(levels(x)))
    stop("You have provided an invalid default reference,",
      " it is ", default_ref, " while it should be between 1 and ", length(levels(x)),
      " as this is only used for factors.")
  
  return(default_ref)
}

#' Pushes viewport with margins
#' 
#' A \code{\link[grid]{grid.layout}} object is used to
#' generate the margins. A second viewport selecting the
#' mid-row/col is used to create the effect of margins
#' 
#' @param bottom The margin object, either in npc or a \code{\link[grid]{unit}} object 
#' @param left The margin object, either in npc or a \code{\link[grid]{unit}} object
#' @param top The margin object, either in npc or a \code{\link[grid]{unit}} object
#' @param right The margin object, either in npc or a \code{\link[grid]{unit}} object
#' @param name The name of the last viewport
#' @return \code{NULL} 
#' 
#' @author max
prPushMarginViewport <- function(bottom, left, top, right, name=NULL){
  if (!is.unit(bottom))
    bottom <- unit(bottom, "npc")
  
  if (!is.unit(top))
    top <- unit(top, "npc")
  
  if (!is.unit(left))
    left <- unit(left, "npc")
  
  if (!is.unit(right))
    right <- unit(right, "npc")
  
  layout_name <- NULL
  if (!is.character(name))
    layout_name <- sprintf("margin_grid_%s", name)
  
  gl <- grid.layout(nrow=3, ncol=3,
    heights = unit.c(top, unit(1, "npc") - top - bottom, bottom),
    widths = unit.c(left, unit(1, "npc") - left - right, right))
  
  pushViewport(viewport(layout=gl, name=layout_name))
  pushViewport(viewport(layout.pos.row=2, layout.pos.col=2, name=name))
}

#' Adds a title to the plot
#' 
#' Adds the title and generates a new
#' main viewport below the title
#' 
#' @param title The title as accepted by \code{\link[grid]{textGrob}} 
#' @param base_cex The base cex used for the plot
#' @param cex_mult The multiplier of the base - i.e. the increase of the
#'  text size for the title as compared to the general
#' @param fontface The type of fontfacte
#' @param space_below The space below, defaults to 1/5 of the title height
#' @return \code{NULL} The function does not return a value 
#' 
#' @author max
prGridPlotTitle <- function(title, 
  base_cex, cex_mult = 1.2,
  fontface = "bold",
  space_below = NULL){
  titleGrob <- textGrob(title, just="center", 
    gp=gpar(fontface = fontface,
      cex = base_cex*cex_mult))
  
  # The y/g/j letters are not included in the height
  gh <- unit(convertUnit(grobHeight(titleGrob), "mm", valueOnly=TRUE)*1.5, "mm")
  if (is.null(space_below)){
    space_below <- unit(convertUnit(gh, "mm", valueOnly=TRUE)/3, "mm")
  }else if (!is.unit(space_below)){
    space_below <- unit(space_below, "npc")
  }
  
  gl <- grid.layout(nrow=3, ncol=1,
    heights = unit.c(gh, space_below, unit(1, "npc") - space_below - gh))
  
  pushViewport(viewport(layout=gl, name="title_layout"))
  pushViewport(viewport(layout.pos.row=1, name="title"))
  grid.draw(titleGrob)
  upViewport()
  
  pushViewport(viewport(layout.pos.row=3, name="main"))
}