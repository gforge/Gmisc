  #' This is a simple funciton for outputting a more advanced
  #' table than xtable allows. It's aim is to provide the Hmisc
  #' latex() colgroup and rowgroup funcitons.
  #' 
  #' @param x The matrix/data.frame with the data
  #' @param title The title of the table. Used for labelling etc.
  #' @param headings a vector of character strings specifying column 
  #'    headings, defaulting to \code{x}'s 	\code{colnames}
  #' @param align a character strings specifying column	alignments, defaulting to	
  #'    \code{paste(rep('c',ncol(x)),collapse='')} to center.  
  #'    You may specify \code{align='c|c'} and other LaTeX tabular formatting.
  #' @param halign a character strings specifying alignment for column headings, 
  #'    defaulting to centered.
  #' @param cgroup a vector of character strings defining major column headings. The default 
  #'    is to have none. This is also known as "the column spanner".
  #' @param n.cgroup  a vector containing the number of columns for which each element in
  #'    cgroup is a heading. For example, specify \code{cgroup=c("Major 1","Major 2")}, 
  #'    \code{n.cgroup=c(3,3)} if \code{"Major 1"} is to span columns 1-3 and 
  #'    \code{"Major 2"} is to span columns 4-6. 
  #'    \code{rowlabel} does not count in the column numbers. You can omit \code{n.cgroup} 
  #'    if all groups have the same number of columns.
  #' @param cgroup.just The justification of the c.groups
  #' @param rgroup A vector of character strings containing headings for row groups. 
  #'    \code{n.rgroup} must be present when \code{rgroup} is given. The first 
  #'    \code{n.rgroup[1]}rows are sectioned off and \code{rgroup[1]} is used as a bold 
  #'    heading for them. The usual row dimnames (which must be present if \code{rgroup} is) 
  #'    are indented. The next \code{n.rgroup[2]} rows are treated likewise, etc.
  #' @param n.rgroup integer vector giving the number of rows in each grouping. If \code{rgroup}
  #'    is not specified, \code{n.rgroup} is just used to divide off blocks of rows by horizontal 
  #'    lines. If \code{rgroup} is given but \code{n.rgroup} is omitted, \code{n.rgroup} will 
  #'    default so that each row group contains the same number of rows.
  #' @param rgroupCSSstyle Css style for the rgorup, if different styles are wanted for each of the
  #'    rgroups you can just specify a vector with the number of elements
  #' @param rgroupCSSseparator The line between different rgroups. The line is set to the TR element
  #'    of the lower rgroup, i.e. you have to set the border-top/padding-top etc to a line with
  #'    the expected function. This is only used for rgroups that are printed. You can specify
  #'    different separators if you give a vector of rgroup - 1 length (this is since the first
  #'    rgroup doesn't have a separator).
  #' @param rowlabel If x has row dimnames, rowlabel is a character string containing the
  #'    column heading for the row dimnames. The default is the name of the argument for x.
  #' @param rowname Default is rownames of matrix or data.frame.  Specify \code{rowname=NULL} 
  #'    to suppress the use of row names.
  #' @param caption a text string to use as a caption to print at the top of the first 
  #'    page of the table. Default is no caption.
  #' @param caption.loc set to \code{"bottom"} to position a caption below the table 
  #'    instead of the default of \code{"top"}.
  #' @param label a text string representing a symbolic label for the 
  #'    table for referencing as an anchor
  #' @param ctable If the table should have a double top border or a single a' la LaTeX ctable style
  #' @param ... Currently not used, here for compatibility reasons
  #' @param output Set to false if you don't want an immediate print
  #' @return Returns a string with the output table if output is not set
  #' 
  #' @example examples/htmlTable_example.R
  #' @author max
  #' @export
  htmlTable <- function(x,
    title=first.word(deparse(substitute(x))),
    headings=colnames(x), 
    align =paste(rep('c',ncol(x)),collapse=''),
    halign=paste(rep('c',ncol(x)),collapse=''),
    cgroup=NULL, n.cgroup=NULL,
    cgroup.just=rep("c",length(n.cgroup)),
    rgroup=NULL, n.rgroup=NULL,
    rgroupCSSstyle="font-weight: 900;",
    rgroupCSSseparator = "border-top: 1px solid grey;",
    rowlabel=title,
    ctable=FALSE,
    rowname=rownames(x),
    caption=NULL,
    caption.loc='top',
    label=title,
    output = TRUE,
    ...)
  {
    require("stringr")
    
    # The CSS expects a semicolon at the end of each argument
    # this function just adds a semicolong if none is given
    addSemicolon2StrEnd <- function(my_str){
      my_str <- str_trim(my_str)
      if (substr(my_str, nchar(my_str), nchar(my_str) + 1) != ";")
        my_str <- sprintf("%s;", my_str)
      
      return (my_str)
    }
    
    addCells <- function(table_str, rowcells, cellcode, align, style=""){
      cgroup_iterator <- 0
      style = addSemicolon2StrEnd(style)
      for (nr in 1:length(rowcells)){
        if (length(cgroup) > 0){
          if (cgroup_iterator > 0){
            if (sum(n.cgroup[1:cgroup_iterator]) < nr ){
              table_str <- sprintf("%s\n\t\t<%s style='%s'>&nbsp</%s>", table_str, cellcode, style, cellcode)
              cgroup_iterator = cgroup_iterator + 1
            }
          }else{
            cgroup_iterator = cgroup_iterator + 1
          }
        }
        
        table_str <- sprintf("%s\n\t\t<%s align='%s' style='%s'>%s</%s>", table_str, cellcode, align, style, rowcells[nr], cellcode)
      }
      return (table_str)
    }
    
    # Sanity checks rgroupCSSstyle and prepares the style
    if (length(rgroupCSSstyle) > 1 &&
      length(rgroupCSSstyle) != length(rgroup))
      stop(sprintf("You must provide the same number of styles as the rgroups, %d != %d",length(rgroupCSSstyle), length(rgroup)))
    else if(length(rgroupCSSstyle) == 1){
      rgroupCSSstyle <- addSemicolon2StrEnd(rgroupCSSstyle)
      
      if (length(rgroup) > 0)
        rgroupCSSstyle <- rep(rgroupCSSstyle, times=length(rgroup))
    } else {
      for (i in 1:length(rgroupCSSstyle))
        rgroupCSSstyle[i] <- addSemicolon2StrEnd(rgroupCSSstyle[i])
    }
    
    # Sanity checks rgroupCSSseparator and prepares the style
    if (length(rgroupCSSseparator) > 1 &&
      length(rgroupCSSseparator) != length(rgroup)-1)
      stop(sprintf("You must provide the same number of separators as the rgroups - 1, %d != %d",length(rgroupCSSseparator), length(rgroup)-1))
    else if(length(rgroupCSSseparator) == 1){
      rgroupCSSseparator <- addSemicolon2StrEnd(rgroupCSSseparator)
        
      if (length(rgroup) > 0)
        rgroupCSSseparator <- rep(rgroupCSSseparator, times=length(rgroup))
    } else {
      for (i in 1:length(rgroupCSSseparator))
        rgroupCSSseparator[i] <- addSemicolon2StrEnd(rgroupCSSseparator[i])
    }
    
    # Sanity check for rgroup
    if (length(rgroup) > 0 &&
      sum(n.rgroup) !=  nrow(x))
      stop(sprintf("Your rows don't match in the n.rgroup, i.e. %d != %d", sum(n.rgroup), nrow(x)))
    
    
    table_str <- "<table class='gmisc_table' style='border-collapse: collapse;'>"
    # Theoretically this should be added to the table but the
    # import to word processors works then less well and therefore I've
    # constructed this work-around with borders for the top and bottom cells
    first_row = TRUE;
    if (ctable){
      top_row_style = "border-top: 2px solid grey;"
      bottom_row_style = "border-bottom: 2px solid grey;"
    } else {
      top_row_style = "border-top: 4px double grey;"
      bottom_row_style = "border-bottom: 1px solid grey;"
    }
  
    if (length(label) > 0){
      table_str <- sprintf("%s\n\t<a name='%s'></a>", table_str, label) 
    }
    
    # Not quite as intended but close enough
    if(length(list(...))) x <- format.df(x, numeric.dollar=FALSE, ...)
    # Remove some specifics for LaTeX
    if (is.character(x))
      x <- matrix(str_replace(x, "\\\\%", "%"), ncol=ncol(x))
    
    if (length(caption) > 0){
      if (caption.loc == "bottom"){
        table_str <- sprintf("%s\n\t<caption align=bottom>", table_str)
      }else{
        table_str <- sprintf("%s\n\t<caption align=top>", table_str)
      }
      table_str <- sprintf("%s%s</caption>", table_str, caption)
    }
    
    if (length(rowname) > 0)
      set_rownames <- TRUE
    else
      set_rownames <- FALSE
    
    # Start the head
    table_str <- sprintf("%s\n\t<thead>", table_str)
  
    # Add the cgroup table header
    if (length(cgroup) > 0){
      if (length(n.cgroup) == 0 && ncol(x) %% length(cgroup) == 0){
        n.cgroup <- rep(ncol(x)/length(cgroup), times=length(cgroup))
      }else if(sum(n.cgroup) != ncol(x)){
        stop(sprintf("Your columns don't match in the n.cgroup, i.e. %d != %d", sum(n.cgroup), ncol(x)))
      }
      
      table_str <- sprintf("%s\n\t<tr>", table_str)
      if (set_rownames && length(rowlabel) > 0){
          table_str <- sprintf("%s\n\t\t<th style='font-weight: 900; %s'>%s</th>", 
                              table_str, top_row_style, rowlabel)
      }
      
      for (i in 1:length(cgroup)){
        table_str <- sprintf("%s\n\t\t<th colspan=%d style='font-weight: 900; border-bottom: 1px solid grey; %s'>%s</th>", 
                        table_str, n.cgroup[i], top_row_style, cgroup[i])
        if (i != length(cgroup))
          table_str <- sprintf("%s<th style='%s'>&nbsp;</th>", table_str, top_row_style)
      }
      first_row = FALSE
      table_str <- sprintf("%s\n\t</tr>", table_str)
    }
    
  
    # Add the headings
    if (length(headings) > 0){
      # The bottom border was ment to be here but it doesn't
      # work that well in the export
      table_str <- sprintf("%s\n\t<tr>", table_str)
      if (set_rownames && length(cgroup) == 0  && length(rowlabel) > 0){
        table_str <- sprintf("%s\n\t\t<th style='font-weight: 900; border-bottom: 1px solid grey; %s'>%s</th>", 
                  table_str, top_row_style, rowlabel)
      }else if(set_rownames){
        table_str <- sprintf("%s\n\t\t<th style='border-bottom: 1px solid grey;'>&nbsp;</th>", table_str)
      }
        
      cell_style= "border-bottom: 1px solid grey;" 
      if (first_row){
        cell_style=sprintf("%s %s", cell_style, top_row_style)
      }
      table_str <- addCells(table_str = table_str, rowcells = headings, cellcode = "th", 
                          align="center", style=cell_style)
      
      table_str <- sprintf("%s\n\t</tr>", table_str)
      first_row = FALSE
    }
    
    # close head and start the body
    table_str <- sprintf("%s\n\t</thead><tbody>", table_str)
    
    rgroup_iterator <- 0
    for (row_nr in 1:nrow(x)){
      if (length(rgroup) > 0){
        if (rgroup_iterator == 0){
          rgroup_iterator = rgroup_iterator + 1
          if (is.na(rgroup[rgroup_iterator]) == FALSE &&
            rgroup[rgroup_iterator] != "")
            table_str <- sprintf("%s\n\t<tr><td colspan=%d style='%s'>%s</tr>", table_str, 
              ncol(x)+set_rownames,
              rgroupCSSstyle[rgroup_iterator],
              rgroup[rgroup_iterator])
        }else if(row_nr > sum(n.rgroup[1:rgroup_iterator])){
          rgroup_iterator = rgroup_iterator + 1
          if (is.na(rgroup[rgroup_iterator]) == FALSE &&
            rgroup[rgroup_iterator] != "")
            table_str <- sprintf("%s\n\t<tr><td colspan=%d style='%s %s'>%s</tr>", table_str, 
                               ncol(x)+set_rownames, 
                               rgroupCSSstyle[rgroup_iterator],
                               rgroupCSSseparator[rgroup_iterator-1],
                               rgroup[rgroup_iterator])
        }
      }
      
      table_str <- sprintf("%s\n\t<tr>", table_str)
      cell_style = "";
      if (row_nr == nrow(x))
        cell_style = bottom_row_style
      
      if (set_rownames){
        # Minor change from original function. If the group doesn't have 
        # a group name then there shouldn't be any indentation
        if (rgroup_iterator > 0 && 
          is.na(rgroup[rgroup_iterator]) == FALSE &&
          rgroup[rgroup_iterator] != ""){
          
          
          # The padding doesn't work well with the Word import - well nothing really works well with word...
          # table_str <- sprintf("%s\n\t\t<td style='padding-left: .5em;'>%s</td>", table_str, rowname[row_nr])
          table_str <- sprintf("%s\n\t\t<td style='%s'>&nbsp;&nbsp;%s</td>", 
                              table_str, cell_style, rowname[row_nr])
        }else
          table_str <- sprintf("%s\n\t\t<td style='%s'>%s</td>", 
                               table_str, cell_style, rowname[row_nr])
      }
      
      table_str <- addCells(table_str = table_str, rowcells = x[row_nr,], 
                            cellcode = "td", align="right", style = cell_style)
      table_str <- sprintf("%s\n\t</tr>", table_str)
    }
    
    # Close body
    table_str <- sprintf("%s\n\t</tbody>", table_str)
    # Close table
    table_str <- sprintf("%s\n</table>", table_str)
    if (output){
      cat(table_str)
    }else{
      return(table_str)
    }
  }