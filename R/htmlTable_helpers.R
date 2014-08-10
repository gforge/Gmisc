#' Gets the table counter string
#'
#' Returns the string used for htmlTable to number the different tables.
#' Uses options \code{table_counter}, \code{table_counter_str},
#' and \code{table_counter_roman} to produce the final string. You
#' can set each option by simply calling \code{options()}.
#'
#' @return \code{string} Returns a string formatted according to
#'  the table_counter_str and table_counter_roman. The number is
#'  decided by the table_counter variable
#' @keywords internal
#' @family hidden helper functions for \code{\link{htmlTable}}
prHtGetTableCntr <- function () {
  tc <- getOption("table_counter")
  if (is.null(tc)){
    return("")
  }

  # Count which table it currently is
  if (is.numeric(tc))
    tc <- tc + 1
  else
    tc <- 1
  options(table_counter = tc)
  table_template <- getOption("table_counter_str", "Table %s: ")
  sprintf(table_template, ifelse(getOption("table_counter_roman", FALSE),
                                 as.character(as.roman(tc)),
                                 as.character(tc)))
}

#' Add a ; at the end
#'
#' The CSS expects a semicolon at the end of each argument
#' this function just adds a semicolong if none is given
#' and remove multiple semicolon if such exist
#'
#' @param my_str The string that is to be processed
#' @return \code{string}
#' @keywords internal
#' @family hidden helper functions for \code{\link{htmlTable}}
prHtAddSemicolon2StrEnd <- function(my_str){
  my_str <- str_trim(my_str)
  if(my_str == "")
    return(my_str)

  if (tail(strsplit(my_str, "")[[1]], 1) != ";")
    my_str <- sprintf("%s;", my_str)

  # Remove duplicated ;
  my_str <- gsub(";;+", ";", my_str)
  if (my_str == ";")
    return("")

  return (my_str)
}

#' Retrieve a header row
#'
#' This function retrieves a header row, i.e. a row
#' within the <th> elements on top of the table. Used by
#' \code{\link{htmlTable}}.
#'
#' @param cgroup_vec The cgroup may be a matrix, this is
#'  just one row of that matrix
#' @param n.cgroup_vec The same as above but for the counter
#' @param cgroup_vec.just The same as above bot for the justificaiton
#' @param row_no The row number within the header group. Useful for multirow
#'  headers when we need to output the rowlabel at the \code{rowlabel.pos}
#'  level.
#' @param top_row_style The top row has a special style depending on
#'  the \code{ctable} option in the \code{htmlTable} call.
#' @param cgroup_spacer_cells The spacer cells due to the multiple cgroup levels.
#'  With multiple rows in cgroup we need to keep track of how many spacer cells
#'  occur between the columns. This variable contains is of the size \code{ncol(x)-1}
#'  and 0 if there is no cgroup element between.
#' @return \code{string}
#' @keywords internal
#' @inheritParams htmlTable
#' @family hidden helper functions for \code{\link{htmlTable}}
prHtGetCgroupHeader <- function(x,
                                cgroup_vec, n.cgroup_vec, cgroup_vec.just,
                                row_no, top_row_style,
                                rowname,
                                rowlabel, rowlabel.pos,
                                cgroup_spacer_cells){

  header_str <- "\n\t<tr>"
  if (row_no == 1)
    ts <- top_row_style
  else
    ts <- ""

  if (!missing(rowlabel)){
    if (row_no == rowlabel.pos)
      header_str <- sprintf("%s\n\t\t<th style='font-weight: 900; %s'>%s</th>",
                            header_str, ts, rowlabel)
    else
      header_str <- sprintf("%s\n\t\t<th style='%s'></th>",
                            header_str, ts)
  }else if (!missing(rowname)){
    header_str <- sprintf("%s\n\t\t<th style='%s'></th>",
                          header_str, ts)
  }

  for (i in 1:length(cgroup_vec)){
    if (!is.na(n.cgroup_vec[i])){
      start_column <- ifelse(i == 1,
                             1,
                             sum(n.cgroup_vec[1:(i-1)], na.rm=TRUE) + 1)

      # 10 3-1
      # 0 0 1
      colspan <- n.cgroup_vec[i] +
        ifelse(start_column > length(cgroup_spacer_cells) ||
                 n.cgroup_vec[i] == 1,
               0,
               ifelse(start_column == 1,
                      sum(cgroup_spacer_cells[1:(n.cgroup_vec[i]-1)]),
                      ifelse(sum(n.cgroup_vec[1:i], na.rm=TRUE) == ncol(x),
                             sum(cgroup_spacer_cells[start_column:length(cgroup_spacer_cells)]),
                             sum(cgroup_spacer_cells[start_column:((start_column-1) + (n.cgroup_vec[i]-1))]))))

      if (nchar(cgroup_vec[i]) == 0)# Removed as this may now be on purpose || is.na(cgroup_vec[i]))
        header_str <- sprintf("%s\n\t\t<th colspan='%d' style='%s'></th>",
                              header_str, colspan,
                              prHtAddAlign2Style(sprintf("font-weight: 900; %s", ts),
                                             prHtGetAlign(strsplit(cgroup_vec.just, '')[[1]][i])))
      else
        header_str <- sprintf("%s\n\t\t<th colspan='%d' style='font-weight: 900; border-bottom: 1px solid grey; %s'>%s</th>",
                              header_str, colspan, ts, cgroup_vec[i])

      # If not last then add a filler cell between the row categories
      # this is also the reason that we need the cgroup_spacer_cells
      if (i != sum(!is.na(cgroup_vec)))
        header_str <- sprintf("%s<th style='%s; border-bottom: hidden;'>&nbsp;</th>",
                              header_str, ts)
    }
  }
  header_str <- sprintf("%s\n\t</tr>", header_str)
  return(header_str)
}

#' Adds text align to style element
#'
#' @param style The CSS style for an element
#' @param align The alignment for that element
#' @return \code{string} Returns the style with the
#'  alignment sett according to the align argument
#' @keywords internal
#' @family hidden helper functions for \code{\link{htmlTable}}
prHtAddAlign2Style <- function(style, align){
  if (grepl("text-align", style)){
    # Change existing to the actual align
    # test with: style <- "font-size: 1em; text-align: left; text-decoration: none;"
    return(gsub("text-align[ ]*:([^;]+)",
                paste0("text-align: ", align),
                style))
  }else{
    # If there is no style then we don't need to worry about
    # adding the align information.
    if (nchar(style) == 0)
      return(sprintf("text-align: %s;", align))

    # Add ; at end if not already there
    if (!grepl(";$", style))
      style <- sprintf("%s;", style)
    return(sprintf("%s text-align: %s;", style, align))
  }
}

#' Prepares the cgroup argument
#'
#' Due to the complicated structure of multilevel cgroups there
#' some preparation for the cgroup options is required.
#'
#' @inheritParams htmlTable
#' @return \code{list(cgroup, n.cgroup, cgroup_spacer_cells)}
#' @keywords internal
#' @family hidden helper functions for \code{\link{htmlTable}}
prHtPrepareCgroup <- function(x, cgroup, n.cgroup, cgroup.just){
  cgroup_spacer_cells <- rep(0, times=(ncol(x)-1))

  # The cgroup is by for compatibility reasons handled as a matrix
  if (!is.matrix(cgroup)){

    cgroup <- matrix(cgroup, nrow=1)
    if (missing(n.cgroup))
      n.cgroup <- matrix(NA, nrow=1)
    else{
      if (any(n.cgroup < 1)){
        warning("You have provided cgroups with less than 1 element,",
                " these will therefore be removed: ",
                paste(sprintf("'%s' = %d", cgroup, n.cgroup)[n.cgroup < 1],
                      collapse=", "))
        cgroup <- cgroup[,n.cgroup >= 1, drop=FALSE]
        n.cgroup <- n.cgroup[n.cgroup >= 1]
      }

      if (ncol(cgroup) != length(n.cgroup)){
        n.cgroup <- n.cgroup[n.cgroup > 0]
        if (ncol(cgroup) != length(n.cgroup))
          stop("You have provided invalid n.cgroup,",
               " it should have the same length as the cgroup (", ncol(cgroup), ")",
               " but it has the length of ", length(n.cgroup))
      }
      n.cgroup <- matrix(n.cgroup, nrow=1)
    }
  }else if(missing(n.cgroup)){
    stop("If you specify the cgroup argument as a matrix you have to",
         " at the same time specify the n.cgroup argument.")
  }

  if (missing(cgroup.just)){
    cgroup.just <- matrix(paste(rep("c", times=length(n.cgroup)), collapse=""),
                          nrow=nrow(n.cgroup))
  }else{
    if (!is.matrix(cgroup.just))
      cgroup.just <- matrix(cgroup.just, ncol=1)

    if (nrow(cgroup.just) != nrow(n.cgroup))
      stop("You have different dimensions for your cgroup.just and your cgroups, ",
           nchar(sub("\\|", "", cgroup.just[1,1])), "x", ncol(cgroup.just), " for the just while the cgroup has ",
           nrow(cgroup), "x", ncol(cgroup))

    # An old leftover behaviour from the latex() function
    if (ncol(cgroup.just) > 1)
      cgroup.just <- as.matrix(apply(cgroup.just, 1, function(x) paste(ifelse(is.na(x), "", x), collapse="")), ncol=1)

    discrepancies <- which(apply(cgroup.just, 1, function(x) nchar(sub("|", "", x))) != rowSums(!is.na(cgroup)))
    if (length(discrepancies) > 0)
      stop("You seem to have different number of justifications in your cgroup.just as compared to your cgroup variable.",
           " There is a discrepancy regarding rows: ", paste(discrepancies, collapse=", "))

  }

  # Go bottom up as the n.cgroup can be based on the previous
  # n.cgroup row.
  for (i in nrow(cgroup):1){
    # The row is empty and filled with NA's then we check
    # that it is possible to evenly split the cgroups among
    # the columns of the table
    if (all(is.na(n.cgroup[i,])) &&
          ncol(x) %% length(cgroup[i,]) == 0){
      # This generates the n.cgroup if this is missing
      n.cgroup[i,] <- rep(ncol(x)/length(cgroup[i,]), times=length(cgroup[i,]))
    }else if(any(n.cgroup[i,!is.na(n.cgroup[i,])] < 1)){
      stop("You have in n.cgroup row no ", i, " cell(s) with < 1")

    }else if(sum(n.cgroup[i,], na.rm=TRUE) != ncol(x)){
      ncgroupFixFromBelowGroup <- function(nc, i){
        if (i+1 > nrow(nc))
          stop("You have provided an invalid nc",
               " where it has fewer rows than the one of interest")

        # Select those below that are not missing
        row_below <- nc[i + 1, !is.na(nc[i + 1, ])]
        # The first position to start
        start_pos <- 1
        # This is a slightly complicated run that took a while to figure out
        # and I'm still afraid of ever having to debug this section.
        for (ii in 1:ncol(nc)){
          if (!is.na(nc[i, ii])){
            # Need to find where to begin tha addition
            pos <- ifelse(any(start_pos > cumsum(row_below)),
                          tail(which(start_pos > cumsum(row_below)), 1) + 1,
                          1)
            # Change the value to the rows below values that add up to this row
            # if the nc value is 1 and start position is 1 -> 1:(1+1-1) -> 1:1 -> 1
            # if the nc value is 2 and start position is 2 -> 2:(2+2-1) -> 2:3
            # if the nc value is 2 and start position is 1 -> 1:(1+2-1) -> 1:2
            nc[i, ii] <- sum(row_below[pos:(pos + nc[i, ii] - 1)])
            # Update the new start position:
            # if first run and nc is 2 then 1 + 2 -> 3 i.e.
            # next run the start_pos is 3 and lets say that nc is 3 then 3 + 3 -> 6
            start_pos <- start_pos + nc[i, ii]
          }
        }

        # Return the full object
        return(nc)
      }

      # This grouping can be based upon the next row
      if (i < nrow(cgroup) &&
            sum(n.cgroup[i, ], na.rm = TRUE) == sum(!is.na(n.cgroup[i + 1, ])))
      {
        n.cgroup <- ncgroupFixFromBelowGroup(n.cgroup, i)
      }else{
        stop(sprintf("Your columns don't match in the n.cgroup for the %d header row, i.e. %d != %d",
                     i,
                     sum(n.cgroup[i,], na.rm=TRUE),
                     ncol(x)))
      }
    }

    if (!all(is.na(n.cgroup[i, ]) == is.na(cgroup[i, ]))){
      stop("On header row (the cgroup argument) no ", i,
           " you fail to get the NA's matching.",
           "\n  The n.cgroup has elements no:",
           sprintf(" '%s'", paste(which(is.na(n.cgroup[i, ])), collapse=", ")),
           " missing while cgroup has elements no:",
           sprintf(" '%s'", paste(which(is.na(cgroup[i, ])), collapse=", ")),
           " missing.",
           "\n If the NA's don't occur at the same point",
           " the software can't decide what belongs where.",
           "\n The full cgroup row: ", paste(cgroup[i, ], collapse=", "),
           "\n The full n.cgroup row: ", paste(n.cgroup[i, ], collapse=", "),
           "\n Example: for a two row cgroup it would be:",
           " n.cgroup = rbind(c(1, NA), c(2, 1)) and",
           " cgroup = rbind(c('a', NA), c('b', 'c'))")
    }

    # Add a spacer cell for each cgroup. If two cgroups
    # on different rows have the same separation then it
    # is enough to have one spacer.
    for (ii in 1:(length(n.cgroup[i, ])-1)){
      if (!is.na(n.cgroup[i, ii]) && sum(n.cgroup[i, 1:ii], na.rm=TRUE) <= length(cgroup_spacer_cells))
        cgroup_spacer_cells[sum(n.cgroup[i, 1:ii], na.rm=TRUE)] <- 1
    }
  }
  return(list(cgroup = cgroup,
              n.cgroup = n.cgroup,
              cgroup_spacer_cells = cgroup_spacer_cells))
}

#' Gets the rowlabel position
#'
#' @inheritParams htmlTable
#' @return \code{integer} Returns the position within the header rows
#'  to print the \code{rowlabel} argument
#' @keywords internal
#' @family hidden helper functions for \code{\link{htmlTable}}
prHtGetRowlabelPos <- function (cgroup, rowlabel.pos, headings) {
  no_cgroup_rows <-
    ifelse(!missing(cgroup),
           nrow(cgroup),
           0)
  no_header_rows <-
    no_cgroup_rows +
    (!missing(headings))*1
  if (is.numeric(rowlabel.pos)){
    if(rowlabel.pos < 1)
      stop("You have specified a rowlabel.pos that is less than 1: ", rowlabel.pos)
    else if (rowlabel.pos > no_header_rows)
      stop("You have specified a rowlabel.pos that more than the max limit, ",
           no_header_rows,
           ", you have provided: ", rowlabel.pos)
  }else{
    rowlabel.pos <- tolower(rowlabel.pos)
    if (rowlabel.pos %in% c("top"))
      rowlabel.pos <- 1
    else if (rowlabel.pos %in% c("bottom", "header"))
      rowlabel.pos <- no_header_rows
    else
      stop("You have provided an invalid rowlabel.pos text,",
           " only 'top', 'bottom' or 'header' are allowed,",
           " can't interpret '", rowlabel.pos, "'")
  }

  return(rowlabel.pos)
}

#' Add a cell
#'
#' Adds a row of cells <td>val</td><td>...</td> to a table string for
#' \code{\link{htmlTable}}
#'
#' @inheritParams htmlTable
#' @param table_str The string to add the cell to
#' @param rowcells The cells with the values that are to be added
#' @param cellcode Type of cell, can either be \code{th} or \code{td}
#' @param style The cell style
#' @param cgroup_spacer_cells The number of cells that occur between
#'  columns due to the cgroup arguments.
#' @return \code{string} Returns the string with the new cell elements
#' @keywords internal
#' @family hidden helper functions for \code{\link{htmlTable}}
prHtAddCells <- function(table_str, rowcells, cellcode, align, style, cgroup_spacer_cells){
  style = prHtAddSemicolon2StrEnd(style)

  for (nr in 1:length(rowcells)){
    cell_value <- rowcells[nr]
    # We don't want missing to be NA in a table, it should be empty
    if (is.na(cell_value))
      cell_value <- ""

    table_str <- sprintf("%s\n\t\t<%s style='%s'>%s</%s>",
                         table_str, cellcode, prHtAddAlign2Style(style, align[nr]), cell_value, cellcode)

    # Add empty cell if not last column
    if (nr != length(rowcells) && cgroup_spacer_cells[nr] > 0){
      table_str <- sprintf("%s\n\t\t<%s style='%s' colspan='%d'>&nbsp;</%s>",
                           table_str, cellcode, style, cgroup_spacer_cells[nr], cellcode)
    }
  }
  return (table_str)
}

#' Gets alignment
#'
#' AVI added utility function to incorporate custom (LaTeX) column alignment
#'
#' @param align_req The align option from \code{\link{htmlTable}}
#' @family hidden helper functions for
#' @keywords internal
prHtGetAlign <- function(align_req) {
  tmp_align_req <- strsplit(align_req, "")[[1]]
  if (length(grep('[|]', tmp_align_req)) >0 ) { # Remove pipe(s) if they exist
    tmp_align_req <- tmp_align_req[-grep('[|]', tmp_align_req)]
  }
  sapply(tmp_align_req, function(f) c("center", "right", "left")[grep(f, c("c", "r", "l"))], USE.NAMES=FALSE)
}
