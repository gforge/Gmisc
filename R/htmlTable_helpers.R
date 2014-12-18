#' Gets the table counter string
#'
#' Returns the string used for htmlTable to number the different tables.
#' Uses options \code{table_counter}, \code{table_counter_str},
#' and \code{table_counter_roman} to produce the final string. You
#' can set each option by simply calling \code{options()}.
#'
#' @param The caption
#' @return \code{string} Returns a string formatted according to
#'  the table_counter_str and table_counter_roman. The number is
#'  decided by the table_counter variable
#' @keywords internal
#' @family hidden helper functions for \code{\link{htmlTable}}
prHtTblNo <- function (caption) {
  tc <- getOption("table_counter")
  if (is.null(tc)){
    if (missing(caption))
      return("")
    else
      return(caption)
  }

  # Count which table it currently is
  if (is.numeric(tc))
    tc <- tc + 1
  else
    tc <- 1
  options(table_counter = tc)
  table_template <- getOption("table_counter_str", "Table %s: ")
  out <- sprintf(table_template,
                 ifelse(getOption("table_counter_roman", FALSE),
                        as.character(as.roman(tc)),
                        as.character(tc)))
  if (!missing(caption))
    out <- paste(out, caption)

  return(out)
}

#' Gets the CSS style element
#'
#' A funciton for checking, merging, and more
#' with a variety of different style formats.
#'
#' @param styles The styles can be provided as \code{vector},
#'  \code{named vector}, or \code{string}.
#' @param ... All styles here are merged with the first parameter.
#'  If you provide a name, e.g. \code{styles="background: blue", align="center"}
#'  the function will convert the \code{align} into proper \code{align: center}.
#' @return \code{string} Returns the codes merged into one string with
#'  correct CSS ; and : structure.
#' @keywords internal
#' @family hidden helper functions for \code{\link{htmlTable}}
prHtGetStyle <- function(styles, ...){
  mergeNames <- function(sv){
    if (!is.null(names(sv))){
      sv <- paste(names(sv), sv, sep=": ")
    }
    return(sv)
  }
  spltNames <- function(sv){
    ret_sv <- c()
    for (i in 1:length(sv))
      ret_sv <- c(ret_sv,
                  # Split on the ; in case it is not at the end/start
                  unlist(strsplit(sv[i], "\\b;(\\b|\\W+)", perl=TRUE)))
    return(ret_sv)
  }

  styles <- spltNames(mergeNames(styles))
  dots <- list(...)
  if (length(dots) > 0){
    for (i in 1:length(dots)){
      if (is.null(names(dots[[i]]))){
        if  (!is.null(names(dots)) &&
               names(dots)[i] != "" &&
               !grepl("\\b[;:](\\b|\\W+)", dots[[i]], perl=TRUE))
          dots[[i]] = paste0(names(dots)[i], ": ", dots[[i]])
      }else{
        dots[[i]] = mergeNames(dots[[i]])
      }
      styles <- c(styles,
                  spltNames(dots[[i]]))

    }
  }

  if (!all(grepl("^[^:]+:.+", styles)))
    stop("Invalid styles detected, one or more styles lack the needed style 'name: value': ",
         paste(paste0("'", styles[!grepl("^[^:]+:.+", styles)], "'"), collapse=", "))

  # Remove empty background colors
  if (any(grepl("^background-color: none", styles))){
    styles <- styles[-grep("^background-color: none", styles)]
  }

  # Merge background colors
  if (sum(grepl("^background-color:", styles)) == 2){
    clrs <- styles[grep("^background-color:", styles)]
    clrs <- gsub("^background-color:[ ]*([^;]+);*", "\\1", clrs)
    # Pick a color merge
    styles <- styles[-grep("^background-color:", styles)]
    styles <-
      c(styles,
        paste0("background-color: ",
               colorRampPalette(clrs)(3)[2]))
  }

  style_names <- gsub("^([^:]+).+", "\\1", styles)
  if (!any(duplicated(style_names))){
    unique_styles <- styles
  }else{
    # Only select the last style if two of the same type
    # exist. This in order to avoid any conflicts.
    unique_styles <- c()
    for(n in unique(style_names)){
      unique_styles <-
        c(unique_styles,
          styles[max(which(n == style_names))])
    }
  }

  unique_styles <- sapply(unique_styles, prHtAddSemicolon2StrEnd, USE.NAMES = FALSE)
  paste(unique_styles, collapse=" ")
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
#'  headers when we need to output the rowlabel at the \code{pos.rowlabel}
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
                                rnames,
                                rowlabel, pos.rowlabel,
                                cgroup_spacer_cells){

  header_str <- "\n\t<tr>"
  if (row_no == 1)
    ts <- top_row_style
  else
    ts <- ""

  if (!missing(rowlabel)){
    if (row_no == pos.rowlabel)
      header_str <- sprintf("%s\n\t\t<th style='font-weight: 900; %s'>%s</th>",
                            header_str, ts, rowlabel)
    else
      header_str <- sprintf("%s\n\t\t<th style='%s'></th>",
                            header_str, ts)
  }else if (!prHtSkipRownames(rnames)){
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
                              prHtGetStyle(c(`font-weight`=900),
                                           ts,
                                           align=prHtGetAlign(cgroup_vec.just, i)))
      else
        header_str <- sprintf("%s\n\t\t<th colspan='%d' style='%s'>%s</th>",
                              header_str, colspan,
                              prHtGetStyle(c(`font-weight`=900,
                                             `border-bottom`="1px solid grey"),
                                           ts,
                                           align=prHtGetAlign(cgroup_vec.just, i)),
                              cgroup_vec[i])

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

#' Prepares the cgroup argument
#'
#' Due to the complicated structure of multilevel cgroups there
#' some preparation for the cgroup options is required.
#'
#' @inheritParams htmlTable
#' @return \code{list(cgroup, n.cgroup, align.cgroup, cgroup_spacer_cells)}
#' @keywords internal
#' @family hidden helper functions for \code{\link{htmlTable}}
prHtPrepareCgroup <- function(x, cgroup, n.cgroup, align.cgroup){
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

  if (missing(align.cgroup)){
    align.cgroup <- apply(n.cgroup, 1,
                         function(nc) paste(rep("c", times=sum(!is.na(nc))), collapse=""))
    align.cgroup <- matrix(align.cgroup,
                          ncol = 1)
  }else{
    if (NROW(align.cgroup) != nrow(n.cgroup))
      stop("You have different dimensions for your align.cgroup and your cgroups, ",
           NROW(align.cgroup), " (just) !=", nrow(n.cgroup), " (n.cgroup)")

    # An old leftover behaviour from the latex() function
    if (NCOL(align.cgroup) > 1)
      align.cgroup <- apply(align.cgroup, 1, function(x) paste(ifelse(is.na(x), "", x), collapse=""))

    align.cgroup <- mapply(prHtPrepareAlign,
                          align = align.cgroup,
                          x = apply(n.cgroup, 1, function(nc) sum(!is.na(nc))),
                          rnames=FALSE)

    align.cgroup <- matrix(align.cgroup, ncol=1)
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
              align.cgroup = align.cgroup,
              cgroup_spacer_cells = cgroup_spacer_cells))
}

#' Gets the rowlabel position
#'
#' @inheritParams htmlTable
#' @return \code{integer} Returns the position within the header rows
#'  to print the \code{rowlabel} argument
#' @keywords internal
#' @family hidden helper functions for \code{\link{htmlTable}}
prHtGetRowlabelPos <- function (cgroup, pos.rowlabel, header) {
  no_cgroup_rows <-
    ifelse(!missing(cgroup),
           nrow(cgroup),
           0)
  no_header_rows <-
    no_cgroup_rows +
    (!missing(header))*1
  if (is.numeric(pos.rowlabel)){
    if(pos.rowlabel < 1)
      stop("You have specified a pos.rowlabel that is less than 1: ", pos.rowlabel)
    else if (pos.rowlabel > no_header_rows)
      stop("You have specified a pos.rowlabel that more than the max limit, ",
           no_header_rows,
           ", you have provided: ", pos.rowlabel)
  }else{
    pos.rowlabel <- tolower(pos.rowlabel)
    if (pos.rowlabel %in% c("top"))
      pos.rowlabel <- 1
    else if (pos.rowlabel %in% c("bottom", "header"))
      pos.rowlabel <- no_header_rows
    else
      stop("You have provided an invalid pos.rowlabel text,",
           " only 'top', 'bottom' or 'header' are allowed,",
           " can't interpret '", pos.rowlabel, "'")
  }

  return(pos.rowlabel)
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
#' @param has_rn_col Due to the alignment issue we need to keep track
#'  of if there has already been printed a rowname column or not and therefore
#'  we have this has_rn_col that is either 0 or 1.
#' @param offset For rgroup rows there may be an offset != 1
#' @return \code{string} Returns the string with the new cell elements
#' @keywords internal
#' @family hidden helper functions for \code{\link{htmlTable}}
prHtAddCells <- function(table_str, rowcells, cellcode, align, style, cgroup_spacer_cells, has_rn_col, col.columns,
                         offset = 1){
  style = prHtAddSemicolon2StrEnd(style)

  for (nr in offset:length(rowcells)){
    cell_value <- rowcells[nr]
    # We don't want missing to be NA in a table, it should be empty
    if (is.na(cell_value))
      cell_value <- ""

    if (!missing(col.columns)){
      cell_style <- prHtGetStyle(style,
                                 align=prHtGetAlign(align, nr + has_rn_col),
                                 paste("background-color:", col.columns[nr]))
    }else{
      cell_style <- prHtGetStyle(style,
                                 align=prHtGetAlign(align, nr + has_rn_col))
    }
    table_str <- sprintf("%s\n\t\t<%s style='%s'>%s</%s>",
                         table_str,
                         cellcode,
                         cell_style,
                         cell_value,
                         cellcode)

    # Add empty cell if not last column
    if (nr != length(rowcells) && cgroup_spacer_cells[nr] > 0){
      spanner_style <- style
      if (!missing(col.columns)){
        if (nr != 1 && 
              col.columns[nr] == col.columns[nr + 1]){
          spanner_style <- prHtGetStyle(spanner_style,
                                        paste("background-color:", col.columns[nr]))
        }
      }
      table_str <- sprintf("%s\n\t\t<%s style='%s' colspan='%d'>&nbsp;</%s>",
                           table_str, cellcode, 
                           spanner_style, 
                           cgroup_spacer_cells[nr], 
                           cellcode)
    }
  }
  return (table_str)
}

#' Gets alignment
#'
#' @param index The index of the align parameter of interest
#' @family hidden helper functions for
#' @keywords internal
#' @inheritParams htmlTable
prHtGetAlign <- function(align, index) {
  segm_rgx <- "[^lrc]*[rlc][^lrc]*"

  res_align <- align
  align <- ""
  # Loop to remove every element prior to the one of interest
  for (i in 1:index){
    if (nchar(res_align) == 0)
      stop("Requested column outside of span, ", index, " > ", i)

    rmatch <- regexpr(segm_rgx, res_align)
    lrc_data <- substr(res_align, 1, rmatch + attr(rmatch, "match.length") - 1)
    res_align <- substring(res_align, rmatch + attr(rmatch, "match.length"))
  }
  styles <- c()
  if (grepl("^[|]", lrc_data))
    styles["border-left"] = "1px solid black"
  if (grepl("[|]$", lrc_data))
    styles["border-right"] = "1px solid black"

  if (grepl("l", lrc_data))
    styles["text-align"] = "left"
  if (grepl("c", lrc_data))
    styles["text-align"] = "center"
  if (grepl("r", lrc_data))
    styles["text-align"] = "right"

  return(styles)
}

#' Prepares the align to match the columns
#'
#' The alignment may be tricky and this function therefore simplifies
#' this process by extending/shortening the alignment to match the
#' correct number of columns.
#'
#' @param default_rn The default rowname alignment. This is an option
#'  as the header uses the same function and there may be differences in
#'  how the alignments should be implemented.
#' @keywords internal
#' @family hidden helper functions for \code{\link{htmlTable}}
#' @inheritParams htmlTable
prHtPrepareAlign <- function (align, x, rnames, default_rn = "l") {
  if (length(align) > 1)
    align <- paste(align, collapse="")

  segm_rgx <- "[^lrc]*[rlc][^lrc]*"
  no_elements <- length(strsplit(align, split = segm_rgx)[[1]])
  no_cols <- ifelse(is.null(dim(x)), x, ncol(x))
  if (!prHtSkipRownames(rnames)){
    no_cols <- no_cols + 1
    if (no_elements < no_cols){
      align <- paste0(default_rn, align)
    }
  }

  res_align <- align
  align <- ""
  for (i in 1:no_cols){
    rmatch <- regexpr(segm_rgx, res_align)
    tmp_lrc <- substr(res_align, 1, rmatch + attr(rmatch, "match.length") - 1)
    res_align <- substring(res_align, rmatch + attr(rmatch, "match.length"))
    align <- paste0(align,
                    tmp_lrc)
    if (nchar(res_align) < 1 &&
          i != no_cols){
      align <- paste0(align,
                      paste(rep(tmp_lrc, times=no_cols - i), collapse=""))
      break;
    }
  }

  structure(align,
            n = no_cols,
            class = class(align))
}

#' Returns if rownames should be printed for the htmlTable
#'
#' @inheritParams htmlTable
#' @keywords internal
prHtSkipRownames <- function(rnames){
  if(missing(rnames))
    return(TRUE)

  if (length(rnames) == 1 &&
        rnames == FALSE)
    return(TRUE)

  return(FALSE)
}

#' Prepares the alternating colors
#'
#' @param clr The colors
#' @param n The number of rows/columns applicable to the color
#' @param ng The n.rgroup argument if applicable
#' @return \code{character} A vector containing hexadecimal colors
#' @keywords internal
prHtPrepareColors <- function(clr, n, ng){
  clr <- sapply(clr, function(a_clr){
    if(a_clr == "none")
      return(a_clr)
    paste0('#',
           paste(as.character(as.hexmode(col2rgb(a_clr))),
                 collapse=""))
  }, USE.NAMES=FALSE)

  if(!missing(ng)){
    clr <- rep(clr, length.out = length(ng))

    attr(clr, "groups") <-
      Map(rep, clr, length.out = ng)
  }else if(!missing(n)){
    clr <- rep(clr, length.out = n)
  }

  return(clr)
}