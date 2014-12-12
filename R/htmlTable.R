#' Outputting HTML tables
#'
#' This is a function for outputting a more advanced
#' table than \pkg{xtable} or \pkg{knitr}'s \code{kable} allows.
#' It's aim is to provide the \pkg{Hmisc} \code{\link[Hmisc]{latex}()}
#' colgroup and rowgroup functions in HTML. The html-output is designed for
#' maximum compatibility with LibreOffice/OpenOffice.
#'
#' @section Important \pkg{knitr}-note:
#'
#' This funciton will only work with \pkg{knitr} outputting html, i.e.
#' markdown mode. For each section where you want a table to be outputted
#' you need to specify: \code{results="asis"}. As the function returns
#' raw html-code the compatibility with non-html formatting is limited,
#' even with \href{http://johnmacfarlane.net/pandoc/}{pandoc}.
#'
#' @section Table counter:
#'
#' If you set the option table_counter you will get a Table 1,2,3
#' etc before each table, just set \code{options(table_counter=TRUE)}. If
#' you set it to a number then that number will correspond to the start of
#' the table_counter. The \code{table_counter} option will also contain the number
#' of the last table, this can be useful when referencing it in text. By
#' setting the option \code{options(table_counter_str = "<b>Table \%s:</b> ")}
#' you can manipulate the counter table text that is added prior to the
#' actual caption. Note, you should use the \code{\link{sprintf}} \code{\%s}
#' instead of \code{\%d} as the software converts all numbers to characters
#' for compatibility reasons. If you set \code{options(table_counter_roman = TRUE)}
#' then the table counter will use Roman numumerals instead of Arabic.
#'
#' @section Possible issues:
#' Note that when using complex cgroup alignments with multiple levels
#' not every browser is able to handle this. For instance the RStudio
#' webkit browser seems to have issues with this and a
#' \href{http://code.google.com/p/chromium/issues/detail?id=305130}{bug has been filed}.
#'
#' As the table uses html for rendering you need to be aware of that headers,
#' rownames, and cell values should try respect this for optimal display. Browsers
#' try to compensate and frequently the tables still turn out OK but it is
#' not advized. Most importantly you should try to use
#' \code{&lt;} instead of \code{<} and
#' \code{&gt;} instead of \code{>}. You can find a complete list
#' of html characters \href{http://ascii.cl/htmlcodes.htm}{here}.
#'
#' @param x The matrix/data.frame with the data
#' @param headings a vector of character strings specifying column
#' headings, defaulting to \code{x}'s 	\code{colnames}
#' @param align a character strings specifying column alignments, defaulting to
#'  \code{paste(c("l", rep('c',ncol(n_table)-1)),collapse='')} to center. Valid alignments are
#'  l = left, c = center and r = right. You can also specify \code{align='c|c'} and
#'  other LaTeX tabular formatting.
#' @param halign a character strings specifying alignment for column headings,
#'  defaulting to centered.
#' @param cgroup a vector or a matrix of character strings defining major column headings. The default
#'  is to have none. This is also known as "the column spanner". If you want a column not
#'  to have a spanner then put that column as "". If you pass cgroup and n.crgroup as
#'  matrices you can have multiline cgroups. If the different levels have different number
#'  of elements you need to set the ones that lack elements to NA. For instance
#'  \code{cgroup = rbind(c("first", "second", NA), c("a", "b", "c"))}.
#' @param n.cgroup  a vector or matrix containing the number of columns for which each element in
#'  cgroup is a heading. For example, specify \code{cgroup=c("Major 1","Major 2")},
#'  \code{n.cgroup=c(3,3)} if \code{"Major 1"} is to span columns 1-3 and
#'  \code{"Major 2"} is to span columns 4-6.
#'  \code{rowlabel} does not count in the column numbers. You can omit \code{n.cgroup}
#'  if all groups have the same number of columns.
#' @param cgroup.just The justification of the c.groups
#' @param rgroup.padding Generally two non-breakings spaces, i.e. \code{&nbsp;&nbsp;}, but some
#'  journals only have a bold face for the rgroup and leaves the subelements unindented.
#' @param rgroupCSSstyle CSS style for the rgorup, if different styles are wanted for each of the
#'  rgroups you can just specify a vector with the number of elements
#' @param rgroupCSSseparator The line between different rgroups. The line is set to the TR element
#'  of the lower rgroup, i.e. you have to set the border-top/padding-top etc to a line with
#'  the expected function. This is only used for rgroups that are printed. You can specify
#'  different separators if you give a vector of rgroup - 1 length (this is since the first
#'  rgroup doesn't have a separator).
#' @param tspanner The table spanner is somewhat of a table header that
#'  you can use when you want to join different tables with the same columns.
#' @param n.tspanner The number of rows in the original matrix that
#'  the table spanner should span
#' @param tspannerCSSstyle The CSS style for the table spanner
#' @param tspannerCSSseparator The line between different spanners
#' @param rowlabel If x has row dimnames, rowlabel is a character string containing the
#'  column heading for the row dimnames.
#' @param rowlabel.pos Where the rowlabel should be positioned. This value can be "top",
#'  "bottom", "header", or a integer between \code{1} and \code{nrow(cgroup) + 1}. The options
#'  "bottom", "header" are the same, where the row label is presented at the same level as
#'  the header.
#' @param rnames Default rownames are generated from \code{rownames(x)}. If you
#'  provide \code{FALSE} then it will skip the rownames. Note that
#'  even if you do \code{rownames(my_dataframe) <- NULL} it still has
#'  rownames, thus you need to use \code{FALSE} if you want to
#'  surpress those.
#' @param caption a text string to use as a caption to print at the top of the first
#'  page of the table. Default is no caption.
#' @param caption.loc set to \code{"bottom"} to position a caption below the table
#'  instead of the default of \code{"top"}.
#' @param tfoot Add a table footer if needed at the bottom of the table using
#'  the \code{<tfoot>} html element.
#' @param label a text string representing a symbolic label for the
#'  table for referencing as an anchor. All you need to do is to reference the
#'  table, for instance \code{<a href="#anchor_name">see table 2</a>}
#' @param ctable If the table should have a double top border or a single a' la LaTeX ctable style
#' @param compatibility Is default set to \code{LibreOffice} as some
#'  settings need to be in old html format as Libre Office can't
#'  handle some commands such as the css caption-alignment. Note: this
#'  option is not yet fully implemented for all details, in the future
#'  I aim to generate a html-correct table and one that is aimed
#'  at Libre Office compatibility. Word-compatibility is difficult as
#'  Word ignores most settings and destroys all layout attempts
#'  (at least that is how my 2010 version behaves).
#' @param altcol alternating colors for each \code{rgroup}; one or two colors
#'  is recommended and will be recycled (will throw warning if the number of
#'  \code{rgroup}s is not a multiple of \code{length(altcol)}). Note that
#'  the altcol currently only works when copy-pasting from the browser and
#'  not when opening directly in LibreOffice.
#' @param tableCSSclass The html CSS class for the table. This allows directing html
#'  formatting through \href{http://www.w3schools.com/css/css_selectors.asp}{CSS}
#'  directly at all instances of that class. \emph{Note:} unfortunately the
#'  CSS is frequently ignored by word processors. This option
#'  is mostly inteded for web-presentations.
#' @param ... Passed on to \code{print.htmlTable} function and any argument except the
#'  \code{useViewer} will be passed on to the \code{\link[base]{cat}} functions arguments.
#' @return \code{string} Returns a string of class htmlTable
#'
#' @example inst/examples/htmlTable_example.R
#'
#' @seealso \code{\link{getDescriptionStatsBy}},
#'          \code{\link{mergeDesc}},
#'          \code{\link{splitLines4Table}},
#'          \code{\link[Hmisc]{latex}}
#'
#'
#' @export
#' @rdname htmlTable
htmlTable <- function(x, ...){
  UseMethod("htmlTable")
}

#' @rdname htmlTable
#' @export
htmlTable.descMrg <- function(x, ...)
{
  dots <- list(...)
  if (!"rgroup" %in% names(dots)){
    return(NextMethod(generic = NULL, object = x,
                      rgroup = attr(x, "rgroup"),
                      n.rgroup = attr(x, "n.rgroup"),
                      ...))
  }

  return(NextMethod(generic = NULL, object = x,
                    ...))
}

#' @param rgroup A vector of character strings containing headings for row groups.
#'  \code{n.rgroup} must be present when \code{rgroup} is given. The first
#'  \code{n.rgroup[1]}rows are sectioned off and \code{rgroup[1]} is used as a bold
#'  heading for them. The usual row dimnames (which must be present if \code{rgroup} is)
#'  are indented. The next \code{n.rgroup[2]} rows are treated likewise, etc. If you don't
#'  want a row to be part of a row group then you just put "" for that row, remember to add
#'  the corresponding number of rows in n.rgroup.
#' @param n.rgroup integer vector giving the number of rows in each grouping. If \code{rgroup}
#'  is not specified, \code{n.rgroup} is just used to divide off blocks of rows by horizontal
#'  lines. If \code{rgroup} is given but \code{n.rgroup} is omitted, \code{n.rgroup} will
#'  default so that each row group contains the same number of rows.
#' @importFrom stringr str_trim
#' @importFrom stringr str_replace
#' @importFrom Hmisc first.word
#' @importFrom Hmisc format.df
#' @rdname htmlTable
#' @export
htmlTable.default <- function(x,
                      headings,
                      align =paste(c("l", rep('c',ncol(x)-1)),collapse=''),
                      halign=paste(rep('c',ncol(x)),collapse=''),
                      cgroup, n.cgroup,
                      cgroup.just,
                      rgroup, n.rgroup,
                      rgroupCSSstyle = "font-weight: 900;",
                      rgroupCSSseparator = "",
                      rgroup.padding = "&nbsp;&nbsp;",
                      tspanner, n.tspanner,
                      tspannerCSSstyle = "font-weight: 900; text-transform: capitalize; text-align: left;",
                      tspannerCSSseparator = "border-top: 1px solid grey;",
                      rowlabel,
                      rowlabel.pos = "bottom",
                      ctable = TRUE,
                      compatibility = "LibreOffice",
                      rnames,
                      caption,
                      caption.loc='top',
                      tfoot,
                      label,
                      altcol = 'white',
                      tableCSSclass = "gmisc_table",
                      ...)
{
  dots <- list(...)
  # Warnings due to interface changes in 1.0
  if ("rowname" %in% names(dots) && missing(rnames)){
    rnames <- dots$rowname
    dots$rowname <- NULL
    warning("Deprecated: rowname argument is now rnames as of ver. 1.0")
  }

  ## this will convert color names to hexadecimal (easier for user)
  ## but also leaves hex format unchanged
  altcol <- paste0('#', apply(apply(rbind(col2rgb(altcol)),
                                    2,
                                    function(x) as.character(as.hexmode(x))),
                              2,
                              paste, collapse = '')
                   )

  # Unfortunately in knitr there seems to be some issue when the
  # rnames is specified immediately as: rnames=rownames(x)
  if (missing(rnames)){
    if (any(is.null(rownames(x)) == FALSE))
      rnames <- rownames(x)
  }else if (any(is.null(rownames(x))) && !missing(rgroup)){
    warning("You have not specified rnames but you seem to have rgroups.",
            "If you have the first column as rowname but you want the rgroups",
            "to result in subhedings with indentation below then, ",
            "you should change the rnames to the first column and then",
            "remove it from the table matrix (the x argument object).")
  }

  if (!missing(rowlabel) &&
        prHtSkipRownames(rnames))
    stop("You can't have a row label and no rownames.",
         " Either remove the rowlabel argument",
         ", set the rnames argument",
         ", or set the rownames of the x argument.")

  if (missing(headings) &&
        !is.null(colnames(x)))
    headings<-colnames(x)

  if (length(dim(x)) != 2)
    stop("Your table variable seems to have the wrong dimension,",
         " length(dim(x)) = ", length(dim(x)) , " != 2")

  # Fix alignment to match with the matrix
  align <- prHtPrepareAlign(align, x, rnames)
  halign <- prHtPrepareAlign(halign, x, rnames, default_rn = "c")

  if (tolower(compatibility) %in% c("libreoffice", "libre office",
                                    "open office", "openoffice",
                                    "word", "ms word", "msword")){
    compatibility <- "LibreOffice"
  }

  if (!missing(rgroup)){
    if (missing(n.rgroup))
      stop("You need to specify the argument n.rgroup if you want to use rgroups")

    if (any(n.rgroup < 1)){
      warning("You have provided rgroups with less than 1 elements,",
              " these will therefore be removed: ",
              paste(sprintf("'%s' = %d", rgroup, n.rgroup)[n.rgroup < 1],
                    collapse=", "))
      rgroup <- rgroup[n.rgroup >= 1]
      n.rgroup <- n.rgroup[n.rgroup >= 1]
    }
    # Sanity check for rgroup
    if (sum(n.rgroup) !=  nrow(x))
      stop("Your rows don't match in the n.rgroup,",
           " i.e. ", sum(n.rgroup) , "(n.rgroup) != ", nrow(x), "(rows in x)")

    # Sanity checks rgroupCSSstyle and prepares the style
    if (length(rgroupCSSstyle) > 1 &&
          length(rgroupCSSstyle) != length(rgroup))
      stop(sprintf("You must provide the same number of styles as the rgroups, %d != %d",
                   length(rgroupCSSstyle), length(rgroup)))
    else if(length(rgroupCSSstyle) == 1){
      rgroupCSSstyle <- prHtGetStyle(rgroupCSSstyle)

      if (length(rgroup) > 0)
        rgroupCSSstyle <- rep(rgroupCSSstyle, length.out=length(rgroup))
    } else {
      for (i in 1:length(rgroupCSSstyle))
        rgroupCSSstyle[i] <- prHtGetStyle(rgroupCSSstyle[i])
    }

    # Sanity checks rgroupCSSseparator and prepares the style
    if (length(rgroupCSSseparator) > 1 &&
          length(rgroupCSSseparator) != length(rgroup)-1)
      stop(sprintf("You must provide the same number of separators as the rgroups - 1, %d != %d",
                   length(rgroupCSSseparator), length(rgroup)-1))
    else if(length(rgroupCSSseparator) == 1){
      rgroupCSSseparator <- prHtAddSemicolon2StrEnd(rgroupCSSseparator)

      if (length(rgroup) > 0)
        rgroupCSSseparator <- rep(rgroupCSSseparator, length.out=length(rgroup))
    } else {
      for (i in 1:length(rgroupCSSseparator))
        rgroupCSSseparator[i] <- prHtAddSemicolon2StrEnd(rgroupCSSseparator[i])
    }
  }

  if (!missing(tspanner)){

    # Sanity checks tspannerCSSstyle and prepares the style
    if (length(tspannerCSSstyle) > 1 &&
          length(tspannerCSSstyle) != length(tspanner))
      stop(sprintf("You must provide the same number of styles as the tspanners, %d != %d",
                   length(tspannerCSSstyle), length(tspanner)))
    else if(length(tspannerCSSstyle) == 1){
      tspannerCSSstyle <- prHtAddSemicolon2StrEnd(tspannerCSSstyle)

      if (length(tspanner) > 0)
        tspannerCSSstyle <- rep(tspannerCSSstyle, length.out=length(tspanner))
    } else {
      for (i in 1:length(tspannerCSSstyle))
        tspannerCSSstyle[i] <- prHtAddSemicolon2StrEnd(tspannerCSSstyle[i])
    }


    # Sanity checks tspannerCSSseparator and prepares the style
    if (length(tspannerCSSseparator) > 1 &&
          length(tspannerCSSseparator) != length(tspanner)-1)
      stop(sprintf("You must provide the same number of separators as the tspanners - 1, %d != %d",
                   length(tspannerCSSseparator), length(tspanner)-1))
    else if(length(tspannerCSSseparator) == 1){
      tspannerCSSseparator <- prHtGetStyle(tspannerCSSseparator)

      if (length(tspanner) > 0)
        tspannerCSSseparator <- rep(tspannerCSSseparator, length.out=length(tspanner)-1)
    } else {
      for (i in 1:length(tspannerCSSseparator))
        tspannerCSSseparator[i] <- prHtGetStyle(tspannerCSSseparator[i])
    }
  }


  # Sanity check for tspanner
  if (!missing(tspanner)){
    if (missing(n.tspanner))
      stop("You need to specify the argument n.tspanner if you want to use table spanners")

    if(sum(n.tspanner) !=  nrow(x))
      stop(sprintf("Your rows don't match in the n.tspanner, i.e. %d != %d",
                   sum(n.tspanner), nrow(x)))

    # Make sure there are no collisions with rgrou
    if (!missing(n.rgroup)){
      for (i in 1:length(n.tspanner)){
        rows <- sum(n.tspanner[1:i])
        if (!rows %in% cumsum(n.rgroup))
          stop("There is no splitter that matches the table spanner ",
               tspanner[i],
               " (no. ", i, ") with rgroup splits.",
               " The missing row splitter should be on row number ", rows,
               " and is not in the n.rgroup list: ", paste(n.rgroup, collapse=", "),
               " note, it should match the cumulative sum n.rgroup", paste(cumsum(n.rgroup), collapse=", "))
      }
    }
  }

  # With multiple rows in cgroup we need to keep track of
  # how many spacer cells occur between the groups
  cgroup_spacer_cells <- rep(0, times=(ncol(x)-1))

  # Sanity check for cgroup
  if (!missing(cgroup)){
    ret <- prHtPrepareCgroup(x = x,
                             cgroup = cgroup,
                             n.cgroup = n.cgroup,
                             cgroup.just = cgroup.just)

    cgroup <- ret$cgroup
    n.cgroup <- ret$n.cgroup
    cgroup.just <- ret$cgroup.just
    cgroup_spacer_cells <- ret$cgroup_spacer_cells
  }

  rowlabel.pos <- prHtGetRowlabelPos(cgroup, rowlabel.pos, headings)

  # Not quite as intended but close enough
  if(length(dots) > 0){
    f.df_args <- dots
    f.df_args[["x"]] <- x
    f.df_args[["numeric.dollar"]] <- FALSE
    x <- fastDoCall(format.df, f.df_args)
    rm(f.df_args)
  }
  # Remove some specifics for LaTeX
  if (is.character(x)) x <- matrix(str_replace(x, "\\\\%", "%"), ncol=ncol(x))

  # The id works just as well as any anchor
  table_id <- ""
  if (!missing(label)){
    table_id <- sprintf(" id='%s'", label)
  }else if(is.numeric(getOption("table_counter", FALSE))){
    table_id <- sprintf(" id='table_%d'", getOption("table_counter"))
  }

  # A column counter that is used for <td colspan="">
  total_columns <- ncol(x)+!prHtSkipRownames(rnames)
  if(!missing(cgroup)){
    if (!is.matrix(cgroup)){
      total_columns <- total_columns + length(cgroup) - 1
    }else{
      total_columns <- total_columns + sum(cgroup_spacer_cells)
    }
  }

  ###############################
  # Start building table string #
  ###############################
  table_str <- sprintf("<table class='%s' style='border-collapse: collapse;' %s>",
                       paste(tableCSSclass, collapse=", "),
                       table_id)

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


  if (!missing(caption)){
    # Combine a table counter if provided
    caption <- sprintf("\n\t%s%s", prHtGetTableCntr(), caption)
  }

  # Add caption according to standard HTML
  if (!missing(caption) &
        compatibility != "LibreOffice"){
    if (caption.loc == "bottom"){
      table_str <- sprintf("%s\n\t<caption style='caption-side: bottom'>", table_str)
    }else{
      table_str <- sprintf("%s\n\t<caption style='caption-side: top'>", table_str)
    }

    table_str <- sprintf("%s%s</caption>", table_str, caption)
  }

  # Start the head
  table_str <- sprintf("%s\n\t<thead>", table_str)

  if (!missing(caption) &
        compatibility == "LibreOffice" &
        caption.loc != "bottom"){

    table_str <- sprintf("%s\n\t<tr><td colspan='%d' style='text-align: left;'>%s</td></tr>",
                         table_str,
                         total_columns,
                         caption)
  }

  # Add the cgroup table header
  if (!missing(cgroup)){

    for (i in 1:nrow(cgroup)){
      table_str <- paste0(table_str,
                          prHtGetCgroupHeader(x = x,
                                              cgroup_vec = cgroup[i,],
                                              n.cgroup_vec = n.cgroup[i,],
                                              cgroup_vec.just = cgroup.just[i, ],
                                              row_no = i,
                                              top_row_style = top_row_style,
                                              rnames = rnames,
                                              rowlabel = rowlabel,
                                              rowlabel.pos = rowlabel.pos,
                                              cgroup_spacer_cells = cgroup_spacer_cells))
    }
    first_row <- FALSE
  }


  # Add the headings
  if (!missing(headings)){
    # The bottom border was ment to be here but it doesn't
    # work that well in the export
    table_str <- sprintf("%s\n\t<tr>", table_str)
    no_cgroup_rows <-
      ifelse(!missing(cgroup),
             nrow(cgroup),
             0)
    ts <- ifelse(no_cgroup_rows > 0, "", top_row_style)
    if (!missing(rowlabel) && rowlabel.pos == no_cgroup_rows + 1){
      table_str <- sprintf("%s\n\t\t<th style='%s'>%s</th>",
                           table_str,
                           prHtGetStyle(c(`font-weight`=900,
                                          `border-bottom`="1px solid grey"),
                                        ts,
                                        align=prHtGetAlign(halign, 1)),
                           rowlabel)
    }else if(!prHtSkipRownames(rnames)){
      table_str <- sprintf("%s\n\t\t<th style='%s'> </th>",
        table_str,
        prHtGetStyle(c(`border-bottom`="1px solid grey"), ts))
    }

    cell_style= "border-bottom: 1px solid grey;"
    if (first_row){
      cell_style=paste(cell_style, top_row_style, sep = "; ")
    }
    table_str <- prHtAddCells(table_str = table_str,
                              rowcells = headings,
                              cellcode = "th",
                              align = halign,
                              style=cell_style,
                              cgroup_spacer_cells = cgroup_spacer_cells,
                              has_rn_col = !prHtSkipRownames(rnames)*1)

    table_str <- sprintf("%s\n\t</tr>", table_str)
    first_row <- FALSE
  }

  #################################
  # Close head and start the body #
  #################################
  table_str <- sprintf("%s\n\t</thead><tbody>", table_str)
  ## background colors for rows, by rgroup
  if (!missing(rgroup)){ rs2 <- unlist(Map(rep, altcol, n.rgroup)) }

  rgroup_iterator <- 0
  tspanner_iterator <- 0
  for (row_nr in 1:nrow(x)){
    # First check if there is a table spanner that should be applied
    if (!missing(tspanner) &&
          (row_nr == 1 ||
             row_nr > sum(n.tspanner[1:tspanner_iterator]))){
      tspanner_iterator = tspanner_iterator + 1

      rs <- tspannerCSSstyle[tspanner_iterator]

      # Use a separator from the one above if this
      # at least the second spanner. Graphically this
      # appears as if underneath the group while it's
      # actually above but this merges into one line
      if (tspanner_iterator > 1){
        rs <- prHtGetStyle(rs,
                           tspannerCSSseparator[tspanner_iterator-1])
      }

      if (first_row){
        rs <- prHtGetStyle(rs,
                           top_row_style)
      }

      table_str <- sprintf("%s\n\t<tr><td colspan='%d' style='%s'>%s</td></tr>", table_str,
                           total_columns,
                           rs,
                           tspanner[tspanner_iterator])
      first_row <- FALSE
    }


    # Add the row group if any
    # and it's:
    # - first row
    # - the row belongs to the next row group
    if (!missing(rgroup) &&
      (row_nr == 1 ||
        row_nr > sum(n.rgroup[1:rgroup_iterator]))){
      rgroup_iterator = rgroup_iterator + 1

      rs <- rgroupCSSstyle[rgroup_iterator]

      # Use a separator from the one above if this
      # at least the second group. Graphically this
      # appears as if underneath the group while it's
      # actually above but this merges into one line
      if (rgroup_iterator > 1){
        rs <- sprintf("%s %s", rs,
          rgroupCSSseparator[rgroup_iterator-1])
      }

      # Only add if there is anything in the group
      if (is.na(rgroup[rgroup_iterator]) == FALSE &&
            rgroup[rgroup_iterator] != ""){

        if (first_row){
          rs <- paste(rs, top_row_style, sep = "; ")
        }

        ## this will allow either rgroupCSSstyle or altcol to
        ## color the rgroup label rows
        table_str <- sprintf("%s\n\t<tr style='background-color:%s;'><td colspan='%d' style='%s'>%s</td></tr>", table_str,
                             rep(unique(rs2), length(rgroup))[rgroup_iterator],
                             total_columns,
                             rs,
                             rgroup[rgroup_iterator])

        first_row <- FALSE
      }
    }


    if (!missing(rgroup)){
      ## this will change the bgcolor of the rows, by rgroup
      rs <- c(`background-color` = rs2[row_nr])
    }else{
      rs <- ""
    }

    cell_style = "";
    if (first_row){
      rs <- prHtGetStyle(rs, top_row_style)
      cell_style <- prHtGetStyle(top_row_style)
    }
    first_row <- FALSE

    if (row_nr == nrow(x))
      cell_style = prHtGetStyle(cell_style,
                                bottom_row_style)

    if (rs == ""){
      table_str <- paste0(table_str, "\n\t<tr>")
    }else{
      table_str <- sprintf("%s\n\t<tr style='%s'>", table_str, rs)
    }

    if (!prHtSkipRownames(rnames)){
      pdng <- ""
      # Minor change from original function. If the group doesn't have
      # a group name then there shouldn't be any indentation
      if (!missing(rgroup) &&
            rgroup_iterator > 0 &&
            is.na(rgroup[rgroup_iterator]) == FALSE &&
            rgroup[rgroup_iterator] != ""){
        pdng <- rgroup.padding
      }

      # The padding doesn't work well with the Word import - well nothing really works well with word...
      # table_str <- sprintf("%s\n\t\t<td style='padding-left: .5em;'>%s</td>", table_str, rnames[row_nr])
      table_str <- sprintf("%s\n\t\t<td style='%s'>%s%s</td>",
                           table_str,
                           prHtGetStyle(cell_style,
                                        align=prHtGetAlign(align, 1)),
                           pdng,
                           rnames[row_nr])
    }

    table_str <- prHtAddCells(table_str = table_str,
                              rowcells = x[row_nr,],
                              cellcode = "td",
                              align = align,
                              style = cell_style,
                              cgroup_spacer_cells = cgroup_spacer_cells,
                              has_rn_col = !prHtSkipRownames(rnames)*1)

    table_str <- sprintf("%s\n\t</tr>", table_str)
  }

  # Close body
  table_str <- sprintf("%s\n\t</tbody>", table_str)

  if (!missing(caption) &
        compatibility == "LibreOffice" &
        caption.loc == "bottom"){

    table_str <- sprintf("%s\n\t<tr><td colspan='%d' style='text-align: left;'>%s</td></tr>",
                         table_str,
                         total_columns,
                         caption)
  }

  # Add footer
  if (!missing(tfoot)){
    # Initiate the tfoot
    table_str <- sprintf("%s\n\t<tfoot><tr><td colspan='%d'>", table_str, total_columns)

    # Add the actual tfoot to a new row
    table_str<- sprintf("%s\n\t%s", table_str, tfoot)

    # Close the tfoot
    table_str <- sprintf("%s</td></tr></tfoot>", table_str)
  }

  # Close table
  table_str <- sprintf("%s\n</table>", table_str)

  class(table_str) <- c("htmlTable", class(table_str))
  attr(table_str, "...") <- list(...)

  return(table_str)
}

#' @importFrom methods setClass
setClass("htmlTable", contains = "character")

#' @rdname htmlTable
#' @param useViewer If you are using RStudio there is a viewer thar can render
#'  the table within that is envoced if in \code{\link[base]{interactive}} mode.
#'  Set this to \code{FALSE} if you want to remove that  functionality. You can
#'  also force the function to call a specific viewer by setting this to a
#'  viewer function, e.g. \code{useViewer = utils::browseUrl} if you want to
#'  override the default RStudio viewer. Another option that does the same is to
#'  set the \code{options(viewer=utils::browseUrl)} and it will default to that
#'  particular viewer (this is how RStudio decides on a viewer).
#' @export
#' @importFrom utils browseURL
print.htmlTable<- function(x, useViewer, ...){
  args <- attr(x, "...")
  # Use the latest ... from the print call
  # and override the original htmlTable call ...
  # if there is a conflict
  print_args <- list(...)
  for (n in names(print_args)){
    args[[n]] <- print_args[[n]]
  }

  # Since the print may be called from another print function
  # it may be handy to allow functions to use attributes for the
  # useViewer parameter
  if (missing(useViewer)){
    if ("useViewer" %in% names(args) &&
      is.logical(args$useViewer)){
        useViewer <- args$useViewer
        args$useViewer <- NULL
    }else{
      useViewer <- TRUE
    }
  }

  if (interactive() &&
        (is.function(useViewer) ||
        useViewer != FALSE))
  {
    if (is.null(args$file)){
      args$file <- tempfile(fileext=".html")
    }

    htmlPage <- paste("<html>",
                      "<head>",
                      "<meta http-equiv=\"Content-type\" content=\"text/html; charset=UTF-8\">",
                      "</head>",
                      "<body>",
                      "<div style=\"margin: 0 auto; display: table; margin-top: 1em;\">",
                      x,
                      "</div>",
                      "</body>",
                      "</html>", sep="\n")
    # We only want to use those arguments that are actually in cat
    # anything else that may have inadvertadly slipped in should
    # be ignored or it will be added to the output
    cat_args <- args
    cat_args <- cat_args[names(cat_args) %in% names(formals(cat))[-1]]
    fastDoCall(cat, c(htmlPage, cat_args))

    if (is.function(useViewer)){
      useViewer(args$file)
    }else{
      viewer <- getOption("viewer")
      if (!is.null(viewer) &&
            is.function(viewer)){
        # (code to write some content to the file)
        viewer(args$file)
      }else{
        utils::browseURL(args$file)
      }
    }
  }else{
    cat_args <- args
    cat_args <- cat_args[names(cat_args) %in% names(formals(cat))[-1]]
    fastDoCall(cat, c(x, cat_args))
  }
}

#' A helper function for html/LaTeX line formatting
#'
#' This function helps you to do a multiline
#' table header in both html and in LaTeX. In
#' html this isn't that tricky, you just use
#' the <br /> command but in LaTeX I often find
#' myself writing vbox/hbox stuff and therefore
#' I've created this simple helper function
#'
#' @param ... The lines that you want to be joined
#' @param html If HTML compatible output should be used. If \code{FALSE}
#'  it outputs LaTeX formatting
#' @return string
#'
#' @examples
#' splitLines4Table("hello", "world")
#' splitLines4Table("hello", "world", html=TRUE)
#' splitLines4Table("hello", "world", list("A list", "is OK"))
#'
#'
#' @export
splitLines4Table <- function(..., html = TRUE){
  strings <- c()
  for (i in list(...)){
    if (is.list(i)){
      for(c in i)
        strings <- append(strings, i)
    }else{
      strings <- append(strings, i)
    }

  }
  if (length(strings) < 2)
    return(strings)

  ret <- ifelse(html, "", "\\vbox{")
  first <- TRUE
  for (line in strings){
    line <- as.character(line)
    if (first)
      ret <- paste0(ret, ifelse(html, line, sprintf("\\hbox{\\strut %s}", line)))
    else
      ret <- paste0(ret, ifelse(html, sprintf("<br />%s", line), sprintf("\\hbox{\\strut %s}", line)))
    first <- FALSE
  }
  ret <- ifelse(html, ret, paste0(ret, "}"))

  return(ret)
}

