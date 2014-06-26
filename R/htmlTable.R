#' Outputting HTML tables
#' 
#' This is a function for outputting a more advanced
#' table than xtable allows. It's aim is to provide the Hmisc
#' \code{\link[Hmisc]{latex}()} colgroup and rowgroup functions in HTML. The
#' code outputted is perhaps a little raw compared to fully
#' CSS formatted HTML. The reason for this is that I've chosen
#' maximum compatibility with LibreOffice/OpenOffice that lacks any more 
#' advanced understanding of HTML & CSS. 
#' 
#' Note that when you use knitr in markdown mode you need to specify: 
#' results="asis". It is also good to know that the function outputs
#' raw html, this limits the compatibility with pandoc and similar tools.
#' 
#' If you set the option table_counter you will get a Table 1,2,3
#' etc before each table, just set \code{options(table_counter=TRUE)}. If
#' you set it to a number then that number will correspond to the start of 
#' the table_counter. The table_counter option will also contain the number
#' of the last table, this can be useful when referencing it in text. By 
#' setting the option \code{options(table_counter_str = "<b>Table \%s:</b> ")}
#' you can manipulate the counter table text that is added prior to the 
#' actual caption. Note, you should use the \code{\link{sprintf}} \code{\%s}
#' instead of \code{\%d} as the software converts all numbers to characters
#' for compatibility reasons. If you set \code{options(table_counter_roman = TRUE)}
#' then the table counter will use Roman numumerals instead of Arabic.
#' 
#' Note that when using complex cgroup alignments with multiple levels
#' not every browser is able to handle this. For instance the RStudio
#' webkit browser seems to have issues with this and a bug has been filed:
#' "http://code.google.com/p/chromium/issues/detail?id=305130"
#' 
#' If you in your knitr html-document get a "structure" noted under
#' the rowlabel heading then this is an effect from the automated
#' rowlabel name. It is copied from the title that in turn uses the
#' \code{x} to find an apropriate name: \code{title=first.word(deparse(substitute(x)))}.
#' All you need to do is simply set either the \code{title} or the \code{rowlabel}
#' arguments to get rid of this "bug".
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
#' @param title The title of the table. Used for labeling etc.
#' @param headings a vector of character strings specifying column 
#'    headings, defaulting to \code{x}'s 	\code{colnames}
#' @param align a character strings specifying column alignments, defaulting to	
#'    \code{paste(c("l", rep('c',ncol(n_table)-1)),collapse='')} to center. Valid alignments are
#'    l = left, c = center and r = right. You can also specify \code{align='c|c'} and 
#'    other LaTeX tabular formatting.
#' @param halign a character strings specifying alignment for column headings, 
#'    defaulting to centered.
#' @param cgroup a vector or a matrix of character strings defining major column headings. The default 
#'    is to have none. This is also known as "the column spanner". If you want a column not
#'    to have a spanner then put that column as "". If you pass cgroup and n.crgroup as
#'    matrices you can have multiline cgroups. If the different levels have different number
#'    of elements you need to set the ones that lack elements to NA. For instance 
#'    \code{cgroup = rbind(c("first", "second", NA), c("a", "b", "c"))}. 
#' @param n.cgroup  a vector or matrix containing the number of columns for which each element in
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
#'    are indented. The next \code{n.rgroup[2]} rows are treated likewise, etc. If you don't
#'    want a row to be part of a row group then you just put "" for that row, remember to add
#'    the corresponding number of rows in n.rgroup.
#' @param n.rgroup integer vector giving the number of rows in each grouping. If \code{rgroup}
#'    is not specified, \code{n.rgroup} is just used to divide off blocks of rows by horizontal 
#'    lines. If \code{rgroup} is given but \code{n.rgroup} is omitted, \code{n.rgroup} will 
#'    default so that each row group contains the same number of rows.
#' @param rgroupCSSstyle CSS style for the rgorup, if different styles are wanted for each of the
#'    rgroups you can just specify a vector with the number of elements
#' @param rgroupCSSseparator The line between different rgroups. The line is set to the TR element
#'    of the lower rgroup, i.e. you have to set the border-top/padding-top etc to a line with
#'    the expected function. This is only used for rgroups that are printed. You can specify
#'    different separators if you give a vector of rgroup - 1 length (this is since the first
#'    rgroup doesn't have a separator).
#' @param tspanner The table spanner is somewhat of a table header that
#'    you can use when you want to join different tables with the same columns.
#' @param n.tspanner The number of rows in the original matrix that 
#'    the table spanner should span
#' @param tspannerCSSstyle The CSS style for the table spanner
#' @param tspannerCSSseparator The line between different spanners
#' @param rowlabel If x has row dimnames, rowlabel is a character string containing the
#'    column heading for the row dimnames. The default is the name of the argument for x.
#' @param rowlabel.pos Where the rowlabel should be positioned. This value can be "top",
#'    "bottom", "header", or a integer between \code{1} and \code{nrow(cgroup) + 1}. The options
#'    "bottom", "header" are the same, where the row label is presented at the same level as
#'    the header.
#' @param rowname Default is rownames of matrix or data.frame. 
#' @param caption a text string to use as a caption to print at the top of the first 
#'    page of the table. Default is no caption.
#' @param caption.loc set to \code{"bottom"} to position a caption below the table 
#'    instead of the default of \code{"top"}.
#' @param tfoot Add a table footer if needed at the bottom of the table using 
#'    the \code{<tfoot>} html element.
#' @param label a text string representing a symbolic label for the 
#'    table for referencing as an anchor. All you need to do is to reference the
#'    table, for instance \code{<a href="#anchor_name">see table 2</a>}
#' @param ctable If the table should have a double top border or a single a' la LaTeX ctable style
#' @param compatibility Is default set to \code{LibreOffice} as some 
#'    settings need to be in old html format as Libre Office can't 
#'    handle some commands such as the css caption-alignment. Note: this
#'    option is not yet fully implemented for all details, in the future
#'    I aim to generate a html-correct table and one that is aimed
#'    at Libre Office compatibility. Word-compatibility is difficult as
#'    Word ignores most settings and destroys all layout attempts 
#'    (at least that is how my 2010 version behaves).
#' @param altcol alternating colors for each \code{rgroup}; one or two colors
#'    is recommended and will be recycled (will throw warning if the number of
#'    \code{rgroup}s is not a multiple of \code{length(altcol)})
#' @param tableCSSclass The html CSS class for the table. This allows directing html
#'    formatting through \href{http://www.w3schools.com/css/css_selectors.asp}{CSS}
#'    directly at all instances of that class. \emph{Note:} unfortunately the
#'    CSS is frequently ignored by word processors. This option
#'    is mostly inteded for web-presentations.
#' @param ... Currently not used, here for compatibility reasons
#' @return Returns a string with the output table if output is not set
#' 
#' @example inst/examples/htmlTable_example.R
#' 
#' @seealso \code{\link{latex}}, \code{\link{getDescriptionStatsBy}}, \code{\link{splitLines4Table}}
#' 
#' @importFrom stringr str_trim
#' @importFrom stringr str_replace
#' @importFrom Hmisc first.word
#' @importFrom Hmisc format.df
#' @author max
#' @export
#' @rdname htmlTable
#' @import methods
htmlTable <- function(x,
  title=first.word(deparse(substitute(x))),
  headings, 
  align =paste(c("l", rep('c',ncol(x)-1)),collapse=''),
  halign=paste(rep('c',ncol(x)),collapse=''),
  cgroup, n.cgroup,
  cgroup.just,
  rgroup, n.rgroup,
  rgroupCSSstyle = "font-weight: 900;",
  rgroupCSSseparator = "",
  tspanner, n.tspanner,
  tspannerCSSstyle = "font-weight: 900; text-transform:capitalize; text-align: center;",
  tspannerCSSseparator = "border-top: 1px solid grey;",
  rowlabel = title,
  rowlabel.pos = "bottom",
  ctable = FALSE,
  compatibility = "LibreOffice",
  rowname,
  caption,
  caption.loc='top',
  tfoot,
  label,
  altcol = 'white',
  tableCSSclass = "gmisc_table",
  ...)
{
  ## this will convert color names to hexadecimal (easier for user)
  ## but also leaves hex format unchanged 
  altcol <- paste0('#', apply(apply(rbind(col2rgb(altcol)), 
                                    2, 
                                    function(x) as.character(as.hexmode(x))),
                              2, 
                              paste, collapse = '')
                   )
  
  # Unfortunately in knitr there seems to be some issue when the
  # rowname is specified immediately as: rowname=rownames(x) 
  if (missing(rowname)){
    if (any(is.null(rownames(x)) == FALSE))
      rowname <- rownames(x)
  }else if (any(is.null(rownames(x))) && !missing(rgroup))
    warning("You have not specified rownames but you seem to have rgroups.",
        "If you have the first column as rowname but you want the rgroups",
        "to result in subhedings with indentation below then",
        "you should change the rownames to the first column and then",
        "remove it from the table matrix (the x argument object).")
  
  # This variable is just an indicator if rownames should be set
  if (!missing(rowname))
    set_rownames <- TRUE
  else
    set_rownames <- FALSE
  
  if (missing(headings) &&
        !is.null(colnames(x)))
    headings<-colnames(x)
  
  if (length(dim(x)) != 2)
    stop("Your table variable seems to have the wrong dimension, length(dim(x)) = ", 
      length(dim(x)) , " != 2")
  
  # Just in case the user forgot that this is a string and not a vector
  if (length(align) > 1)
    align <- paste(align, collapse="")
  
  if (tolower(compatibility) %in% c("libreoffice", "libre office", 
                                    "open office", "openoffice",
                                    "word", "ms word", "msword"))
    compatibility <- "LibreOffice"
  
  # Table counter #
  tc <- getOption("table_counter")
  if (is.null(tc)){
    tc_string <- ""
  }else{
    # Count which table it currently is
    if (is.numeric(tc))
      tc <- tc + 1
    else
      tc <- 1
    options(table_counter = tc)
    table_template <- getOption("table_counter_str", "Table %s: ")
    tc_string <- sprintf(table_template, ifelse(getOption("table_counter_roman", FALSE),
                                                as.character(as.roman(tc)),
                                                as.character(tc)))
  }
  
  # The CSS expects a semicolon at the end of each argument
  # this function just adds a semicolong if none is given
  addSemicolon2StrEnd <- function(my_str){
    my_str <- str_trim(my_str)
    if (substr(my_str, nchar(my_str), nchar(my_str) + 1) != ";")
      my_str <- sprintf("%s;", my_str)
    
    # Remove duplicated ;
    my_str <- gsub(";;+", ";", my_str)
    if (my_str == ";")
      return("")
    
    return (my_str)
  }
  
  addAlign2Style <- function(style, align){
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
  
  addCells <- function(table_str, rowcells, cellcode, align, style=""){
    style = addSemicolon2StrEnd(style)
    
    for (nr in 1:length(rowcells)){
      cell_value <- rowcells[nr]
      # We don't want missing to be NA in a table, it should be empty
      if (is.na(cell_value))
        cell_value <- ""
      
      table_str <- sprintf("%s\n\t\t<%s style='%s'>%s</%s>", 
        table_str, cellcode, addAlign2Style(style, align[nr]), cell_value, cellcode)
      
      # Add empty cell if not last column
      if (nr != length(rowcells) && cgroup_spacer_cells[nr] > 0){
        table_str <- sprintf("%s\n\t\t<%s style='%s' colspan='%d'>&nbsp;</%s>", 
          table_str, cellcode, style, cgroup_spacer_cells[nr], cellcode)
      }
    }
    return (table_str)
  }
  
  # AVI added utility function to incorporate custom (LaTeX) column alignment
  getAlign <- function(align_req) {
    tmp_align_req <- strsplit(align_req, "")[[1]]
    if (length(grep('[|]', tmp_align_req)) >0 ) { # Remove pipe(s) if they exist
      tmp_align_req <- tmp_align_req[-grep('[|]', tmp_align_req)]
    }
    sapply(tmp_align_req, function(f) c("center", "right", "left")[grep(f, c("c", "r", "l"))], USE.NAMES=FALSE)
  }
  
  setRowLabel <- function(){
    if (set_rownames && 
      length(rowlabel) > 0 &&
      nchar(rowlabel))
      return(TRUE)
    else
      return(FALSE)
  }
  
  getCgroupHeader <- function(cgroup_vec, n.cgroup_vec, cgroup_vec.just, top_row = TRUE, row_no){
    
    header_str <- "\n\t<tr>"
    if (top_row)
      ts <- top_row_style
    else
      ts <- ""

    if (setRowLabel()){
      if (row_no == rowlabel.pos)
        header_str <- sprintf("%s\n\t\t<th style='font-weight: 900; %s'>%s</th>", 
          header_str, ts, rowlabel)
      else
        header_str <- sprintf("%s\n\t\t<th style='%s'></th>", 
          header_str, ts)
    }else if (set_rownames){
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
          header_str <- sprintf("%s\n\t\t<th colspan='%d' style='%s'>&nbsp;</th>", 
            header_str, colspan, 
            addAlign2Style(sprintf("font-weight: 900; %s", ts), getAlign(strsplit(cgroup_vec.just, '')[[1]][i])))
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
      stop(sprintf("Your rows don't match in the n.rgroup, i.e. %d != %d", 
                   sum(n.rgroup), nrow(x)))
    
    # Sanity checks rgroupCSSstyle and prepares the style
    if (length(rgroupCSSstyle) > 1 &&
          length(rgroupCSSstyle) != length(rgroup))
      stop(sprintf("You must provide the same number of styles as the rgroups, %d != %d",
                   length(rgroupCSSstyle), length(rgroup)))
    else if(length(rgroupCSSstyle) == 1){
      rgroupCSSstyle <- addSemicolon2StrEnd(rgroupCSSstyle)
      
      if (length(rgroup) > 0)
        rgroupCSSstyle <- rep(rgroupCSSstyle, length.out=length(rgroup))
    } else {
      for (i in 1:length(rgroupCSSstyle))
        rgroupCSSstyle[i] <- addSemicolon2StrEnd(rgroupCSSstyle[i])
    }
    
    # Sanity checks rgroupCSSseparator and prepares the style
    if (length(rgroupCSSseparator) > 1 &&
          length(rgroupCSSseparator) != length(rgroup)-1)
      stop(sprintf("You must provide the same number of separators as the rgroups - 1, %d != %d",
                   length(rgroupCSSseparator), length(rgroup)-1))
    else if(length(rgroupCSSseparator) == 1){
      rgroupCSSseparator <- addSemicolon2StrEnd(rgroupCSSseparator)
      
      if (length(rgroup) > 0)
        rgroupCSSseparator <- rep(rgroupCSSseparator, length.out=length(rgroup))
    } else {
      for (i in 1:length(rgroupCSSseparator))
        rgroupCSSseparator[i] <- addSemicolon2StrEnd(rgroupCSSseparator[i])
    }
  }
  
  if (!missing(tspanner)){
    
    # Sanity checks tspannerCSSstyle and prepares the style
    if (length(tspannerCSSstyle) > 1 &&
          length(tspannerCSSstyle) != length(tspanner))
      stop(sprintf("You must provide the same number of styles as the tspanners, %d != %d",
                   length(tspannerCSSstyle), length(tspanner)))
    else if(length(tspannerCSSstyle) == 1){
      tspannerCSSstyle <- addSemicolon2StrEnd(tspannerCSSstyle)
      
      if (length(tspanner) > 0)
        tspannerCSSstyle <- rep(tspannerCSSstyle, length.out=length(tspanner))
    } else {
      for (i in 1:length(tspannerCSSstyle))
        tspannerCSSstyle[i] <- addSemicolon2StrEnd(tspannerCSSstyle[i])
    }
    
    
    # Sanity checks tspannerCSSseparator and prepares the style
    if (length(tspannerCSSseparator) > 1 &&
          length(tspannerCSSseparator) != length(tspanner)-1)
      stop(sprintf("You must provide the same number of separators as the tspanners - 1, %d != %d",
                   length(tspannerCSSseparator), length(tspanner)-1))
    else if(length(tspannerCSSseparator) == 1){
      tspannerCSSseparator <- addSemicolon2StrEnd(tspannerCSSseparator)
      
      if (length(tspanner) > 0)
        tspannerCSSseparator <- rep(tspannerCSSseparator, length.out=length(tspanner)-1)
    } else {
      for (i in 1:length(tspannerCSSseparator))
        tspannerCSSseparator[i] <- addSemicolon2StrEnd(tspannerCSSseparator[i])
    }
    
  }
  
  
  # Sanity check for tspanner
  if (!missing(tspanner)){
    if (missing(n.tspanner))
      stop("You need to specify the argument n.tspanner if you want to use table spanners")
    
    if(sum(n.tspanner) !=  nrow(x))
      stop(sprintf("Your rows don't match in the n.tspanner, i.e. %d != %d", 
                   sum(n.rgroup), nrow(x)))
    
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
  }
    
  no_cgroup_rows <-
    ifelse(!missing(cgroup),
           nrow(cgroup),
           0)
  if (is.numeric(rowlabel.pos)){
    if(rowlabel.pos < 1)
      stop("You have specified a rowlabel.pos that is less than 1: ", rowlabel.pos)
    else if (rowlabel.pos > no_cgroup_rows + (!missing(headings))*1)
      stop("You have specified a rowlabel.pos that more than the max limit, ",
        no_cgroup_rows + (!missing(headings))*1,
        ", you have provided: ", rowlabel.pos)
  }else{
    rowlabel.pos <- tolower(rowlabel.pos)
    if (rowlabel.pos %in% c("top"))
      rowlabel.pos <- 1
    else if (rowlabel.pos %in% c("bottom", "header"))
      rowlabel.pos <- no_cgroup_rows + (!missing(headings))*1
    else
      stop("You have provided an invalid rowlabel.pos text, only 'top', 'bottom' or 'header' are allowed, can't interpret '", rowlabel.pos, "'")
  }
  
  
  # Not quite as intended but close enough
  if(length(list(...)) > 0) x <- format.df(x, numeric.dollar=FALSE, ...)
  # Remove some specifics for LaTeX
  if (is.character(x)) x <- matrix(str_replace(x, "\\\\%", "%"), ncol=ncol(x))
  
  # The id works just as well as any anchor 
  table_id <- ""
  if (!missing(label)){
    table_id <- sprintf(" id='%s'", label) 
  }else if(is.numeric(tc)){
    table_id <- sprintf(" id='table_%d'", tc)
  }
  
  # A column counter that is used for <td colspan="">
  total_columns <- ncol(x)+set_rownames
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
    caption <- sprintf("\n\t%s%s", tc_string, caption)
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
  
  rowname_align <- getAlign("l")
  if (set_rownames && nchar(align) - 1 == ncol(x)){
    rowname_align <- getAlign(substr(align, 1,1))
    align <- substring(align, 2)
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
      table_str <- sprintf("%s%s", table_str, getCgroupHeader(cgroup_vec = cgroup[i,], 
          n.cgroup_vec = n.cgroup[i,], 
          cgroup_vec.just = cgroup.just[i, ], 
          top_row = (i == 1), row_no = i))
    }
    first_row = FALSE
  }
  
  
  # Add the headings
  if (!missing(headings)){
    # The bottom border was ment to be here but it doesn't
    # work that well in the export
    table_str <- sprintf("%s\n\t<tr>", table_str)
    ts <- ifelse(no_cgroup_rows > 0, "", top_row_style)
    if (setRowLabel() && rowlabel.pos == no_cgroup_rows + 1){
      table_str <- sprintf("%s\n\t\t<th style='font-weight: 900; border-bottom: 1px solid grey; %s'>%s</th>", 
        table_str, ts, rowlabel)
    }else if(set_rownames){
      table_str <- sprintf("%s\n\t\t<th style='border-bottom: 1px solid grey; %s'>&nbsp;</th>", 
        table_str, ts)
    }
    
    cell_style= "border-bottom: 1px solid grey;" 
    if (first_row){
      cell_style=sprintf("%s %s", cell_style, top_row_style)
    }
    table_str <- addCells(table_str = table_str, rowcells = headings, 
      cellcode = "th", align = getAlign(halign), style=cell_style)
    
    table_str <- sprintf("%s\n\t</tr>", table_str)
    first_row = FALSE
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
        rs <- sprintf("%s %s", rs,
                      tspannerCSSseparator[tspanner_iterator-1])
      }
      
      table_str <- sprintf("%s\n\t<tr><td colspan='%d' style='%s'>%s</td></tr>", table_str, 
                           total_columns, 
                           rs,
                           tspanner[tspanner_iterator])
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
        
        ## this will allow either rgroupCSSstyle or altcol to 
        ## color the rgroup label rows
        table_str <- sprintf("%s\n\t<tr style='background-color:%s;'><td colspan='%d' style='%s'>%s</td></tr>", table_str, 
                             rep(unique(rs2), length(rgroup))[rgroup_iterator],
                             total_columns, 
                             rs,
                             rgroup[rgroup_iterator])
      }
    }
    
    if (!missing(rgroup)){
      ## this will change the bgcolor of the rows, by rgroup
      table_str <- sprintf("%s\n\t<tr style='background-color:%s;'>", table_str, rs2[row_nr])
    }else{
      table_str <- sprintf("%s\n\t<tr>", table_str)
    }
    cell_style = "";
    if (row_nr == nrow(x))
      cell_style = bottom_row_style
    
    if (set_rownames){
      # Minor change from original function. If the group doesn't have 
      # a group name then there shouldn't be any indentation
      if (!missing(rgroup) && 
            rgroup_iterator > 0 && 
            is.na(rgroup[rgroup_iterator]) == FALSE &&
            rgroup[rgroup_iterator] != ""){
        
        
        # The padding doesn't work well with the Word import - well nothing really works well with word...
        # table_str <- sprintf("%s\n\t\t<td style='padding-left: .5em;'>%s</td>", table_str, rowname[row_nr])
        table_str <- sprintf("%s\n\t\t<td style='%s'>&nbsp;&nbsp;%s</td>", 
          table_str, addAlign2Style(cell_style, rowname_align), rowname[row_nr])
      }else
        table_str <- sprintf("%s\n\t\t<td style='%s'>%s</td>", 
          table_str, addAlign2Style(cell_style, rowname_align), rowname[row_nr])
    }
    
    table_str <- addCells(table_str = table_str, rowcells = x[row_nr,], 
      cellcode = "td", align=getAlign(align), style = cell_style)
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
    table_str <- sprintf("%s\n\t<tfoot><tr><td colspan=%d>", table_str, total_columns)
    
    # Add the actual tfoot to a new row
    table_str<- sprintf("%s\n\t%s", table_str, tfoot)
    
    # Close the tfoot
    table_str <- sprintf("%s</td></tr></tfoot>", table_str)
  }
  
  # Close table
  table_str <- sprintf("%s\n</table>", table_str)
  
  class(table_str) <- c("htmlTable", class(table_str))
  return(table_str)
}

setClass("htmlTable", contains = "character")

#' @rdname htmlTable
#' @S3method print htmlTable
#' @param useViewer If you are using RStudio there is a viewer thar can render 
#'  the table within that is automatically envoced unless you have the knitr 
#'  package loaded. Set this to \code{FALSE} if you want to remove that 
#'  functionality. 
print.htmlTable<- function(x, useViewer, ...){
  # Since the print may be called from another print function
  # it may be handy to allow functions to use attributes for the
  # useViewer parameter
  if (missing(useViewer)){
    if ("useViewer" %in% names(attributes(x)) &&
      is.logical(attr(x, "useViewer"))){
        useViewer <- attr(x, "useViewer")
    }else{
      useViewer <- TRUE
    }
  }
        
  # Don't use viewer if in knitr
  # As of 0.98.932 the knitr package isn't loaded, instead the
  # metadata element appears indicating that it is knitting
  if (useViewer &&
        (!"package:knitr" %in% search() ||
           length(ls(pattern = "metadata")) == 1)){

    htmlFile <- tempfile(fileext=".html")
    htmlPage <- paste("<html>",
                      "<head>",
                      "<meta http-equiv=\"Content-type\" content=\"text/html;charset=UTF-8\">",
                      "</head>",
                      "<body>",
                      "<div style=\"margin: 0 auto; display: table; margin-top: 1em;\">",
                      x,
                      "</div>",
                      "</body>",
                      "</html>", sep="\n")   
    cat(htmlPage, file=htmlFile)

    viewer <- getOption("viewer")
    if (!is.null(viewer) &&
          is.function(viewer)){
      # (code to write some content to the file)
      viewer(htmlFile)
    }else{
      utils::browseURL(htmlFile)
    }
  }else{
    cat(x)
  }
}

#' If you want a row to span two or more lines
#' 
#' This function helps you to do a multiline
#' table header in both html and in LaTeX. In
#' html this isn't that tricky, you just use 
#' the <br /> command but in LaTeX I often find
#' myself writing vbox/hbox stuff and therefore
#' I've created this simple helper function
#' 
#' @param ... The lines that you want to be joined
#' @param html If it's suppose to be in html or LaTeX.
#'  Default is LaTeX.
#' @return string 
#' 
#' @examples 
#' splitLines4Table("hello", "world")
#' splitLines4Table("hello", "world", html=TRUE)
#' splitLines4Table("hello", "world", list("A list", "is OK"))
#' 
#' 
#' @author max
#' @export
splitLines4Table <- function(..., html=FALSE){
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
    stop("You need to provide at least two valid strings to separate into multiple lines")
  
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

