# A simple example
# note that this won't show due to the second example
mx <- matrix(1:6, ncol=3) 
rownames(mx) <- LETTERS[1:NROW(mx)] 
colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
htmlTable(mx, n.rgroup=c(2), rgroup=c("Nice!"),
  n.cgroup=c(2,1), cgroup=c("First", "Second"))

## altcol does not break rgroupCSSstyle
htmlTable(mx, n.rgroup=c(2), rgroup=c("Nice!"),
          n.cgroup=c(2,1), cgroup=c("First", "Second"),
          rgroupCSSstyle = "font-weight:900; background-color:#f2f2f2;")


# A slightly more advanced example
data(mtcars)

label(mtcars$mpg) <- "Gas"
units(mtcars$mpg) <- "Miles/(US) gallon"

label(mtcars$wt) <- "Weight"
units(mtcars$wt) <- "10^3 kg" # not sure the unit is correct 

mtcars$am <- factor(mtcars$am, levels=0:1, labels=c("Automatic", "Manual"))
label(mtcars$am) <- "Transmission"

mtcars$gear <- factor(mtcars$gear)
label(mtcars$gear) <- "Gears"

# Make up some data for making it slightly more interesting
mtcars$col <- factor(sample(c("red", "black", "silver"), 
                            size=NROW(mtcars), replace=TRUE))
label(mtcars$col) <- "Car color"

mpg_data_mean <- getDescriptionStatsBy(mtcars$mpg, mtcars$am, 
                                       use_units = TRUE, html=TRUE)
mpg_data_median <- getDescriptionStatsBy(mtcars$mpg, mtcars$am, 
                                         use_units = TRUE, html=TRUE, 
                                         continuous_fn=describeMedian)
wt_data <- getDescriptionStatsBy(mtcars$wt, mtcars$am, 
                                 use_units = TRUE, html=TRUE)

vars <- rbind(mpg_data_mean, mpg_data_median, wt_data)
rownames(vars) <- c("Mean (SD)", "Median (IQR)", "Mean (SD)")
htmlTable(
  vars,
  caption  = "Continuous & binary variables", 
  n.rgroup = c(2,1), rgroup=c("Gas", "Weight"),
  n.cgroup = c(2,1), 
  cgroup   = c(splitLines4Table("Results", 
                                sprintf("n=%d", NROW(mtcars)), html=TRUE),
               ""),
  headings =c(sprintf("%s (SD)", levels(mtcars$am)), "Units"),
  rowlabel = "Variable",
  ctable   = TRUE)

## again with altcol
htmlTable(
  vars,
  caption  = "Continuous & binary variables", 
  n.rgroup = c(2,1), rgroup=c("Gas", "Weight"),
  n.cgroup = c(2,1), 
  cgroup   = c(splitLines4Table("Results", 
                                sprintf("n=%d", NROW(mtcars)), html=TRUE),
               ""),
  headings =c(sprintf("%s (SD)", levels(mtcars$am)), "Units"),
  rowlabel = "Variable",
  ctable   = TRUE,
  altcol   = c('#f2f2f2','ivory'))


## another example
describeMedian_minmax <- function(...) describeMedian(..., iqr = FALSE)

## t1 wrapper function 1
getT1stat <- function(varname, digits = 0) {
  getDescriptionStatsBy(data[ , varname],
                        data$treat,
                        add_total_col = TRUE, 
                        show_all_values = TRUE,
                        hrzl_prop = FALSE,
                        statistics = FALSE,
                        html = TRUE,
                        digits = digits,
                        continuous_fn = describeMedian_minmax)
}

set.seed(1)
f <- function(...) sample(..., 100, replace = TRUE)
data <- data.frame(age = rpois(100, 50),
                   cat_var = f(LETTERS[1:5]),
                   sex = f(c('Male','Female')),
                   race = f(c('Black','White','Asian')),
                   treat = factor(f(1:3),
                                  # The factor helps arranging the order
                                  labels=c('Treatment A', 'Treatment B', 'Placebo')))

## table 1 stats
table_data <- list()
table_data[['Age']] <- getT1stat('age')
table_data[['Some categorical<br>&nbsp&nbspvariable']] <- getT1stat('cat_var')
table_data[['Sex']] <- getT1stat('sex')
table_data[['Race']] <- getT1stat('race')

## combine into matrix
output_data <- do.call(rbind, table_data)
rgroup <- names(table_data)
n.rgroup <- unname(sapply(rgroup, function(x) nrow(table_data[[x]])))

# add a column spanner for the status columns
cgroup <- c("", "Type of treatment<sup>&dagger;</sup>")
n.cgroup <- c(1, 3) 
colnames(output_data) <- 
  c(paste0('Total<br><font weight = normal; size = 1>n = ',
           nrow(data), '</font>'),
    paste0('Treated A<br><font weight = normal; size = 1>n = ',
           sum(data$treat == 'Treatment A'),'</font>'),
    paste0('Treatment B&Dagger;<br><font weight = normal; size = 1>n = ',
           sum(data$treat == 'Treatment B'),'</font>'),
    paste0('Placebo<br><font weight = normal; size = 1>n = ',
           sum(data$treat == 'Placebo'),'</font>'))


htmlTable(output_data, align = 'rccc',
          rgroup = rgroup, n.rgroup = n.rgroup, 
          rgroupCSSseparator = '', 
          cgroup = cgroup,
          n.cgroup = n.cgroup,
          rowlabel = '', 
          ctable = TRUE, # latex-style table lines
          caption = "Table 1: Patient demographics", 
          altcol = c('white','lightblue1'),
          tfoot = paste0(
            '<font size=1>Abbreviations: ECOG, Eastern Cooperative Oncology Group; PS, performance score</font><br>',
            "<font size=1><sup>&dagger;</sup>Note 1. Trial groups for a new wonder drug</font><br>",
            "<font size = 1><sup>&Dagger;</sup>Note 2. Twice the dosage of treatment A</font>"))