#' A function that returns a description statistic that can be used
#' for creating a publication "table 1" when you want it by groups.
#' The function identifies if the variable is a continuous, binary 
#' or a factored variable. The format is inspired by NEJM, Lancet &
#' BMJ.
#' 
#' @param x The variable that you want the statistics for
#' @param by The variable that you want to split into different 
#'  columns
#' @param vars If this statistic is for a continuous or a binary 
#'  statistic it's easy to append the current statistic to the
#'  previous
#' @param digits The number of decimals used 
#' @param html If HTML compatible output shoudl be used instead
#'  of default LaTeX
#' @param NEJMstyle Adds - no (\%) at the end to proportions
#' @param numbers_first If the number should be given or if the percentage
#'  should be presented first. The second is encapsulated in parentheses ().
#' @param show_missing Show the missing values. This adds another row if 
#'  there are any missing values.
#' @param continuous_function The method to describe continuous variables. The
#'  default is @seealso describe_mean.
#' @param prop_function The method used to describe proportions. 
#' @param factor_function The method used to describe factors
#' @param statistics Add statistics, fisher test for proportions and Wilcoxon
#'  for continuous variables
#' @param min_pval The minimum p-value before doing a "< 0.0001"
#' @return Returns a vector if vars wasn't specified and it's a
#'  continuous or binary statistic. If vars was a matrix then it
#'  appends the result to the end of that matrix. If the x variable
#'  is a factor then it does not append and you get a warning.
#' 
#' @example examples/getDescriptionStatsBy_example.R
#' 
#' @author max
#' @export
getDescriptionStatsBy <- 
   function(x, by,
     vars=FALSE, digits=1, 
     html = FALSE, NEJMstyle = FALSE, 
     numbers_first = TRUE, 
     statistics=FALSE, min_pval = 10^-4,
     show_missing = FALSE,
     continuous_function = describe_mean,
     prop_function = describe_prop,
     factor_function = describe_factors){
     
     getFormattedPval <- function(p_val){
       if (p_val < min_pval)
         return(sprintf("< %s", format(min_pval, scientific=FALSE)))
       return(format(p_val, digits=2, scientific=FALSE))
     }
     
     addEmptyValuesToMakeListCompatibleWithMatrix <- function(t){
       # The list doesn't have to be equally long
       # and we need to add empty values to the end
       # or the matrix won't work. This happens when
       # we calculate missing and one side lacks missing
       elements <- length(unlist(t))
       rows <- ceiling(elements/length(t))
       columns <- length(t)
       if (rows * columns != elements){
         max_length <- 0
         for (i in 1:length(t)){
           if (max_length < length(t[[i]]))
             max_length <- length(t[[i]])
         }
         
         for (i in 1:length(t)){
           if (max_length > length(t[[i]]))
             t[[i]] <- c(t[[i]], rep("", times=max_length - length(t[[i]])))
         }
       }
       return(t)
     }
     
     if (is.numeric(x) ||
       (is.factor(x) && length(levels(x)) == 2)){
       if (is.factor(x)){
         t <- by(x, by, FUN=prop_function, html=html, digits=digits,
           number_first=numbers_first, show_missing = show_missing)
         name <- sprintf("%s %s", capitalize(levels(x)[1]), tolower(label(x)))
         if (NEJMstyle) {
           if (html) {
             name <- sprintf("%s - no (%%)", name)
           } else {
             name <- sprintf("%s - no (\\%%)", name)
           }
         }
         if (statistics){
           if (length(unique(x))*length(unique(by)) < 3*3)
             pval <- fisher.test(x, by, workspace=20)$p.value
           else
             pval <- fisher.test(x, by, workspace=20, simulate.p.value=TRUE)$p.value
           
           pval <- getFormattedPval(pval)
           
         }
       }else{
         t <- by(x, by, FUN=continuous_function, html=html, digits=digits,
           number_first=numbers_first, show_missing = show_missing)
         name <- label(x)
         if (statistics)
           pval <- getFormattedPval(wilcox.test(x ~ by)$p.value)
       }
       t <- addEmptyValuesToMakeListCompatibleWithMatrix(t)
       results <- matrix(unlist(t), ncol=length(t))
       if (class(t[[1]]) == "matrix")
         rownames(results) <- rownames(t[[1]])
       else
         rownames(results) <- names(t[[1]])
       
       if (is.matrix(vars)){
         rn <- rownames(vars)
         rn <- c(rn, rownames(name))
         if (NCOL(vars) == length(unique(by)) + 1 + statistics*1){
           if (units(x) != "") unitcol <- rep(sprintf("%s",units(x)), times=NROW(results))
           else unitcol <- rep("-", times=NROW(results))
           unitcol[rownames(results) == "Missing"] <- ""
           results <- cbind(results, unitcol)
         }
         
         if (statistics){
           results <- cbind(results,
             c(sprintf("%s", pval), rep("", times=NROW(results)-1)))
         }
         vars <- rbind(vars, results)
         rownames(vars) <- rn
       }else{
         if (units(x) != ""){
           unitcol <- rep(sprintf("%s",units(x)), times=NROW(results))
           unitcol[rownames(results) == "Missing"] <- ""
           results <- cbind(results, unitcol)
         }
         
         if (statistics){
           results <- cbind(results,
             c(sprintf("%s", pval), rep("", times=NROW(results)-1)))
         }
         vars <- results
       }
     }else{
       if (is.matrix(vars))
         warning("The function cannot add a factor variable to a previous matrix")
       
       t <- by(x, by, FUN=factor_function, html=html, digits=digits,
         number_first=numbers_first, show_missing = show_missing)
       
       t <- addEmptyValuesToMakeListCompatibleWithMatrix(t)
       vars <- matrix(unlist(t), ncol=length(unique(by)))
       label(vars) <- label(x)
       if (class(t[[1]]) == "matrix")
         rownames(vars) <- rownames(t[[1]])
       else
         rownames(vars) <- names(t[[1]])
       if (statistics){
         # This is a quick fix in case of large dataset
         workspace = 10^5
         if (length(x)*length(levels(x)) > 10^4)
           workspace = 10^7
         # Large statistics tend to be very heavy and therefore
         # i need to catch errors in fisher and redo by simulation
         pval <- tryCatch({fisher.test(x, by, workspace=workspace)$p.value},
           error = function(err){
             warning("Simulating p-value for fisher due to high computational demands on the current varible")
             fisher.test(x, by, simulate.p.value=TRUE, B=10^5)$p.value
           })
         pval <- getFormattedPval(pval)
         vars <- cbind(vars, c(pval, rep("", nrow(vars)-1)))
       }
     }
     
     return (vars)
   }