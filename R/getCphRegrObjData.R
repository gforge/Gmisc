#' Prints the cph output
#' 
#' This is a different way of outputting the cph outcome
#' from the rms package. I needed something a little simpler
#' than the general summary and therefore created this function.
#' 
#' @param fit The cph return object 
#' @param round How many integers to display
#' @return Returns a matrix with columns HR, 2.5%, 97.5 % and Pr(>|Z|)
#' 
#' @example examples/getCphRegrObjData_example.R
#' 
#' @author max
#' @export
getCphRegrObjData <- function (fit, round = TRUE){
    beta <- fit$coefficients
    se <- sqrt(diag(fit$var))
    Z <- fit$coefficients/se
    p_vals <- 1 - pchisq(Z^2, 1)
    ci <- exp(confint(fit))
    ci[,2][ci[,2] > 10^4] <- Inf
    if (round == TRUE){
        data <- cbind(round(exp(beta), 3), round(ci, 3), round(p_vals, 4))
    }else{
        data <- cbind(exp(beta), ci, p_vals)
    }
    
    colnames(data) <- c("HR", "2.5 %", "97,5 %", "Pr(>|Z|)")
    
    # Dump the splines
    splines <- rownames(data)[grep("[^']'$", rownames(data))]
    for(i in 1:length(splines)){
        # Clear the '
        splines[i] <- substr(rownames(data)[grep("[^']'$", rownames(data))][i], 1, nchar(rownames(data)[grep("[^']'$", rownames(data))][i])-1)
    }
    
    spline_grep <- paste("(", paste(splines, collapse="|"), ")", sep="")
    drop_rows <- grep(spline_grep, rownames(data))
    if (length(drop_rows) > 0){
        data <- data[-drop_rows, ]
    }
    
    return(data)
}
