\name{simpleRmsAnova}
\alias{print.simpleRmsAnova}
\alias{simpleRmsAnova}
\title{A simpler latex output of the latex.anova.rms}
\usage{
simpleRmsAnova(anova_output, subregexps = NA, digits = 4,
  pval_threshold = 10^-4, rowlabel = "Variable", ...)

\method{print}{simpleRmsAnova}(x, html = TRUE, ...)
}
\arguments{
  \item{anova_output}{An object from the
  \code{\link{anova}()} function}

  \item{subregexps}{A 2 column matrix with sub() regular
  expressions to search for and their substitutions. The
  regular expression should be located in column 1 and the
  substitution in column 2.}

  \item{digits}{Number of digits in using the round}

  \item{rowlabel}{The label of the rows}

  \item{pval_threshold}{The threshold before setting "<",
  default is < 0.0001}

  \item{...}{Passed on to latex() or htmlTable}

  \item{x}{The output object from the SimpleRmsAnova
  function}

  \item{html}{If HTML output through the htmlTable should
  be used instead of traditional latex() function}
}
\value{
void See the latex() function
}
\description{
The original problem is that the anova default function
output is very detailed and cause a complaint in
Sweave/knitr that \\hbox is overfull. It basically changes
capitalized TOTAL, TOTAL INTERACTION and TOTAL NONLINEAR
INTERACTION into lower case letters. It also deletes the
(Factor + Higher Order Factors).
}
\examples{
# ** Borrowed code from the lrm example **

#Fit a logistic model containing predictors age, blood.pressure, sex
#and cholesterol, with age fitted with a smooth 5-knot restricted cubic 
#spline function and a different shape of the age relationship for males 
#and females.  

n <- 1000    # define sample size
set.seed(17) # so can reproduce the results
age            <- rnorm(n, 50, 10)
blood.pressure <- rnorm(n, 120, 15)
cholesterol    <- rnorm(n, 200, 25)
sex            <- factor(sample(c('female','male'), n,TRUE))
label(age)            <- 'Age'      # label is in Hmisc
label(cholesterol)    <- 'Total Cholesterol'
label(blood.pressure) <- 'Systolic Blood Pressure'
label(sex)            <- 'Sex'
units(cholesterol)    <- 'mg/dl'   # uses units.default in Hmisc
units(blood.pressure) <- 'mmHg'

#To use prop. odds model, avoid using a huge number of intercepts by
#grouping cholesterol into 40-tiles

# Specify population model for log odds that Y=1
L <- .4*(sex=='male') + .045*(age-50) +
     (log(cholesterol - 10)-5.2)*(-2*(sex=='female') + 2*(sex=='male'))
# Simulate binary y to have Prob(y=1) = 1/[1+exp(-L)]
y <- ifelse(runif(n) < plogis(L), 1, 0)
cholesterol[1:3] <- NA   # 3 missings, at random

ddist <- datadist(age, blood.pressure, cholesterol, sex)
options(datadist='ddist')

fit_lrm <- lrm(y ~ blood.pressure + sex * (age + rcs(cholesterol,4)),
               x=TRUE, y=TRUE)
             
a_out <- anova(fit_lrm, 
               dec.F = 1,
               ss    = FALSE)
             
simpleRmsAnova(a_out, 
               subregexps = rbind(c("age", "Age"),
                                  c("cholesterol", "Cholesterol"),
                                  c("sex", "Sex")),
               file="",
               where="!htbp",
               label="tab:Anova4lrm",
               caption="Anova output for a logistic regression model",
               ctable=FALSE)
}
\author{
max
}

