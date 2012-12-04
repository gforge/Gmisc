\name{bootcov_alt}
\alias{bootcov_alt}
\title{An alternative to the rms bootcov function that catches errors in the
runs.}
\usage{
  bootcov_alt(fit, cluster, B = 200, fitter,
    coef.reps = FALSE, loglik = coef.reps, pr = FALSE,
    maxit = 15, group = NULL, stat = NULL)
}
\arguments{
  \item{fit}{The model}

  \item{cluster}{a variable indicating groupings. cluster
  may be any type of vector (factor, character, integer).
  Unique values of cluster indicate possibly correlated
  groupings of observations. Note the data used in the fit
  and stored in fit$x and fit$y may have had observations
  containing missing values deleted.  It is assumed that if
  there were any NAs, an naresid function exists for the
  class of fit. This function restores NAs so that the rows
  of the design matrix coincide with cluster.}

  \item{B}{number of bootstrap repetitions. Default is
  200.}

  \item{fitter}{the name of a function with arguments (x,y)
  that will fit bootstrap samples. Default is taken from
  the class of fit if it is ols, lrm, cph, psm, Rq.}

  \item{coef.reps}{set to TRUE if you want to store a
  matrix of all bootstrap regression coefficient estimates
  in the returned component boot.Coef. For models set
  loglik=FALSE to get coef.reps=TRUE to work.}

  \item{loglik}{set to TRUE to store -2 log likelihoods for
  each bootstrap model, evaluated against the original x
  and y data. The default is to do this when coef.reps is
  specified as TRUE. The use of loglik=TRUE assumes that an
  oos.loglik method exists for the type of model being
  analyzed, to calculate out-of-sample -2 log likelihoods
  (see rmsMisc).  After the B -2 log likelihoods (stored in
  the element named boot.loglik in the returned fit
  object), the B+1 element is the -2 log likelihood for the
  original model fit.}

  \item{pr}{set to TRUE to print the current sample number
  to monitor progress.}

  \item{maxit}{maximum number of iterations, to pass to
  fitter}

  \item{group}{a grouping variable used to stratify the
  sample upon bootstrapping.  This allows one to handle
  k-sample problems, i.e., each bootstrap sample will be
  forced to select the same number of observations from
  each level of group as the number appearing in the
  original dataset. You may specify both group and
  cluster.}

  \item{stat}{a single character string specifying the name
  of a stats element produced by the fitting function to
  save over the bootstrap repetitions. The vector of saved
  statistics will be in the boot.stats part of the list
  returned by bootcov.}
}
\description{
  The original function has a risk of failing in quantile
  regressions (Rq) when there is a risk for a singularity
  matrix.
}
\examples{
library(rms)
set.seed(1)
n <- 100
x1 <- rnorm(n)
x2 <- factor(sample(c(rep("A", length=20), "B"), replace=TRUE, size=n))
y <- exp(x1 + (x2=="B")*2 + rnorm(n)/4)
dd <- datadist(x1, x2); options(datadist='dd')
fq <- Rq(y ~ pol(x1,2) + x2, x=TRUE, y=TRUE)
bootcov_alt(fq, B=500, pr=T)
}
\author{
  max
}

