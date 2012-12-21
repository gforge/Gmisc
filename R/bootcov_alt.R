#' An alternative to the rms bootcov function that catches errors in the
#' runs. 
#' 
#' The original function has a risk of failing in quantile regressions (Rq)
#' when there is a risk for a singularity matrix.
#' 
#' @param fit The model
#' @param cluster a variable indicating groupings. cluster may be any type of 
#'   vector (factor, character, integer). Unique values of cluster indicate possibly 
#'   correlated groupings of observations. Note the data used in the fit and stored 
#'   in fit$x and fit$y may have had observations containing missing values deleted. 
#'   It is assumed that if there were any NAs, an naresid function exists for the class 
#'   of fit. This function restores NAs so that the rows of the design matrix 
#'   coincide with cluster.
#' @param B number of bootstrap repetitions. Default is 200.
#' @param fitter the name of a function with arguments (x,y) that will fit 
#'   bootstrap samples. Default is taken from the class of fit if it is ols, lrm, cph, psm, Rq.
#' @param coef.reps set to TRUE if you want to store a matrix of all bootstrap regression
#'   coefficient estimates in the returned component boot.Coef. For models set 
#'   loglik=FALSE to get coef.reps=TRUE to work.
#' @param loglik set to TRUE to store -2 log likelihoods for each bootstrap model, 
#'   evaluated against the original x and y data. The default is to do this when coef.reps 
#'   is specified as TRUE. The use of loglik=TRUE assumes that an oos.loglik method exists for 
#'   the type of model being analyzed, to calculate out-of-sample -2 log likelihoods (see rmsMisc).
#'   After the B -2 log likelihoods (stored in the element named boot.loglik in the returned fit object),
#'   the B+1 element is the -2 log likelihood for the original model fit.
#' @param pr set to TRUE to print the current sample number to monitor progress.
#' @param maxit maximum number of iterations, to pass to fitter
#' @param group a grouping variable used to stratify the sample upon bootstrapping. 
#'   This allows one to handle k-sample problems, i.e., each bootstrap sample will be forced
#'   to select the same number of observations from each level of group as the number appearing 
#'   in the original dataset. You may specify both group and cluster.
#' @param stat a single character string specifying the name of a stats element 
#'   produced by the fitting function to save over the bootstrap repetitions. The vector 
#'   of saved statistics will be in the boot.stats part of the list returned by bootcov.
#' 
#' @examples 
#' library(rms)
#' set.seed(1)
#' n <- 100
#' x1 <- rnorm(n)
#' x2 <- factor(sample(c(rep("A", length=20), "B"), replace=TRUE, size=n))
#' y <- exp(x1 + (x2=="B")*2 + rnorm(n)/4)
#' dd <- datadist(x1, x2); options(datadist='dd')
#' fq <- Rq(y ~ pol(x1,2) + x2, x=TRUE, y=TRUE)
#' bootcov_alt(fq, B=500, pr=TRUE)
#' 
#' @seealso \code{\link{bootcov}}
#' @author max
#' @export
bootcov_alt <- function (fit, cluster, B = 200, fitter, coef.reps = FALSE, loglik = coef.reps, 
  pr = FALSE, maxit = 15, group = NULL, stat = NULL) 
{
  coxcph <- inherits(fit, "coxph") || inherits(fit, "cph") || 
    (length(fit$fitFunction) && any(c("cph", "coxph") %in% 
          fit$fitFunction))
  nfit <- fit$fitFunction[1]
  if (!length(nfit)) 
    nfit <- setdiff(oldClass(fit), "Design")[1]
  if (length(fit$weights) && (coxcph || nfit[1] == "Rq")) 
    stop("does not handle weights")
  if (!length(X <- fit$x) | !length(Y <- fit$y)) 
    stop("you did not specify x=TRUE and y=TRUE in the fit")
  sc.pres <- match("scale", names(fit), 0) > 0
  if (nfit == "psm") {
    fixed <- fit$fixed
    fixed <- if (length(fixed) == 1 && is.logical(fixed) && 
        !fixed) 
        list()
      else list(scale = TRUE)
    fixed <- NULL
    dist <- fit$dist
    parms <- fit$parms
  }
  if (nfit == "Glm") 
    fitFamily <- fit$family
  penalty.matrix <- fit$penalty.matrix
  if (missing(fitter)) {
    fitter <- switch(nfit, ols = if (length(penalty.matrix)) {
          function(x, y, penalty.matrix, ...) {
            lm.pfit(x, y, penalty.matrix = penalty.matrix, 
              tol = 1e-11, regcoef.only = TRUE)
          }
        } else function(x, y, ...) {
            lm.fit.qr.bare(x, y, tolerance = 1e-11, intercept = FALSE)
          }, lrm = function(x, y, maxit = 15, penalty.matrix, ...) {
        lrm.fit(x, y, maxit = maxit, tol = 1e-11, penalty.matrix = penalty.matrix)
      }, cph = function(x, y, strata = NULL, maxit = 15, ...) {
        coxphFit(x, y, strata = strata, iter.max = maxit, 
          eps = 1e-04, method = "efron", toler.chol = 1e-11, 
          type = "right")
      }, psm = function(x, y, maxit = 15, ...) {
        survreg.fit2(x, y, dist = dist, parms = parms, fixed = fixed, 
          offset = NULL, init = NULL, maxiter = maxit)
      }, bj = function(x, y, maxit = 15, eps = 1e-04, ...) {
        bj.fit(x, y, control = list(iter.max = maxit, eps = 1e-04))
      }, Glm = function(x, y, ...) {
        glm.fit(x, as.vector(y), family = fitFamily)
      }, Rq = RqFit(fit, wallow = FALSE))
  }
  if (!length(fitter)) 
    stop("fitter not valid")
  if (loglik) {
    oosl <- switch(nfit, ols = oos.loglik.ols, lrm = oos.loglik.lrm, 
      cph = oos.loglik.cph, psm = oos.loglik.psm, Glm = oos.loglik.Glm)
    if (!length(oosl)) 
      stop("loglik=TRUE but no oos.loglik method for model in rmsMisc")
    Loglik <- double(B + 1)
    Loglik[B + 1] <- oosl(fit)
  }
  else Loglik <- NULL
  n <- nrow(X)
  p <- length(fit$coef)
  vname <- names(fit$coef)
  if (sc.pres) {
    p <- p + 1
    vname <- c(vname, "log scale")
  }
  bar <- rep(0, p)
  cov <- matrix(0, nrow = p, ncol = p, dimnames = list(vname, 
      vname))
  if (coef.reps) 
    coefs <- matrix(NA, nrow = B, ncol = p, dimnames = list(NULL, 
        vname))
  if (length(stat)) 
    stats <- numeric(B)
  Y <- as.matrix(if (is.category(Y)) 
        oldUnclass(Y)
      else Y)
  ny <- ncol(Y)
  Strata <- fit$Strata
  nac <- fit$na.action
  if (length(group)) {
    if (length(group) > n) {
      if (length(nac)) {
        j <- !is.na(naresid(nac, Y) %*% rep(1, ny))
        group <- group[j]
      }
    }
    if (length(group) != n) 
      stop("length of group does not match # rows used in fit")
    group.inds <- split(1:n, group)
    ngroup <- length(group.inds)
  }
  else ngroup <- 0
  if (missing(cluster)) {
    b <- 0
    for (i in 1:B) {
      if (pr) 
        cat(i, "\r")
      if (ngroup) {
        j <- integer(n)
        for (si in 1:ngroup) {
          gi <- group.inds[[si]]
          j[gi] <- sample(gi, length(gi), replace = TRUE)
        }
      }
      else j <- sample(1:n, n, replace = TRUE)
      f <- tryCatch(f <- fitter(X[j, , drop = FALSE], Y[j, , drop = FALSE], 
          maxit = maxit, penalty.matrix = penalty.matrix, 
          strata = Strata[j]),
        error=function(e) list(fail=TRUE))
      if (length(f$fail) && f$fail) 
        next
      cof <- as.vector(f$coef)
      if (any(is.na(cof))) 
        next
      b <- b + 1
      if (sc.pres) 
        cof <- c(cof, log(f$scale))
      if (coef.reps) 
        coefs[b, ] <- cof
      if (length(stat)) 
        stats[b] <- f$stats[stat]
      bar <- bar + cof
      cof <- as.matrix(cof)
      cov <- cov + cof %*% t(cof)
      if (loglik) 
        Loglik[b] <- oosl(f, matxv(X, cof), Y)
    }
    if (pr) 
      cat("\n")
  }
  else {
    if (length(cluster) > n) {
      if (length(nac)) {
        j <- !is.na(naresid(nac, Y) %*% rep(1, ny))
        cluster <- cluster[j]
      }
    }
    if (length(cluster) != n) 
      stop("length of cluster does not match # rows used in fit")
    if (any(is.na(cluster))) 
      stop("cluster contains NAs")
    cluster <- as.character(cluster)
    clusters <- unique(cluster)
    nc <- length(clusters)
    Obsno <- split(1:n, cluster)
    b <- 0
    for (i in 1:B) {
      if (pr) 
        cat(i, "\r")
      if (ngroup) {
        j <- integer(0)
        for (si in 1:ngroup) {
          gi <- group.inds[[si]]
          cluster.gi <- cluster[gi]
          clusters.gi <- unique(cluster.gi)
          nc.gi <- length(clusters.gi)
          Obsno.gci <- split(gi, cluster.gi)
          j.gci <- sample(clusters.gi, nc.gi, replace = TRUE)
          obs.gci <- unlist(Obsno.gci[j.gci])
          j <- c(j, obs.gci)
        }
        obs <- j
      }
      else {
        j <- sample(clusters, nc, replace = TRUE)
        obs <- unlist(Obsno[j])
      }
      f <- tryCatch(f <- fitter(X[obs, , drop = FALSE], Y[obs, , drop = FALSE], 
          maxit = maxit, penalty.matrix = penalty.matrix, 
          strata = Strata[obs]),
        error=function(e) list(fail=TRUE))
      if (length(f$fail) && f$fail) 
        next
      cof <- as.vector(f$coef)
      if (any(is.na(cof))) 
        next
      b <- b + 1
      if (sc.pres) 
        cof <- c(cof, log(f$scale))
      if (coef.reps) 
        coefs[b, ] <- cof
      if (length(stat)) 
        stats[b] <- f$stats[stat]
      bar <- bar + cof
      cof <- as.matrix(cof)
      cov <- cov + cof %*% t(cof)
      if (loglik) 
        Loglik[b] <- oosl(f, matxv(X, cof), Y)
    }
    if (pr) 
      cat("\n")
  }
  if (b < B) {
    warning(paste("fit failure in", B - b, "resamples.  Might try increasing maxit"))
    if (coef.reps) 
      coefs <- coefs[1:b, , drop = FALSE]
    Loglik <- Loglik[1:b]
  }
  bar <- bar/b
  fit$B <- b
  names(bar) <- vname
  fit$boot.coef <- bar
  if (coef.reps) 
    fit$boot.Coef <- coefs
  bar <- as.matrix(bar)
  cov <- (cov - b * bar %*% t(bar))/(b - 1)
  fit$orig.var <- fit$var
  fit$var <- cov
  fit$boot.loglik <- Loglik
  if (length(stat)) 
    fit$boot.stats <- stats
  if (nfit == "Rq") {
    newse <- sqrt(diag(cov))
    newt <- fit$summary[, 1]/newse
    newp <- 2 * (1 - pt(abs(newt), fit$stats["n"] - fit$stats["p"]))
    fit$summary[, 2:4] <- cbind(newse, newt, newp)
  }
  fit
}