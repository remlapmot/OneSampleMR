#' Two-stage predictor substitution (TSPS) estimators
#'
#' Terza et al. (2008) give an excellent description of TSPS estimators.
#' They proceed by fitting a first stage model of the
#' exposure regressed upon the instruments (and possibly any measured
#' confounders). From this the predicted values of the exposure are obtained.
#' A second stage model if then fitted of the outcome regressed upon
#' the predicted values of the exposure (and possibly measured confounders).
#'
#' `tsps()` performs GMM estimation to ensure appropriate standard errors
#' on its estimates.
#'
#' @inheritParams msmm
#' @param link character; one of `"identity"` (the default), `"logadd"`, `"logmult"`, `"logit"`.
#'  This is the link function for the second stage model. `"identity"` corresponds to linear
#'  regression; `"logadd"` is log-additive and corresponds to Poisson / log-binomial regression;
#'  `"logmult"` is log-multiplicative and corresponds to gamma regression;
#'  `"logit"` corresponds to logistic regression.
#' @references
#' Burgess S, CRP CHD Genetics Collaboration.
#' Identifying the odds ratio estimated by a
#' two-stage instrumental variable analysis
#' with a logistic regression model.
#' Statistics in Medicine, 2013, 32, 27, 4726-4747.
#' \doi{10.1002/sim.5871}
#'
#' Dukes O, Vansteelandt S.
#' A note on G-estimation of causal risk ratios.
#' American Journal of Epidemiology, 2018, 187, 5, 1079-1084.
#' \doi{10.1093/aje/kwx347}
#'
#' Palmer TM, Sterne JAC, Harbord RM, Lawlor DA, Sheehan NA, Meng S,
#' Granell R, Davey Smith G, Didelez V.
#' Instrumental variable estimation of causal risk ratios and causal odds ratios
#' in Mendelian randomization analyses.
#' American Journal of Epidemiology, 2011, 173, 12, 1392-1403.
#' \doi{10.1093/aje/kwr026}
#'
#' Terza JV, Basu A, Rathouz PJ. Two-stage residual inclusion estimation:
#' Addressing endogeneity in health econometric modeling.
#' Journal of Health Economics, 2008, 27, 3, 531-543.
#' \doi{10.1016/j.jhealeco.2007.09.009}
#' @export
tsps <- function(formula, instruments, data, subset, na.action,
                 contrasts = NULL,
                 t0 = NULL,
                 link = "identity",
                 ...) {
  # ivreg::ivreg() arguments I haven't implemented:
  # weights, offset,
  # model = TRUE, y = TRUE, x = FALSE,

  # code from beginning for ivreg::ivreg()
  ## set up model.frame() call
  cl <- match.call()
  if(missing(data)) data <- environment(formula)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action", "weights", "offset"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  ## handle instruments for backward compatibility
  if(!missing(instruments)) {
    formula <- Formula::as.Formula(formula, instruments)
    cl$instruments <- NULL
    cl$formula <- formula(formula)
  } else {
    formula <- Formula::as.Formula(formula)
  }
  if(length(formula)[2L] == 3L) formula <- Formula::as.Formula(
    formula(formula, rhs = c(2L, 1L), collapse = TRUE),
    formula(formula, lhs = 0L, rhs = c(3L, 1L), collapse = TRUE)
  )
  stopifnot(length(formula)[1L] == 1L, length(formula)[2L] %in% 1L:2L)
  ## try to handle dots in formula
  has_dot <- function(formula) inherits(try(terms(formula), silent = TRUE), "try-error")
  if(has_dot(formula)) {
    f1 <- formula(formula, rhs = 1L)
    f2 <- formula(formula, lhs = 0L, rhs = 2L)
    if(!has_dot(f1) & has_dot(f2)) formula <- Formula::as.Formula(f1,
                                                                  update(formula(formula, lhs = 0L, rhs = 1L), f2))
  }
  ## call model.frame()
  mf$formula <- formula
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  ## extract response, terms, model matrices
  Y <- model.response(mf, "numeric")
  mt <- terms(formula, data = data)
  mtX <- terms(formula, data = data, rhs = 1)
  X <- model.matrix(mtX, mf, contrasts)
  if(length(formula)[2] < 2L) {
    mtZ <- NULL
    Z <- NULL
  } else {
    mtZ <- delete.response(terms(formula, data = data, rhs = 2))
    Z <- model.matrix(mtZ, mf, contrasts)
  }
  ## weights and offset
  # weights <- model.weights(mf)
  # offset <- model.offset(mf)
  # if(is.null(offset)) offset <- 0
  # if(length(offset) == 1) offset <- rep(offset, NROW(Y))
  # offset <- as.vector(offset)
  # end of code from ivreg::ivreg()

  xnames <- colnames(X)
  xnames <- xnames[-1]
  znames <- colnames(Z)[-1]

  print(znames)

  covariatenames <- intersect(xnames, znames)
  covariates <- X[,covariatenames]

  tsps_env <- new.env(parent = emptyenv())
  tsps_env$xnames <- xnames
  tsps_env$znames <- znames
  tsps_env$covariatenames <- covariatenames

  print(tsps_end$xnames)
  print(tsps_end$znames)
  print(tsps_end$covariatenames)

  link <- match.arg(link, c("identity", "logadd", "logmult", "logit"))

  # check y binary
  if (link == "logit" & !all(Y %in% 0:1))
    stop("With the logit link, the outcome must be binary, i.e. take values 0 or 1.")

  # initial values
  if (is.null(t0)) {
    stage1 <- lm(X[,2] ~ -1 + Z)
    t0 <- coef(stage1)
    xhat <- fitted.values(stage1)
    if (!identical(covariatenames, character(0))) {
      xhat <- cbind(xhat, covariates)
    }
    if (link == "identity") {
      stage2 <- lm(Y ~ xhat)
    }
    else if (link == "logadd") {
      stage2 <- glm(Y ~ xhat, family = poisson(link = "log"))
    }
    else if (link == "logmult") {
      Ystar <- Y
      Ystar[Y == 0] <- 0.001
      stage2 <- glm(Ystar ~ xhat, family = Gamma(link = "log"))
    }
    else if (link == "logit") {
      stage2 <- glm(Y ~ xhat, family = binomial(link = "logit"))
    }
    t0 <- c(t0, coef(stage2))
  }

  # gmm fit
  output <- tsps_gmm(x = X[,-1], y = Y, z = Z[,-1],
                     xnames = xnames,
                     t0 = t0,
                     link = link,
                     covariatenames = covariatenames)

  class(output) <- append("tsps", class(output))
  output
}

tsps_gmm <- function(x, y, z, xnames, t0, link, covariatenames){
  x <- as.matrix(x)

  if (!identical(covariatenames, character(0))) {
    x <- x[,!(colnames(x) %in% covariatenames), drop = FALSE]
  }

  dat = data.frame(y, x, z)

  if (is.null(t0))
    t0 <- rep(0, ncol(x) + 1)

  # gmm fit
  if (link == "identity") {
    fit <- gmm::gmm(tspsIdentityMoments, x = dat, t0 = t0, vcov = "iid")
  }
  else if (link == "logadd") {
    fit <- gmm::gmm(tspsLogaddMoments, x = dat, t0 = t0, vcov = "iid")
  }
  else if (link == "logmult") {
    fit <- gmm::gmm(tspsLogmultMoments, x = dat, t0 = t0, vcov = "iid")
  }
  else if (link == "logit") {
    fit <- gmm::gmm(tspsLogitMoments, x = dat, t0 = t0, vcov = "iid")
  }

  if (fit$algoInfo$convergence != 0)
    warning("The GMM fit has not converged, perhaps try different initial parameter values")

  estci <- cbind(gmm::coef.gmm(fit), gmm::confint.gmm(fit)$test)
  colnames(estci)[1] <- "Estimate"

  reslist <- list(fit = fit,
                  estci = estci,
                  link = link)
  return(reslist)
}

tspsIdentityMoments <- function(theta, x){
  # extract variables from x
  Y <- as.matrix(x[,"y"])
  xcolstop <- length(theta)
  X <- as.matrix(x[,2:xcolstop])
  zcolstart <- 1 + length(theta) # 1 is y, length(theta) is nX
  zcolstop <- ncol(x)
  Z <- as.matrix(x[,zcolstart:zcolstop])
  nZ <- zcolstop - zcolstart + 1
  nZp1 <- nZ + 1

  # generate first stage predicted values
  if (ncol(X) == 1) {
    stage1 <- lm(X ~ Z) # TODO covariates
    xhat <- fitted.values(stage1)
  }

}

tspsLogaddMoments <- function(theta, x){
  # extract variables from x
  Y <- as.matrix(x[,"y"])
  xcolstop <- length(theta)
  X <- as.matrix(x[,2:xcolstop])
  zcolstart <- 1 + length(theta) # 1 is y, length(theta) is nX
  zcolstop <- ncol(x)
  Z <- as.matrix(x[,zcolstart:zcolstop])
  nZ <- zcolstop - zcolstart + 1
  nZp1 <- nZ + 1

  # generate first stage predicted values
  if (ncol(X) == 1) {
    stage1 <- lm(X ~ Z) # TODO covariates
    xhat <- fitted.values(stage1)
  }

}

tspsLogmultMoments <- function(theta, x){
  # extract variables from x
  Y <- as.matrix(x[,"y"])
  xcolstop <- length(theta)
  X <- as.matrix(x[,2:xcolstop])
  zcolstart <- 1 + length(theta) # 1 is y, length(theta) is nX
  zcolstop <- ncol(x)
  Z <- as.matrix(x[,zcolstart:zcolstop])
  nZ <- zcolstop - zcolstart + 1
  nZp1 <- nZ + 1

  # generate first stage predicted values
  if (ncol(X) == 1) {
    stage1 <- lm(X ~ Z) # TODO covariates
    xhat <- fitted.values(stage1)
  }

}

tspsLogitMoments <- function(theta, x){
  # extract variables from x
  Y <- as.matrix(x[,"y"])
  xcolstop <- 2
  X <- as.matrix(x[,2:xcolstop])
  zcolstart <- 3 # 1 is y, length(theta) is nX
  zcolstop <- ncol(x)
  print(zcolstop)
  Z <- as.matrix(x[,zcolstart:zcolstop])
  nZ <- zcolstop - zcolstart + 1
  nZp1 <- nZ + 1
  Zwithcons <- cbind(rep(1, nrow(x)), Z)
  cend <- ncol(Z)
  cend2 <- cend + 1

  # generate first stage predicted values
  if (ncol(X) == 1) {
    stage1 <- lm(X ~ Z)
    xhat <- fitted.values(stage1)
  }

  if (cend2 >= nZp1) {
    print(head(Z))
    print(nZp1)
    print(head(covariates))
    covariates <- Z[,nZp1:cend]

    xhat <- cbind(xhat, covariates)
  }

  linearpredictor <- Zwithcons %*% theta[1:cend2] # only need first stage subset of theta

  # moments
  moments <- matrix(nrow = nrow(x), ncol = length(theta), NA)

  moments[,1] <- (X - linearpredictor)

  end1 <- 1 + nZ
  for (i in 2:end1) {
    moments[,i] <- (X - linearpredictor)*Zwithcons[,i]
  }

  if (cend >= nZp1) {
    stage2linpred <- cbind(linearpredictor, covariates)
  }
  else {
    stage2linpred <- linearpredictor
  }

  print(head(stage2linpred))
  print(theta)
  start2 <- cend2 + 1
  thetastart <- start2 + 1
  print(thetastart)
  thetaend <- length(theta)
  print(thetaend)
  moments[,start2] <- (Y - plogis(theta[start2] + stage2linpred %*% theta[thetastart:thetaend]))

  start3 <- cend2 + 2
  j <- 1
  for (i in start3:thetaend) {
    moments[,i] <- (Y - plogis(theta[start2] + stage2linpred %*% theta[thetastart:thetaend]))*xhat[,j]
    j <- j + 1
  }

  return(moments)
}

#' Summarizing TSPS Fits
#'
#' @param object an object of class `"tsps"`.
#' @param x an object of class `"summary.tsps"`.
#' @param digits the number of significant digits to use when printing.
#' @param ... further arguments passed to or from other methods.
#'
#' S3 summary and print methods for objects of class `tsps` and `summary.tsps`.
#'
#' @return `summary.tsps()` returns an object of class `"summary.tsps"`. A list with the following elements:
#'
#' \item{smry}{An object from a call to [`gmm::summary.gmm()`]}
#' \item{object}{The object of class `tsps` passed to the function.}
#'
#' @examples
#' # For examples see the examples at the bottom of help('tsps')
#' @export
summary.tsps <- function(object, ...) {

  smry <- gmm::summary.gmm(object$fit)

  res <- list(smry = smry,
              object = object)

  class(res) <- append(class(res), "summary.tsps")
  return(res)
}

#' @rdname summary.tsps
#' @export
print.tsps <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\n")
  cat("Link function:", x$link)
  cat("\n")

  cat("\n")
  gmm::print.gmm(x$fit)

  cat("\nEstimates with 95% CI limits:\n")
  print(x$estci, digits = digits, ...)

  cat("\n")
  invisible(x)
}

#' @rdname summary.tsps
#' @export
print.summary.tsps <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\n")

  cat("\nGMM fit summary:\n")
  gmm::print.summary.gmm(x$smry)

  cat("\nEstimates with 95% CI limits:\n")
  print(x$object$estci, digits = digits, ...)

  cat("\n")
  invisible(x)
}
