#' Two-stage residual inclusion (TSRI) estimators
#'
#' An excellent description of TSRI estimators is given by Terza et al.
#' (2008). TSRI estimators proceed by fitting a first stage model of the
#' exposure regressed upon the instruments (and possibly any measured
#' confounders). From this the first stage residuals are estimated.
#' A second stage model if then fitted of the outcome regressed upon
#' the exposure and first stage residuals (and possibly measured
#' confounders).
#'
#' TSRI estimators are sometimes described as a special case of
#' control function estimators.
#'
#' `tsri()` performs GMM estimation to ensure appropriate standard errors
#' on its estimates similar to that described that described by
#' Clarke et al. (2015). Terza (2017) described an alternative approach.
#'
#' @inheritParams msmm
#' @inheritParams tsps
#' @references
#' Bowden J, Vansteelandt S.
#' Mendelian randomization analysis of case-control data using
#' structural mean models.
#' Statistics in Medicine, 2011, 30, 6, 678-694.
#' \doi{10.1002/sim.4138}
#'
#' Clarke PS, Palmer TM, Windmeijer F. Estimating structural
#' mean models with multiple instrumental variables using the
#' Generalised Method of Moments. Statistical Science, 2015, 30, 1,
#' 96-117. \doi{10.1214/14-STS503}
#'
#' Dukes O, Vansteelandt S.
#' A note on G-estimation of causal risk ratios.
#' American Journal of Epidemiology, 2018, 187, 5, 1079-1084.
#' \doi{10.1093/aje/kwx347}
#'
#' Palmer T, Thompson JR, Tobin MD, Sheehan NA, Burton PR.
#' Adjusting for bias and unmeasured confounding in Mendelian randomization
#' studies with binary responses.
#' International Journal of Epidemiology, 2008, 37, 5, 1161-1168.
#' \doi{10.1093/ije/dyn080}
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
#'
#' Terza JV.
#' Two-stage residual inclusion estimation: A
#' practitioners guide to Stata implementation.
#' The Stata Journal, 2017, 17, 4, 916-938.
#' \doi{10.1177/1536867X1801700409}
#'
#' @importFrom stats lm residuals glm poisson Gamma binomial
#' @export
tsri <- function(formula, instruments, data, subset, na.action,
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
  covariatenames <- intersect(xnames, znames)

  tsri_env <<- new.env(parent = emptyenv())
  tsri_env$anycovs <- FALSE
  if (!identical(covariatenames, character(0))) {
    tsri_env$anycovs <- TRUE
    covariates <- X[,covariatenames]
  }

  tsri_env$xnames <- xnames[!(xnames %in% covariatenames)]
  tsri_env$znames <- znames[!(znames %in% covariatenames)]
  tsri_env$covariatenames <- covariatenames

  link <- match.arg(link, c("identity", "logadd", "logmult", "logit"))

  # check y binary
  if (link == "logit" & !all(Y %in% 0:1))
    stop("With the logit link, the outcome must be binary, i.e. take values 0 or 1.")

  # initial values
  if (is.null(t0)) {
    stage1 <- lm(X[,2] ~ -1 + Z)
    t0 <- coef(stage1)
    res <- residuals(stage1)
    if (tsri_env$anycovs) {
      res <- cbind(res, covariates)
    }
    if (link == "identity") {
      stage2 <- lm(Y ~ X[,2] + res)
    }
    else if (link == "logadd") {
      stage2 <- glm(Y ~ X[,2] + res, family = poisson(link = "log"))
    }
    else if (link == "logmult") {
      Ystar <- Y
      Ystar[Y == 0] <- 0.001
      stage2 <- glm(Ystar ~ X[,2] + res, family = Gamma(link = "log"),
                    control = list(maxit = 1E5))
    }
    else if (link == "logit") {
      stage2 <- glm(Y ~ X[,2] + res, family = binomial(link = "logit"))
    }
    t0 <- c(t0, coef(stage2))
    index <- 1 + 2*length(covariatenames) + 3
    names(t0)[index] <- tsri_env$xnames
  }

  Xtopass <- as.data.frame(X[, tsri_env$xnames])
  colnames(Xtopass) <- tsri_env$xnames

  Ztopass <- as.data.frame(Z[, -1])
  if (tsri_env$anycovs) {
    colnames(Ztopass) <- c(tsri_env$znames, tsri_env$covariatenames)
  }
  else {
    colnames(Ztopass) <- tsri_env$znames
  }

  # gmm fit
  output <- tsri_gmm(x = Xtopass, y = Y, z = Ztopass,
                     xnames = xnames,
                     t0 = t0,
                     link = link)
  rm(tsri_env)
  class(output) <- append("tsri", class(output))
  output
}

tsri_gmm <- function(x, y, z, xnames, t0, link){
  x <- as.matrix(x)

  if (!identical(tsri_env$covariatenames, character(0))) {
    x <- x[,!(colnames(x) %in% tsri_env$covariatenames), drop = FALSE]
  }

  dat = data.frame(y, x, z)

  if (is.null(t0))
    t0 <- rep(0, ncol(x) + 1)

  # gmm fit
  if (link == "identity") {
    fit <- gmm::gmm(tsriIdentityMoments, x = dat, t0 = t0, vcov = "iid")
  }
  else if (link == "logadd") {
    fit <- gmm::gmm(tsriLogaddMoments, x = dat, t0 = t0, vcov = "iid")
  }
  else if (link == "logmult") {
    fit <- gmm::gmm(tsriLogmultMoments, x = dat, t0 = t0, vcov = "iid",
                    itermax = 1E7)
  }
  else if (link == "logit") {
    fit <- gmm::gmm(tsriLogitMoments, x = dat, t0 = t0, vcov = "iid")
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

#' @importFrom stats lm residuals
tsriIdentityMoments <- function(theta, x){
  # extract variables from x
  Y <- as.matrix(x[,"y"])
  X <- x[, tsri_env$xnames]
  Z <- x[, tsri_env$znames]
  nZ <- ncol(Z)
  if (tsri_env$anycovs) {
    covariates <- x[, tsri_env$covariatenames]
    ncovariates <- length(tsri_env$covariatenames)
    Z <- as.matrix(cbind(Z, covariates))
  }
  Zwithcons <- as.matrix(cbind(rep(1, nrow(x)), Z))
  stage1end <- ncol(Zwithcons)
  thetastage1 <- theta[1:stage1end]
  stage2start <- stage1end + 1
  thetaend <- length(theta)
  thetastage2 <- theta[stage2start:thetaend]
  thetacausal <- thetastage2[2]
  thetares <- thetastage2[3]
  thetastage2rescov <- thetastage2[3:length(thetastage2)]

  # generate first stage residuals
  if (length(tsri_env$xnames) == 1) {
    stage1 <- lm(X ~ Z)
    res <- as.matrix(residuals(stage1))
    res <- cbind(X, res)
  }

  if (tsri_env$anycovs) {
    res <- cbind(res, covariates)
  }

  linearpredictor <- Zwithcons %*% as.matrix(thetastage1)

  # moments
  moments <- matrix(nrow = nrow(x), ncol = length(theta), NA)

  moments[,1] <- (X - linearpredictor)

  for (i in 2:stage1end) {
    moments[,i] <- (X - linearpredictor)*Zwithcons[,i]
  }

  if (tsri_env$anycovs) {
    stage2linpred <- as.matrix(cbind(linearpredictor, covariates))
    stage2express <- (Y - (theta[stage2start] +
                             thetacausal*X +
                             thetares * (X - as.matrix(stage2linpred) +
                             covariates %*% as.matrix(thetacov))))
  }
  else {
    stage2linpred <- linearpredictor
    stage2express <- (Y - (theta[stage2start] +
                             thetacausal*X +
                             thetares * (X - as.matrix(stage2linpred))))
  }

  thetastart <- stage2start + 1

  moments[,stage2start] <- stage2express

  start3 <- stage2start + 1
  j <- 1
  for (i in start3:thetaend) {
    moments[,i] <- (stage2express)*res[,j]
    j <- j + 1
  }

  return(moments)
}

#' Summarizing TSRI Fits
#'
#' @param object an object of class `"tsri"`.
#' @param x an object of class `"summary.tsri"`.
#' @param digits the number of significant digits to use when printing.
#' @param ... further arguments passed to or from other methods.
#'
#' S3 summary and print methods for objects of class `tsri` and `summary.tsri`.
#'
#' @return `summary.tsri()` returns an object of class `"summary.tsri"`. A list with the following elements:
#'
#' \item{smry}{An object from a call to [`gmm::summary.gmm()`]}
#' \item{object}{The object of class `tsps` passed to the function.}
#'
#' @examples
#' # For examples see the examples at the bottom of help('tsri')
#' @export
summary.tsri <- function(object, ...) {

  smry <- gmm::summary.gmm(object$fit)

  res <- list(smry = smry,
              object = object)

  class(res) <- append(class(res), "summary.tsri")
  return(res)
}

#' @rdname summary.tsri
#' @export
print.tsri <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\nEstimation method:", x$estmethod, "\n\n")
  gmm::print.gmm(x$fit)

  cat("\nEstimates with 95% CI:\n")
  print(x$estci, digits = digits, ...)

  cat("\n")
  invisible(x)
}

#' @rdname summary.tsri
#' @export
print.summary.tsri <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\nGMM fit summary:\n")
  gmm::print.summary.gmm(x$smry)

  cat("\nEstimates with 95% CI:\n")
  print(x$object$estci, digits = digits, ...)

  cat("\n")
  invisible(x)
}
