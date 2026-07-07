#' Two-stage residual inclusion (TSRI) estimators
#'
#' An excellent description of TSRI estimators is given by Terza et al.
#' (2008). TSRI estimators proceed by fitting a first stage model of the
#' exposure regressed upon the instruments (and possibly any measured
#' confounders). From this the first stage residuals are estimated.
#' A second stage model is then fitted of the outcome regressed upon
#' the exposure and first stage residuals (and possibly measured
#' confounders).
#'
#' TSRI estimators are sometimes described as a special case of
#' control function estimators.
#'
#' `tsri()` performs GMM estimation to ensure appropriate standard errors
#' on its estimates similar to that described by
#' Clarke et al. (2015). Terza (2017) described an alternative approach.
#'
#' @inheritParams msmm
#' @inheritParams tsps
#' @return
#' An object of class `"tsri"` with the following elements
#' \describe{
#' \item{fit}{the fitted object of class `"gmm"` from the call to [gmm::gmm()].}
#' \item{estci}{a matrix of the estimates with their corresponding confidence interval limits.}
#' \item{link}{a character vector containing the specified link function.}
#' }
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
#' @examples
#' # Two-stage residual inclusion estimator
#' # with second stage logistic regression
#' set.seed(9)
#' n            <- 1000
#' psi0         <- 0.5
#' Z            <- rbinom(n, 1, 0.5)
#' X            <- rbinom(n, 1, 0.7*Z + 0.2*(1 - Z))
#' m0           <- plogis(1 + 0.8*X - 0.39*Z)
#' Y            <- rbinom(n, 1, plogis(psi0*X + log(m0/(1 - m0))))
#' dat          <- data.frame(Z, X, Y)
#' tsrilogitfit <- tsri(Y ~ X | Z , data = dat, link = "logit")
#' summary(tsrilogitfit)
#' @export
tsri <- function(
  formula,
  instruments,
  data,
  subset,
  na.action,
  contrasts = NULL,
  t0 = NULL,
  link = "identity",
  ...
) {
  # ivreg::ivreg() arguments I haven't implemented:
  # weights, offset,
  # model = TRUE, y = TRUE, x = FALSE,

  rlang::check_dots_used()

  # code from beginning for ivreg::ivreg()
  ## set up model.frame() call
  if (missing(data)) {
    data <- environment(formula)
  }
  mf <- match.call(expand.dots = FALSE)
  m <- match(
    c("formula", "data", "subset", "na.action", "weights", "offset"),
    names(mf),
    0
  )
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  ## handle instruments for backward compatibility
  if (!missing(instruments)) {
    formula <- Formula::as.Formula(formula, instruments)
  } else {
    formula <- Formula::as.Formula(formula)
  }
  if (length(formula)[2L] == 3L) {
    formula <- Formula::as.Formula(
      formula(formula, rhs = c(2L, 1L), collapse = TRUE),
      formula(formula, lhs = 0L, rhs = c(3L, 1L), collapse = TRUE)
    )
  }
  stopifnot(length(formula)[1L] == 1L, length(formula)[2L] %in% 1L:2L)
  ## try to handle dots in formula
  has_dot <- function(formula) {
    inherits(try(stats::terms(formula), silent = TRUE), "try-error")
  }
  if (has_dot(formula)) {
    f1 <- formula(formula, rhs = 1L)
    f2 <- formula(formula, lhs = 0L, rhs = 2L)
    if (!has_dot(f1) && has_dot(f2)) {
      formula <- Formula::as.Formula(
        f1,
        stats::update(formula(formula, lhs = 0L, rhs = 1L), f2)
      )
    }
  }
  ## call model.frame()
  mf$formula <- formula
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  ## extract response, terms, model matrices
  Y <- stats::model.response(mf, "numeric")
  mtX <- stats::terms(formula, data = data, rhs = 1)
  X <- stats::model.matrix(mtX, mf, contrasts)
  if (length(formula)[2] < 2L) {
    mtZ <- NULL
    Z <- NULL
  } else {
    mtZ <- stats::delete.response(stats::terms(formula, data = data, rhs = 2))
    Z <- stats::model.matrix(mtZ, mf, contrasts)
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

  # the column name "y" is reserved for the outcome in the data.frame
  # passed to the gmm fit; a variable with the same name would be silently
  # renamed and lookups by name would retrieve the outcome instead
  if ("y" %in% c(xnames, znames)) {
    stop(
      "Please rename the variable \"y\" in your model; ",
      "that name is reserved for internal use."
    )
  }

  tsri_env <- new.env(parent = emptyenv())
  tsri_env$anycovs <- FALSE
  if (!identical(covariatenames, character(0))) {
    tsri_env$anycovs <- TRUE
    covariates <- X[, covariatenames]
  }

  tsri_env$xnames <- xnames[!(xnames %in% covariatenames)]
  tsri_env$znames <- znames[!(znames %in% covariatenames)]
  tsri_env$covariatenames <- covariatenames
  tsri_env$ncovs <- length(covariatenames)

  # check for only 1 exposure
  if (length(tsri_env$xnames) != 1) {
    stop("Only 1 exposure variable is allowed.")
  }

  link <- match.arg(link, c("identity", "logadd", "logmult", "logit"))
  tsri_env$link <- link

  # check y binary
  if (link == "logit" && !all(Y %in% 0:1)) {
    stop(
      "With the logit link, the outcome must be binary, i.e. take values 0 or 1."
    )
  }

  # initial values
  if (is.null(t0)) {
    # select columns by name, in the order the moment functions use
    # (instruments then covariates), so that the starting values are
    # correct regardless of the order of terms in the formula
    Zstage1 <- Z[,
      c("(Intercept)", tsri_env$znames, tsri_env$covariatenames),
      drop = FALSE
    ]
    Xexposure <- X[, tsri_env$xnames]
    stage1 <- stats::lm(Xexposure ~ -1 + Zstage1)
    t0 <- stats::coef(stage1)
    res <- stats::residuals(stage1)
    if (tsri_env$anycovs) {
      res <- cbind(res, covariates)
    }
    if (link == "identity") {
      stage2 <- stats::lm(Y ~ Xexposure + res)
    } else if (link == "logadd") {
      stage2 <- stats::glm(
        Y ~ Xexposure + res,
        family = stats::poisson(link = "log")
      )
    } else if (link == "logmult") {
      Ystar <- Y
      Ystar[Y == 0] <- 0.001
      stage2 <- stats::glm(
        Ystar ~ Xexposure + res,
        family = stats::Gamma(link = "log"),
        control = list(maxit = 1E5)
      )
    } else if (link == "logit") {
      stage2 <- stats::glm(
        Y ~ Xexposure + res,
        family = stats::binomial(link = "logit")
      )
    }
    t0 <- c(t0, stats::coef(stage2))

    if (!tsri_env$anycovs) {
      xindex <- length(t0) - 1
    } else {
      xindex <- length(t0) - 1 - tsri_env$ncovs
    }
    names(t0)[xindex] <- tsri_env$xnames
  }

  Xtopass <- as.data.frame(X[, tsri_env$xnames])
  colnames(Xtopass) <- tsri_env$xnames

  # select columns by name so that the instrument and covariate data keep
  # their correct labels regardless of the order of terms in the formula
  zcolorder <- c(tsri_env$znames, tsri_env$covariatenames)
  Ztopass <- as.data.frame(Z[, zcolorder, drop = FALSE])
  colnames(Ztopass) <- zcolorder

  # the first stage residuals do not depend on theta, so compute them once
  # here rather than on every evaluation of the gmm moment functions below
  stage1fit <- stats::lm(Xtopass[[1]] ~ as.matrix(Ztopass))
  resfixed <- cbind(Xtopass[[1]], as.matrix(stats::residuals(stage1fit)))
  if (tsri_env$anycovs) {
    resfixed <- cbind(resfixed, Ztopass[, tsri_env$covariatenames])
  }
  tsri_env$res <- resfixed

  # functions for tsri fit
  tsri_gmm <- function(x, y, z, t0, link) {
    x <- as.matrix(x)

    if (!identical(tsri_env$covariatenames, character(0))) {
      x <- x[, !(colnames(x) %in% tsri_env$covariatenames), drop = FALSE]
    }

    dat <- data.frame(y, x, z)

    if (is.null(t0)) {
      t0 <- rep(0, ncol(x) + 1)
    }

    # gmm fit (tsriMoments reads the link function from tsri_env)
    if (link == "logmult") {
      fit <- gmm::gmm(
        tsriMoments,
        x = dat,
        t0 = t0,
        vcov = "iid",
        itermax = 1E7
      )
    } else {
      fit <- gmm::gmm(tsriMoments, x = dat, t0 = t0, vcov = "iid")
    }

    if (fit$algoInfo$convergence != 0) {
      warning(
        "The GMM fit has not converged, perhaps try different initial parameter values"
      )
    }

    estci <- cbind(gmm::coef.gmm(fit), gmm::confint.gmm(fit)$test)
    colnames(estci)[1] <- "Estimate"

    reslist <- list(fit = fit, estci = estci, link = link)
    return(reslist)
  }

  tsriMoments <- function(theta, x) {
    # extract variables from x
    Y <- as.matrix(x[, "y"])
    X <- x[, tsri_env$xnames]
    Z <- as.matrix(x[, tsri_env$znames])
    if (tsri_env$anycovs) {
      covariates <- x[, tsri_env$covariatenames]
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

    # first stage linear predictor and residual
    linearpredictor <- Zwithcons %*% as.matrix(thetastage1)
    stage1express <- as.vector(X - linearpredictor)

    # second stage linear predictor
    eta2 <- theta[stage2start] +
      thetacausal * X +
      thetares * (X - as.matrix(linearpredictor))
    if (tsri_env$anycovs) {
      thetacov <- thetastage2[4:length(thetastage2)]
      eta2 <- eta2 + as.matrix(covariates) %*% as.matrix(thetacov)
    }
    eta2 <- as.vector(eta2)

    # second stage residual on the scale of the specified link function
    Yvec <- as.vector(Y)
    stage2express <- switch(
      tsri_env$link,
      identity = Yvec - eta2,
      logadd = Yvec - exp(eta2),
      logmult = Yvec * exp(-1 * eta2) - 1,
      logit = Yvec - stats::plogis(eta2)
    )

    # first stage residuals (precomputed in tsri() because they do not
    # depend on theta)
    res <- as.matrix(tsri_env$res)

    # moments: the first stage residual multiplied by a constant and each
    # instrument (and covariate), then the second stage residual multiplied
    # by a constant, the exposure, the first stage residuals, and any
    # covariates
    unname(cbind(
      stage1express * Zwithcons,
      stage2express * cbind(1, res)
    ))
  }

  # gmm fit
  output <- tsri_gmm(
    x = Xtopass,
    y = Y,
    z = Ztopass,
    t0 = t0,
    link = link
  )
  class(output) <- append("tsri", class(output))
  output
}

#' Summarizing TSRI Fits
#'
#' S3 print and summary methods for objects of
#' class `"tsri"` and print method for objects of
#' class `"summary.tsri"`.
#'
#' @param object an object of class `"tsri"`.
#' @param x an object of class `"summary.tsri"`.
#' @param digits the number of significant digits to use when printing.
#' @param ... further arguments passed to or from other methods.
#'
#' @return `summary.tsri()` returns an object of class `"summary.tsri"`. A list with the following elements:
#'
#' \item{smry}{An object from a call to [gmm::summary.gmm()]}
#' \item{object}{The object of class `tsri` passed to the function.}
#'
#' @examples
#' # See the examples at the bottom of help('tsri')
#' @export
summary.tsri <- function(object, ...) {
  smry <- gmm::summary.gmm(object$fit)

  res <- list(smry = smry, object = object)

  class(res) <- append(class(res), "summary.tsri")
  return(res)
}

#' @rdname summary.tsri
#' @export
print.tsri <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\nSecond stage model link function:", x$link, "\n\n")
  gmm::print.gmm(x$fit)

  cat("\nEstimates with 95% CI limits:\n")
  print(x$estci, digits = digits, ...)

  # the second stage rows cannot be located if the coefficients are
  # unnamed, e.g. from a user specified unnamed t0
  rowstart <- which(rownames(x$estci) == "(Intercept)")
  rowstop <- nrow(x$estci)
  if (
    length(rowstart) == 1 && x$link %in% c("logadd", "logmult", "logit")
  ) {
    parname <- "Causal odds ratio"
    if (x$link %in% c("logadd", "logmult")) {
      parname <- "Causal risk ratio"
    }
    cat("\n", parname, " with 95% CI limits:\n", sep = "")
    print(exp(x$estci[rowstart:rowstop, ]), digits = digits, ...)
  }

  cat("\n")
  invisible(x)
}

#' @rdname summary.tsri
#' @export
print.summary.tsri <- function(
  x,
  digits = max(3, getOption("digits") - 3),
  ...
) {
  cat("\nGMM fit summary:\n")
  gmm::print.summary.gmm(x$smry)

  cat("\nEstimates with 95% CI limits:\n")
  print(x$object$estci, digits = digits, ...)

  # the second stage rows cannot be located if the coefficients are
  # unnamed, e.g. from a user specified unnamed t0
  rowstart <- which(rownames(x$object$estci) == "(Intercept)")
  rowstop <- nrow(x$object$estci)
  if (
    length(rowstart) == 1 &&
      x$object$link %in% c("logadd", "logmult", "logit")
  ) {
    parname <- "Causal odds ratio"
    if (x$object$link %in% c("logadd", "logmult")) {
      parname <- "Causal risk ratio"
    }
    cat("\n", parname, " with 95% CI limits:\n", sep = "")
    print(exp(x$object$estci[rowstart:rowstop, ]), digits = digits, ...)
  }

  cat("\n")
  invisible(x)
}
