#' Two-stage predictor substitution (TSPS) estimators
#'
#' Terza et al. (2008) give an excellent description of TSPS estimators.
#' They proceed by fitting a first stage model of the
#' exposure regressed upon the instruments (and possibly any measured
#' confounders). From this the predicted values of the exposure are obtained.
#' A second stage model is then fitted of the outcome regressed upon
#' the predicted values of the exposure (and possibly measured confounders).
#'
#' `tsps()` performs GMM estimation to ensure appropriate standard errors
#' on its estimates similar to the approach described in Clarke et al. (2015).
#'
#' @inheritParams msmm
#' @param link character; one of `"identity"` (the default), `"logadd"`, `"logmult"`, `"logit"`.
#'  This is the link function for the second stage model. `"identity"` corresponds to linear
#'  regression; `"logadd"` is log-additive and corresponds to Poisson / log-binomial regression;
#'  `"logmult"` is log-multiplicative and corresponds to gamma regression;
#'  `"logit"` corresponds to logistic regression.
#' @return An object of class `"tsps"` with the following elements
#' \describe{
#' \item{fit}{the fitted object of class `"gmm"` from the call to [gmm::gmm()].}
#' \item{estci}{a matrix of the estimates with their corresponding confidence interval limits.}
#' \item{link}{a character vector containing the specified link function.}
#' }
#' @references
#' Burgess S, CRP CHD Genetics Collaboration.
#' Identifying the odds ratio estimated by a
#' two-stage instrumental variable analysis
#' with a logistic regression model.
#' Statistics in Medicine, 2013, 32, 27, 4726-4747.
#' \doi{10.1002/sim.5871}
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
#' @examples
#' # Two-stage predictor substitution estimator
#' # with second stage logistic regression
#' set.seed(9)
#' n            <- 1000
#' psi0         <- 0.5
#' Z            <- rbinom(n, 1, 0.5)
#' X            <- rbinom(n, 1, 0.7*Z + 0.2*(1 - Z))
#' m0           <- plogis(1 + 0.8*X - 0.39*Z)
#' Y            <- rbinom(n, 1, plogis(psi0*X + log(m0/(1 - m0))))
#' dat          <- data.frame(Z, X, Y)
#' tspslogitfit <- tsps(Y ~ X | Z , data = dat, link = "logit")
#' summary(tspslogitfit)
#' @export
tsps <- function(
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

  tsps_env <- new.env(parent = emptyenv())
  tsps_env$anycovs <- FALSE
  if (!identical(covariatenames, character(0))) {
    tsps_env$anycovs <- TRUE
    covariates <- X[, covariatenames]
  }

  tsps_env$xnames <- xnames[!(xnames %in% covariatenames)]
  tsps_env$znames <- znames[!(znames %in% covariatenames)]
  tsps_env$covariatenames <- covariatenames

  # check for only 1 exposure
  if (length(tsps_env$xnames) != 1) {
    stop("Only 1 exposure variable is allowed.")
  }

  link <- match.arg(link, c("identity", "logadd", "logmult", "logit"))
  tsps_env$link <- link

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
      c("(Intercept)", tsps_env$znames, tsps_env$covariatenames),
      drop = FALSE
    ]
    stage1 <- stats::lm(X[, tsps_env$xnames] ~ -1 + Zstage1)
    t0 <- stats::coef(stage1)
    xhat <- stats::fitted.values(stage1)
    if (tsps_env$anycovs) {
      xhat <- cbind(xhat, covariates)
    }
    if (link == "identity") {
      stage2 <- stats::lm(Y ~ xhat)
    } else if (link == "logadd") {
      stage2 <- stats::glm(Y ~ xhat, family = stats::poisson(link = "log"))
    } else if (link == "logmult") {
      Ystar <- Y
      Ystar[Y == 0] <- 0.001
      stage2 <- stats::glm(
        Ystar ~ xhat,
        family = stats::Gamma(link = "log"),
        control = list(maxit = 1E5)
      )
    } else if (link == "logit") {
      stage2 <- stats::glm(Y ~ xhat, family = stats::binomial(link = "logit"))
    }
    t0 <- c(t0, stats::coef(stage2))
  }

  Xtopass <- as.data.frame(X[, tsps_env$xnames])
  colnames(Xtopass) <- tsps_env$xnames

  # select columns by name so that the instrument and covariate data keep
  # their correct labels regardless of the order of terms in the formula
  zcolorder <- c(tsps_env$znames, tsps_env$covariatenames)
  Ztopass <- as.data.frame(Z[, zcolorder, drop = FALSE])
  colnames(Ztopass) <- zcolorder

  # the first stage predicted values do not depend on theta, so compute
  # them once here rather than on every evaluation of the gmm moment
  # functions below
  stage1fit <- stats::lm(Xtopass[[1]] ~ as.matrix(Ztopass))
  xhatfixed <- as.matrix(stats::fitted.values(stage1fit))
  if (tsps_env$anycovs) {
    xhatfixed <- cbind(xhatfixed, Ztopass[, tsps_env$covariatenames])
  }
  tsps_env$xhat <- xhatfixed

  # functions for the tsps fit
  tsps_gmm <- function(x, y, z, t0, link) {
    x <- as.matrix(x)

    if (!identical(tsps_env$covariatenames, character(0))) {
      x <- x[, !(colnames(x) %in% tsps_env$covariatenames), drop = FALSE]
    }

    dat <- data.frame(y, x, z)

    if (is.null(t0)) {
      t0 <- rep(0, ncol(x) + 1)
    }

    # gmm fit (tspsMoments reads the link function from tsps_env)
    if (link == "logmult") {
      fit <- gmm::gmm(
        tspsMoments,
        x = dat,
        t0 = t0,
        vcov = "iid",
        itermax = 1E7
      )
    } else {
      fit <- gmm::gmm(tspsMoments, x = dat, t0 = t0, vcov = "iid")
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

  tspsMoments <- function(theta, x) {
    # extract variables from x
    Y <- as.matrix(x[, "y"])
    X <- x[, tsps_env$xnames]
    Z <- as.matrix(x[, tsps_env$znames])
    if (tsps_env$anycovs) {
      covariates <- x[, tsps_env$covariatenames]
      Z <- as.matrix(cbind(Z, covariates))
    }
    Zwithcons <- as.matrix(cbind(rep(1, nrow(x)), Z))
    stage1end <- ncol(Zwithcons)
    thetastage1 <- theta[1:stage1end]
    stage2start <- stage1end + 1
    thetaend <- length(theta)

    # first stage linear predictor and residual
    linearpredictor <- Zwithcons %*% as.matrix(thetastage1)
    stage1express <- as.vector(X - linearpredictor)

    # second stage linear predictor
    if (tsps_env$anycovs) {
      stage2linpred <- as.matrix(cbind(linearpredictor, covariates))
    } else {
      stage2linpred <- linearpredictor
    }
    eta2 <- as.vector(
      theta[stage2start] +
        stage2linpred %*% as.matrix(theta[(stage2start + 1):thetaend])
    )

    # second stage residual on the scale of the specified link function
    Yvec <- as.vector(Y)
    stage2express <- switch(
      tsps_env$link,
      identity = Yvec - eta2,
      logadd = Yvec - exp(eta2),
      logmult = Yvec * exp(-1 * eta2) - 1,
      logit = Yvec - stats::plogis(eta2)
    )

    # first stage predicted values (precomputed in tsps() because they do
    # not depend on theta)
    xhat <- as.matrix(tsps_env$xhat)

    # moments: the first stage residual multiplied by a constant and each
    # instrument (and covariate), then the second stage residual multiplied
    # by a constant and the first stage predicted values (and covariates)
    unname(cbind(
      stage1express * Zwithcons,
      stage2express * cbind(1, xhat)
    ))
  }

  # gmm fit
  output <- tsps_gmm(
    x = Xtopass,
    y = Y,
    z = Ztopass,
    t0 = t0,
    link = link
  )
  class(output) <- append("tsps", class(output))
  output
}

#' Summarizing TSPS Fits
#'
#' S3 print and summary methods for objects of
#' class `"tsps"` and print method for objects of
#' class `"summary.tsps"`.
#'
#' @param object an object of class `"tsps"`.
#' @param x an object of class `"summary.tsps"`.
#' @param digits the number of significant digits to use when printing.
#' @param ... further arguments passed to or from other methods.
#'
#' @return `summary.tsps()` returns an object of class `"summary.tsps"`. A list with the following elements:
#'
#' \item{smry}{An object from a call to [gmm::summary.gmm()]}
#' \item{object}{The object of class `tsps` passed to the function.}
#'
#' @examples
#' # See the examples at the bottom of help('tsps')
#' @export
summary.tsps <- function(object, ...) {
  smry <- gmm::summary.gmm(object$fit)

  res <- list(smry = smry, object = object)

  class(res) <- append(class(res), "summary.tsps")
  return(res)
}

#' @rdname summary.tsps
#' @export
print.tsps <- function(x, digits = max(3, getOption("digits") - 3), ...) {
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

#' @rdname summary.tsps
#' @export
print.summary.tsps <- function(
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
