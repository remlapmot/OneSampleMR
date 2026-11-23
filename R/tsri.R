#' Two-Stage Residual Inclusion (TSRI) Estimators
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
#' @inheritParams tsps
#' @inheritParams msmm
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

  # the call and model formula, returned for the formula() and print methods
  cl <- match.call()
  fml <- if (missing(instruments)) {
    formula
  } else {
    Formula::as.Formula(formula, instruments)
  }

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

  exposurenames <- setdiff(xnames, covariatenames)
  instrumentnames <- setdiff(znames, covariatenames)

  # check for only 1 exposure
  if (length(exposurenames) != 1) {
    stop("Only 1 exposure variable is allowed.")
  }

  link <- match.arg(link, c("identity", "logadd", "logmult", "logit"))

  # check y binary
  if (link == "logit" && !all(Y %in% 0:1)) {
    stop(
      "With the logit link, the outcome must be binary, i.e. take values 0 or 1."
    )
  }

  # model matrices, with the first stage regressors in the order of the
  # first stage parameters (intercept, instruments, then covariates)
  xvec <- X[, exposurenames]
  Zstage1 <- Z[,
    c("(Intercept)", instrumentnames, covariatenames),
    drop = FALSE
  ]
  covs <- Z[, covariatenames, drop = FALSE]
  k1 <- ncol(Zstage1)

  # the first stage does not depend on the second stage parameters, so fit
  # it once here; its residuals are included in the second stage
  stage1 <- stats::.lm.fit(Zstage1, xvec)
  R2 <- cbind(1, xvec, stage1$residuals, covs)

  # initial values: the sequential two-stage estimates
  if (is.null(t0)) {
    stage2 <- switch(
      link,
      identity = stats::.lm.fit(R2, Y)$coefficients,
      logadd = stats::glm.fit(
        R2,
        Y,
        family = stats::poisson(link = "log")
      )$coefficients,
      # solve the second stage estimating equations exactly, starting from
      # the (quasi-)Poisson regression estimates
      logmult = logmult_coef(
        Y,
        R2,
        stats::glm.fit(
          R2,
          Y,
          family = stats::quasipoisson(link = "log")
        )$coefficients
      ),
      logit = stats::glm.fit(
        R2,
        Y,
        family = stats::binomial(link = "logit")
      )$coefficients
    )
    t0 <- c(stage1$coefficients, stage2)
  }

  # label the parameters, also when the user has specified t0
  parnames <- c(
    paste0(
      "stage1:",
      c("(Intercept)", instrumentnames, covariatenames)
    ),
    "(Intercept)",
    exposurenames,
    "stage1:residuals",
    covariatenames
  )
  if (length(t0) != length(parnames)) {
    stop("t0 must have length ", length(parnames), ".")
  }
  names(t0) <- parnames

  # moment conditions: the first stage residual multiplied by the first
  # stage regressors, then the second stage residual, on the scale of the
  # link function, multiplied by the second stage regressors; the data are
  # taken from the enclosing environment rather than from x
  tsriMoments <- function(theta, x) {
    linearpredictor <- drop(Zstage1 %*% theta[1:k1])
    stage1express <- xvec - linearpredictor
    theta2 <- theta[-(1:k1)]
    eta2 <- theta2[1] + theta2[2] * xvec + theta2[3] * stage1express
    if (ncol(covs) > 0) {
      eta2 <- eta2 + drop(covs %*% theta2[-(1:3)])
    }
    stage2express <- switch(
      link,
      identity = Y - eta2,
      logadd = Y - exp(eta2),
      logmult = Y * exp(-eta2) - 1,
      logit = Y - stats::plogis(eta2)
    )
    unname(cbind(stage1express * Zstage1, stage2express * R2))
  }

  # gmm fit
  fit <- gmm::gmm(tsriMoments, x = cbind(Y), t0 = t0, vcov = "iid")

  # the model is exactly identified, so the minimum of the gmm objective
  # function is 0; the optimizer can report non-convergence when it has
  # reached this minimum but cannot further shrink its simplex
  if (fit$algoInfo$convergence != 0 && fit$objective > 1e-10) {
    warning(
      "The GMM fit has not converged, perhaps try different initial parameter values"
    )
  }

  estci <- cbind(gmm::coef.gmm(fit), gmm::confint.gmm(fit)$test)
  colnames(estci)[1] <- "Estimate"

  output <- list(
    fit = fit,
    estci = estci,
    link = link,
    call = cl,
    formula = fml
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
#' @param signif.stars logical; if `TRUE`, p-values are additionally
#' encoded visually as significance stars.
#' @return `summary.tsri()` returns an object of class `"summary.tsri"`. A list with the following elements:
#'
#' \item{call}{The call of the fitted model.}
#' \item{link}{The second stage link function.}
#' \item{coefficients}{A matrix of the parameter estimates, their standard
#' errors, Wald z statistics, and p-values.}
#' \item{smry}{An object from a call to [gmm::summary.gmm()].}
#' \item{object}{The object of class `tsri` passed to the function.}
#'
#' @examples
#' # See the examples at the bottom of help('tsri')
#' @export
summary.tsri <- function(object, ...) {
  res <- list(
    call = object$call,
    link = object$link,
    coefficients = coef_table(stats::coef(object), stats::vcov(object)),
    smry = gmm::summary.gmm(object$fit),
    object = object
  )
  class(res) <- "summary.tsri"
  res
}

#' @rdname summary.tsri
#' @export
print.tsri <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\nSecond stage model link function:", x$link, "\n\n")
  gmm::print.gmm(x$fit)

  cat("\nEstimates with 95% CI limits:\n")
  print(x$estci, digits = digits, ...)

  print_causal_ratio(x$estci, x$link, digits = digits, ...)

  cat("\n")
  invisible(x)
}

#' @rdname summary.tsri
#' @export
print.summary.tsri <- function(
  x,
  digits = max(3, getOption("digits") - 3),
  signif.stars = getOption("show.signif.stars"),
  ...
) {
  print_call(x$call)
  cat("\nSecond stage model link function:", x$link, "\n")
  cat("\nCoefficients:\n")
  stats::printCoefmat(
    x$coefficients,
    digits = digits,
    signif.stars = signif.stars,
    ...
  )
  print_causal_ratio(x$object$estci, x$link, digits = digits)
  cat("\n")
  invisible(x)
}
