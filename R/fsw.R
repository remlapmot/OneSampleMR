#' Conditional F-statistic of Sanderson and Windmeijer (2016)
#'
#' `fsw` calculates the conditional F-statistic of
#' Sanderson and Windmeijer (2016) for each endogenous variable
#' in the model.
#' @param object An object of class `"ivreg"` / `"iv_robust"` /
#' `"fixest"` containing the results of an IV model fitted by
#' [ivreg::ivreg()] / [AER::ivreg()] / [estimatr::iv_robust()] /
#' [fixest::feols()] for which to calculate the conditional
#' F-statistics for each endogenous variable.
#' @return An object of class `"fsw"` with the following elements:
#' \describe{
#' \item{fswres}{matrix with columns for the conditional *F*-statistics,
#' degrees of freedom, residual degrees of freedom, and p-value.
#' 1 row per endogenous variable.}
#' \item{namesendog}{a character vector of the variable names of the endogenous
#' variables.}
#' \item{nendog}{the number of endogenous variables.}
#' \item{n}{the sample size used for the fitted model.}
#' }
#' @examples
#' require(ivreg)
#' require(AER)
#' require(estimatr)
#' require(fixest)
#' set.seed(12345)
#' n   <- 4000
#' z1  <- rnorm(n)
#' z2  <- rnorm(n)
#' w1  <- rnorm(n)
#' w2  <- rnorm(n)
#' u   <- rnorm(n)
#' x1  <- z1 + z2 + 0.2*u + 0.1*w1 + rnorm(n)
#' x2  <- z1 + 0.94*z2 - 0.3*u + 0.1*w2 + rnorm(n)
#' y   <- x1 + x2 + w1 + w2 + u
#' dat <- data.frame(w1, w2, x1, x2, y, z1, z2)
#' mod1 <- ivreg::ivreg(y ~ x1 + x2 + w1 + w2 | z1 + z2 + w1 + w2, data = dat)
#' mod2 <- AER::ivreg(y ~ x1 + x2 + w1 + w2 | z1 + z2 + w1 + w2, data = dat)
#' mod3 <- estimatr::iv_robust(y ~ x1 + x2 + w1 + w2 | z1 + z2 + w1 + w2,
#'         data = dat, se_type = "classical")
#' mod4 <- fixest::feols(y ~ w1 + w2 | x1 + x2 ~ z1 + z2, data = dat)
#' fsw(mod1)
#' fsw(mod2)
#' fsw(mod3)
#' fsw(mod4)
#'
#' @references
#' Sanderson E and Windmeijer F. A weak instrument *F*-test in linear
#' IV models with multiple endogenous variables. Journal of Econometrics,
#' 2016, 190, 2, 212-221, \doi{10.1016/j.jeconom.2015.06.004}.
#' @export
fsw <- function(object) UseMethod("fsw", object)

#' S3 fsw method for object returned by [ivreg::ivreg()] or [AER::ivreg()]
#' @rdname fsw
#' @export
fsw.ivreg <- function(object) {
  # Error message if model option not set to TRUE:
  if (is.null(object$model)) {
    stop("Please re-run your ivreg() model with the option model=TRUE")
  }

  # Check if object contains "endogenous" component (returned by ivreg::ivreg) or not (returned by AER::ivreg)
  if (!is.null(object$endogenous)) {
    ### Object is from ivreg::ivreg

    # Names of endogenous variables (object$endogenous holds their model
    # matrix indices by name, so this is robust to the term order in the
    # formula, e.g. covariates listed before the exposures):
    namesendog <- names(object$endogenous)

    # Error for less than 2 endogenous variables:
    if (length(namesendog) < 2) {
      stop("The number of exposures must be 2 or more.")
    }

    # Error for factor variables among endogenous variables
    # (checked via the term labels because a factor variable has different
    # column names in the model matrix than in the model frame):
    namesendogterms <- setdiff(
      labels(object$terms$regressors),
      labels(object$terms$instruments)
    )
    fsw_factor_check(object$model, namesendogterms, "ivreg()")

    # Names of exogenous explanatory variables:
    namesregressors <- labels(object$terms$regressors)
    namesexog <- namesregressors[!(namesregressors %in% namesendog)]

    # Names of excluded instruments:
    namesinstruments <- names(object$instruments)

    fitter <- function(modelfor, dat) ivreg::ivreg(modelfor, data = dat)
  } else {
    ### Object is from AER::ivreg

    if (!requireNamespace("AER", quietly = TRUE)) {
      stop(
        "Package \"AER\" must be installed to use this function.",
        call. = FALSE
      )
    }

    # Explanatory variables (both endogenous and exogenous):
    xvars <- attr(
      stats::terms(Formula::Formula(object$formula), rhs = 1),
      "term.labels"
    )
    # Instruments (both excluded and included - i.e. includes exogenous expl. vars):
    zvars <- attr(
      stats::terms(Formula::Formula(object$formula), rhs = 2),
      "term.labels"
    )

    # Names of endogenous variables:
    namesendog <- xvars[!xvars %in% zvars]

    # Error for less than 2 endogenous variables:
    if (length(namesendog) < 2) {
      stop("The number of exposures must be 2 or more.")
    }

    # Error for factor variables among endogenous variables:
    fsw_factor_check(object$model, namesendog, "ivreg()")

    # Names of exogenous explanatory variables:
    namesexog <- xvars[xvars %in% zvars]

    # Names of excluded instruments:
    namesinstruments <- zvars[!zvars %in% xvars]

    fitter <- function(modelfor, dat) AER::ivreg(modelfor, data = dat)
  }

  fsw_common(
    namesendog = namesendog,
    namesexog = namesexog,
    namesinstruments = namesinstruments,
    dat = object$model,
    n = object$n,
    fitter = fitter,
    formulafun = fsw_condformula,
    residfun = function(condmod, dat, endogoutcome) condmod$residuals,
    condmoderrmsg = fsw_condmoderrmsg(
      "ivreg",
      "ivreg(y ~ log(x1) + x2 | z1 + z2 + z3)",
      "ivreg(y ~ logx1 + x2 | z1 + z2 + z3)"
    )
  )
}


#' S3 fsw method for object returned by [estimatr::iv_robust()]
#' @rdname fsw
#' @export
fsw.iv_robust <- function(object) {
  if (!requireNamespace("estimatr", quietly = TRUE)) {
    stop(
      "Package \"estimatr\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Explanatory variables (both endogenous and exogenous):
  xvars <- attr(
    stats::terms(Formula::Formula(object$formula), rhs = 1),
    "term.labels"
  )
  # Instruments (both excluded and included - i.e. includes exogenous expl. vars):
  zvars <- attr(
    stats::terms(Formula::Formula(object$formula), rhs = 2),
    "term.labels"
  )

  # Names of endogenous variables:
  namesendog <- xvars[!xvars %in% zvars]

  # Error for less than 2 endogenous variables:
  if (length(namesendog) < 2) {
    stop("The number of exposures must be 2 or more.")
  }

  dat <- get_data(object)

  # Error for factor variables among endogenous variables:
  fsw_factor_check(dat, namesendog, "iv_robust()")

  fsw_common(
    namesendog = namesendog,
    namesexog = xvars[xvars %in% zvars],
    namesinstruments = zvars[!zvars %in% xvars],
    dat = dat,
    n = object$nobs,
    fitter = function(modelfor, dat) {
      estimatr::iv_robust(modelfor, data = dat, se_type = "classical")
    },
    formulafun = fsw_condformula,
    residfun = function(condmod, dat, endogoutcome) {
      # iv_robust objects do not contain residuals:
      dat[[endogoutcome]] - condmod$fitted.values
    },
    condmoderrmsg = fsw_condmoderrmsg(
      "iv_robust",
      "iv_robust(y ~ log(x1) + x2 | z1 + z2 + z3)",
      "iv_robust(y ~ logx1 + x2 | z1 + z2 + z3)"
    )
  )
}


#' S3 fsw method for object returned by [fixest::feols()]
#' @rdname fsw
#' @export
fsw.fixest <- function(object) {
  if (!requireNamespace("fixest", quietly = TRUE)) {
    stop(
      "Package \"fixest\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Names of endogenous variables:
  namesendog <- object$iv_endo_names

  # Error for less than 2 endogenous variables:
  if (length(namesendog) < 2) {
    stop("The number of exposures must be 2 or more.")
  }

  dat <- get_data(object)

  # Error for factor variables among endogenous variables
  # (checked via the model formula because a factor variable has different
  # column names in the model matrix than in the data):
  namesendogterms <- intersect(all.vars(object$iv_endo_fml), colnames(dat))
  fsw_factor_check(dat, namesendogterms, "feols()")

  fsw_common(
    namesendog = namesendog,
    namesexog = attr(
      stats::terms(Formula::Formula(object$fml), rhs = 1),
      "term.labels"
    ),
    namesinstruments = object$iv_inst_names,
    dat = dat,
    n = object$nobs,
    fitter = function(modelfor, dat) fixest::feols(modelfor, data = dat),
    formulafun = fsw_condformula_fixest,
    residfun = function(condmod, dat, endogoutcome) condmod$residuals,
    condmoderrmsg = fsw_condmoderrmsg(
      "fixest",
      "feols(y ~ 1 | log(x1) + x2 ~ z1 + z2 + z3)",
      "feols(y ~ 1 | logx1 + x2 ~ z1 + z2 + z3)"
    )
  )
}


#' Compute the conditional F-statistics and assemble the fsw object.
#' This is the shared workhorse for all fsw methods; the method specific
#' behaviour is passed in via the fitter, formulafun, and residfun functions.
#' @param namesendog character vector of the endogenous variable names
#' @param namesexog character vector of the exogenous explanatory variable names
#' @param namesinstruments character vector of the excluded instrument names
#' @param dat the data used to fit the model (complete cases)
#' @param n the sample size used for the fitted model
#' @param fitter function(formula, dat) fitting the conditional IV model
#' @param formulafun function building the conditional model equation string
#' @param residfun function(condmod, dat, endogoutcome) returning the
#' residuals of the conditional model
#' @param condmoderrmsg error message if the conditional model fit fails
#' @noRd
fsw_common <- function(
  namesendog,
  namesexog,
  namesinstruments,
  dat,
  n,
  fitter,
  formulafun,
  residfun,
  condmoderrmsg
) {
  nendog <- length(namesendog)
  nexogenous <- length(namesexog)

  # Create plus-separated string of excluded instruments (e.g. "z1 + z2 + ..."):
  instrplus <- paste(namesinstruments, collapse = " + ")

  # Create plus-separated string of exogenous explanatory variables (if any):
  exogplus <- NULL
  if (nexogenous > 0) {
    exogplus <- paste(namesexog, collapse = " + ")
  }

  # Create equations of the unrestricted and restricted models compared in the Wald test:
  equations <- wald_equations(nexogenous, instrplus, exogplus)

  # Obtain conditional F statistic for each endogenous explanatory variable:
  fswres <- sapply(namesendog, function(endogoutcome) {
    # Names of other endogenous explanatory variables:
    endogothers <- namesendog[!(namesendog %in% endogoutcome)]

    # Create condit. model equation (endog. expl. var regressed against
    # other endog. expl. vars and any exog. vars, instrumented with excl.
    # instruments) and convert to formula:
    modelstr <- formulafun(
      endogoutcome,
      endogothers,
      nexogenous,
      instrplus,
      exogplus
    )
    modelfor <- stats::as.formula(modelstr)

    # Estimate condit. model:
    condmod <- try(fitter(modelfor, dat), silent = TRUE)

    # Error message if condit. model estimation fails:
    if (inherits(condmod, "try-error")) {
      stop(condmoderrmsg)
    }

    # Residuals of the condit. model (used by the lm() fits below through
    # the environment of the Wald test equation formulas):
    condres <- residfun(condmod, dat, endogoutcome)

    # Unrestricted model for Wald test:
    # Estimate regression of condit. residuals against instruments (and any exogenous regressors):
    resmod <- stats::lm(
      stats::as.formula(equations$unrestricted),
      data = dat
    )

    # Restricted model for Wald test:
    # Estimate regression of condit. residuals against intercept (and any exogenous regressors):
    resbase <- stats::lm(
      stats::as.formula(equations$restricted),
      data = dat
    )

    # Compute conditional F-Statistic using Wald test for restricted vs unrestricted model:
    fsw_wald_test(resbase, resmod, nendog)
  })

  # Prepare results matrix:
  fswres <- t(fswres)
  rownames(fswres) <- namesendog
  colnames(fswres) <- c("F value", "d.f.", "Residual d.f.", "Pr(>F)")

  # Define and return output:
  output <- list(
    fswres = fswres,
    namesendog = namesendog,
    nendog = nendog,
    n = n
  )
  class(output) <- append("fsw", class(output))
  output
}


#' Stop with an informative error if any endogenous variable is a factor
#' (is.factor() is TRUE for ordered factors as well)
#' @param dat the data used to fit the model
#' @param namesendogterms term level names of the endogenous variables
#' @param refitname name of the fitting function for the error message
#' @noRd
fsw_factor_check <- function(dat, namesendogterms, refitname) {
  if (any(vapply(dat[namesendogterms], is.factor, logical(1)))) {
    stop(paste(
      "One or more of your exposure variables is a factor.",
      "Please convert to numeric with say as.numeric(),",
      sprintf("refit your %s model, and rerun fsw().", refitname)
    ))
  }
}


#' Create the conditional model equation string for two-part IV formulas
#' as used by ivreg::ivreg(), AER::ivreg(), and estimatr::iv_robust()
#' @param endogoutcome the endogenous variable used as the outcome
#' @param endogothers the other endogenous variables
#' @param nexogenous number of exogenous explanatory variables
#' @param instrplus plus-separated string of excluded instruments
#' @param exogplus plus-separated string of exogenous explanatory variables
#' @noRd
fsw_condformula <- function(
  endogoutcome,
  endogothers,
  nexogenous,
  instrplus,
  exogplus
) {
  if (nexogenous > 0) {
    paste(
      endogoutcome,
      "~",
      paste(endogothers, collapse = " + "),
      "+",
      exogplus,
      "|",
      instrplus,
      "+",
      exogplus
    )
  } else {
    paste(
      endogoutcome,
      "~",
      paste(endogothers, collapse = " + "),
      "|",
      instrplus
    )
  }
}


#' Create the conditional model equation string in the three-part
#' formula syntax used by fixest::feols()
#' @inheritParams fsw_condformula
#' @noRd
fsw_condformula_fixest <- function(
  endogoutcome,
  endogothers,
  nexogenous,
  instrplus,
  exogplus
) {
  if (nexogenous > 0) {
    paste(
      endogoutcome,
      "~",
      exogplus,
      "|",
      paste(endogothers, collapse = " + "),
      "~",
      instrplus
    )
  } else {
    paste(
      endogoutcome,
      "~",
      "1 |",
      paste(endogothers, collapse = " + "),
      "~",
      instrplus
    )
  }
}


#' Create the error message for a failed conditional model fit
#' @param objname class of the fitted model object for the error message
#' @param badexample example call with a transformed variable
#' @param goodexample example call with the transformed variable precomputed
#' @noRd
fsw_condmoderrmsg <- function(objname, badexample, goodexample) {
  paste(
    "The IV regression of one of the exposures",
    "on the other/s has failed.",
    "This is most likely because you have a transformation",
    "on one or more of the exposure or instrumental variables.",
    "Please create the transformed variable/s in your",
    "data.frame and refit,",
    sprintf("e.g. instead of creating your %s object from", objname),
    badexample,
    "please create dat$logx1 = log(x1) in your data.frame",
    sprintf("and fit %s.", goodexample)
  )
}


#' Retrieve the data frame used by a fitted model object (complete cases only)
#' Tries call_env (fixest), then the formula environment (estimatr).
#' Subsets to the rows actually used in the model fit to handle NA removal.
#' @param object a fitted model object
#' @noRd
get_data <- function(object) {
  data_name <- object$call$data
  # fixest stores the environment where the model was fitted
  if (!is.null(object$call_env)) {
    d <- eval(data_name, envir = object$call_env)
    # fixest$obs_selection$obsRemoved contains (negative) indices of removed rows
    if (!is.null(object$obs_selection$obsRemoved)) {
      removed <- abs(object$obs_selection$obsRemoved)
      d <- d[setdiff(seq_len(nrow(d)), removed), , drop = FALSE]
    }
    return(d)
  }
  # for estimatr and others, the formula captures the fitting environment
  d <- eval(data_name, envir = environment(object$formula))
  # iv_robust names fitted.values with the row indices actually used
  if (!is.null(names(object$fitted.values))) {
    used <- as.integer(names(object$fitted.values))
    d <- d[used, , drop = FALSE]
  }
  d
}


#' Create the equations for the Wald test:
#' unrestricted - regression of condit. residuals against instruments (and any exogenous regressors)
#' restricted - regression of condit. residuals against intercept (and any exogenous regressors)
#' @param nexogenous number of exogenous explanatory variables
#' @param instrplus plus-separated string of excluded instruments (e.g. "z1 + z2 + ...")
#' @param exogplus plus-separated string of exogenous explanatory variables
#' @noRd
wald_equations <- function(nexogenous, instrplus, exogplus) {
  if (nexogenous > 0) {
    # with exogenous explanatory variables
    unrestricted <- paste("condres", "~", instrplus, "+", exogplus)
    restricted <- paste("condres ~ 1 +", exogplus)
  } else {
    # without exogenous explanatory variables
    unrestricted <- paste("condres", "~", instrplus)
    restricted <- paste("condres ~ 1")
  }
  return(list("unrestricted" = unrestricted, "restricted" = restricted))
}


#' Run Wald test for restricted vs unrestricted model and return conditional F Statistic:
#' @param restricted lm object for restricted model
#' @param unrestricted lm object for unrestricted model
#' @param nendog number of endogenous variables (for degrees of freedom)
#' @noRd
fsw_wald_test <- function(restricted, unrestricted, nendog) {
  # Wald test for restricted vs unrestricted model:
  wldtst <- lmtest::waldtest(restricted, unrestricted)

  # Add to results vectors:
  # Sanderson-Windmeijer conditional F-statistic:
  fsw <- (wldtst$F[2L] * wldtst$Df[2L]) / (wldtst$Df[2L] - (nendog - 1))
  # Degrees of freedom:
  fswdf <- wldtst$Df[2L] - (nendog - 1)
  # Residual degrees of freedom:
  fswresdf <- wldtst$Res.Df[2L]
  # P-value:
  fswp <- stats::pf(fsw, nendog, wldtst$Res.Df[2L], lower.tail = FALSE)

  return(cbind(fsw, fswdf, fswresdf, fswp))
}


#' S3 method for class 'fsw'
#' @param x an object of class "[fsw]".
#' @param digits minimal number of significant digits, see print.default.
#' @param ... further arguments passed to or from other methods.
#' @rdname fsw
#' @export
print.fsw <- function(x, digits = getOption("digits"), ...) {
  cat("\nModel sample size: ", x$n, "\n")
  cat(
    "\nSanderson-Windmeijer conditional F-statistics for first stage model:\n"
  )
  stats::printCoefmat(
    x$fswres,
    cs.ind = 2L:3L,
    tst.ind = 1L,
    has.Pvalue = TRUE,
    P.values = TRUE,
    digits = digits,
    ...
  )
  cat("\n")
  invisible(x)
}
