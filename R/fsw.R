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

    # Number of endogenous variables:
    nendog <- length(object$endogenous)

    # Error for less than 2 endogenous variables:
    if (nendog < 2) {
      stop("The number of exposures must be 2 or more.")
    }

    # Number of excluded instruments:
    ninstruments <- length(object$instruments)

    # Number of exogenous explanatory variables:
    nexogenous <- length(object$exogenous) - 1

    # Names of endogenous variables:
    namesendog <- labels(object$terms$regressors)[1:nendog]

    # Error for factor variables among endogenous variables:
    endogfcterrmsg <- paste(
      "One or more of your exposure variables is a factor.",
      "Please convert to numeric with say as.numeric(),",
      "refit your ivreg() model, and rerun fsw()."
    )
    if ("factor" %in% lapply(object$model[namesendog], class)) {
      stop(endogfcterrmsg)
    }

    # Names of exogenous explanatory variables:
    namesexog <- labels(object$terms$regressors)[-(1:nendog)]

    # Names of excluded instruments:
    namesinstruments <- names(object$instruments)

    # Sample size:
    n <- object$n

    # Create plus-separated string of excluded instruments (e.g. "z1 + z2 + ..."):
    instrplus <- paste(namesinstruments, collapse = " + ")

    # Create plus-separated string of exogenous explanatory variables (if any):
    exogplus <- NULL
    if (nexogenous > 0) {
      exogplus <- paste(namesexog, collapse = " + ")
    }

    # Create equations of the unrestricted and restricted models compared in the Wald test:
    equations <- wald_equations(nexogenous, instrplus, exogplus)

    # Obtain conditional F statistic for each endogenous explanatory variables:
    fswres <- sapply(namesendog, function(endogoutcome) {
      # Names of other endogenous explanatory variables:
      endogothers <- namesendog[!(namesendog %in% endogoutcome)]

      # Create conditional model equation:
      if (nexogenous > 0) {
        # with exogenous explanatory variables
        # Create condit. model equation (endog. expl. var regressed against other endog. expl. vars and exog. vars, instrumented with excl. instruments)
        modelstr <- paste(
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
        # without exogenous explanatory variables
        # Create condit. model equation (endog. expl. var regressed against other endog. expl. vars, instrumented with excl. instruments)
        modelstr <- paste(
          endogoutcome,
          "~",
          paste(endogothers, collapse = " + "),
          "|",
          instrplus
        )
      }

      # Convert condit. model equation to formula:
      modelfor <- stats::as.formula(modelstr)
      # Estimate condit. model using iv_robust:
      condmod <- try(ivreg::ivreg(modelfor, data = object$model), silent = TRUE)

      # Error message if condit. model estimation fails:
      condmoderrmsg <- paste(
        "The IV regression of one of the exposures",
        "on the other/s has failed.",
        "This is most likely because you have a transformation",
        "on one or more of the exposure or instrumental variables.",
        "Please create the transformed variable/s in your",
        "data.frame and refit,",
        "e.g. instead of creating your ivreg object from",
        "ivreg(y ~ log(x1) + x2 | z1 + z2 + z3)",
        "please create dat$logx1 = log(x1) in your data.frame",
        "and fit ivreg(y ~ logx1 + x2 | z1 + z2 + z3)."
      )
      if (inherits(condmod, "try-error")) {
        stop(condmoderrmsg)
      }

      # Residuals of the condit. model:
      condres <- condmod$residuals

      # Unrestricted model for Wald test:
      # Estimate regression of condit. residuals against instruments (and any exogenous regressors):
      resmod <- stats::lm(
        stats::as.formula(equations$unrestricted),
        data = object$model
      )
      # summary(resmod)

      # Restricted model for Wald test:
      # Estimate regression of condit. residuals against intercept (and any exogenous regressors):
      resbase <- stats::lm(
        stats::as.formula(equations$restricted),
        data = object$model
      )
      # summary(resbase)

      # Compute conditional F-Statistic using Wald test for restricted vs unrestricted model:
      fsw_wald_test(resbase, resmod, nendog)
    })

    # Prepare results vectors:
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
    return(output)
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

    # Number of endogenous variables:
    nendog <- length(xvars[!xvars %in% zvars])

    # Error for less than 2 endogenous variables:
    if (nendog < 2) {
      stop("The number of exposures must be 2 or more.")
    }

    # Number of excluded instruments:
    ninstruments <- length(zvars[!zvars %in% xvars])

    # Number of exogenous explanatory variables:
    nexogenous <- length(xvars[xvars %in% zvars])

    # Names of endogenous variables:
    namesendog <- xvars[!xvars %in% zvars]

    # Error for factor variables among endogenous variables:
    endogfcterrmsg <- paste(
      "One or more of your exposure variables is a factor.",
      "Please convert to numeric with say as.numeric(),",
      "refit your iv_robust() model, and rerun fsw()."
    )
    if ("factor" %in% lapply(object$model[namesendog], class)) {
      stop(endogfcterrmsg)
    }

    # Names of exogenous explanatory variables:
    namesexog <- xvars[xvars %in% zvars]

    # Names of excluded instruments:
    namesinstruments <- zvars[!zvars %in% xvars]

    # Sample size:
    n <- object$n

    # Create plus-separated string of excluded instruments (e.g. "z1 + z2 + ..."):
    instrplus <- paste(namesinstruments, collapse = " + ")

    # Create plus-separated string of exogenous explanatory variables (if any):
    exogplus <- NULL
    if (nexogenous > 0) {
      exogplus <- paste(namesexog, collapse = " + ")
    }

    # Create equations of the unrestricted and restricted models compared in the Wald test:
    equations <- wald_equations(nexogenous, instrplus, exogplus)

    # Obtain conditional F statistic for each endogenous explanatory variables:
    fswres <- sapply(namesendog, function(endogoutcome) {
      # Names of other endogenous explanatory variables:
      endogothers <- namesendog[!(namesendog %in% endogoutcome)]

      # Create conditional model equation:
      if (nexogenous > 0) {
        # with exogenous explanatory variables
        # Create plus-separated string of exogenous explanatory variables:
        exogplus <- paste(namesexog, collapse = " + ")
        # Create condit. model equation (endog. expl. var regressed against other endog. expl. vars and exog. vars, instrumented with excl. instruments)
        modelstr <- paste(
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
        # without exogenous explanatory variables
        # Create condit. model equation (endog. expl. var regressed against other endog. expl. vars, instrumented with excl. instruments)
        modelstr <- paste(
          endogoutcome,
          "~",
          paste(endogothers, collapse = " + "),
          "|",
          instrplus
        )
      }

      # Convert condit. model equation to formula:
      modelfor <- stats::as.formula(modelstr)
      # Estimate condit. model using iv_robust:
      condmod <- try(AER::ivreg(modelfor, data = object$model), silent = TRUE)

      # Error message if condit. model estimation fails:
      condmoderrmsg <- paste(
        "The IV regression of one of the exposures",
        "on the other/s has failed.",
        "This is most likely because you have a transformation",
        "on one or more of the exposure or instrumental variables.",
        "Please create the transformed variable/s in your",
        "data.frame and refit,",
        "e.g. instead of creating your iv_robust object from",
        "ivreg(y ~ log(x1) + x2 | z1 + z2 + z3)",
        "please create dat$logx1 = log(x1) in your data.frame",
        "and fit ivreg(y ~ logx1 + x2 | z1 + z2 + z3)."
      )
      if (inherits(condmod, "try-error")) {
        stop(condmoderrmsg)
      }

      # Residuals of the condit. model:
      condres <- condmod$residuals

      # Unrestricted model for Wald test:
      # Estimate regression of condit. residuals against instruments (and any exogenous regressors):
      resmod <- stats::lm(
        stats::as.formula(equations$unrestricted),
        data = object$model
      )
      # summary(resmod)

      # Restricted model for Wald test:
      # Estimate regression of condit. residuals against intercept (and any exogenous regressors):
      resbase <- stats::lm(
        stats::as.formula(equations$restricted),
        data = object$model
      )
      # summary(resbase)

      # Compute conditional F-Statistic using Wald test for restricted vs unrestricted model:
      fsw_wald_test(resbase, resmod, nendog)
    })

    # Prepare results vectors:
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
    return(output)
  }
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

  # Number of endogenous variables:
  nendog <- length(xvars[!xvars %in% zvars])

  # Error for less than 2 endogenous variables:
  if (nendog < 2) {
    stop("The number of exposures must be 2 or more.")
  }

  # Number of excluded instruments:
  ninstruments <- length(zvars[!zvars %in% xvars])

  # Number of exogenous explanatory variables:
  nexogenous <- length(xvars[xvars %in% zvars])

  # Names of endogenous variables:
  namesendog <- xvars[!xvars %in% zvars]

  # Error for factor variables among endogenous variables:
  endogfcterrmsg <- paste(
    "One or more of your exposure variables is a factor.",
    "Please convert to numeric with say as.numeric(),",
    "refit your iv_robust() model, and rerun fsw()."
  )
  if ("factor" %in% lapply(get_data(object)[namesendog], class)) {
    stop(endogfcterrmsg)
  }

  # Names of exogenous explanatory variables:
  namesexog <- xvars[xvars %in% zvars]

  # Names of excluded instruments:
  namesinstruments <- zvars[!zvars %in% xvars]

  # Sample size:
  n <- object$nobs

  # Create plus-separated string of excluded instruments (e.g. "z1 + z2 + ..."):
  instrplus <- paste(namesinstruments, collapse = " + ")

  # Create plus-separated string of exogenous explanatory variables (if any):
  exogplus <- NULL
  if (nexogenous > 0) {
    exogplus <- paste(namesexog, collapse = " + ")
  }

  # Create equations of the unrestricted and restricted models compared in the Wald test:
  equations <- wald_equations(nexogenous, instrplus, exogplus)

  dat <- get_data(object)

  # Obtain conditional F statistic for each endogenous explanatory variables:
  fswres <- sapply(namesendog, function(endogoutcome) {
    # Names of other endogenous explanatory variables:
    endogothers <- namesendog[!(namesendog %in% endogoutcome)]

    # Create conditional model equation:
    if (nexogenous > 0) {
      # with exogenous explanatory variables
      # Create condit. model equation (endog. expl. var regressed against other endog. expl. vars and exog. vars, instrumented with excl. instruments)
      modelstr <- paste(
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
      # without exogenous explanatory variables
      # Create condit. model equation (endog. expl. var regressed against other endog. expl. vars, instrumented with excl. instruments)
      modelstr <- paste(
        endogoutcome,
        "~",
        paste(endogothers, collapse = " + "),
        "|",
        instrplus
      )
    }

    # Convert condit. model equation to formula:
    modelfor <- stats::as.formula(modelstr)
    # Estimate condit. model using iv_robust:
    condmod <- try(
      estimatr::iv_robust(modelfor, data = dat, se_type = "classical"),
      silent = TRUE
    )

    # Error message if condit. model estimation fails:
    condmoderrmsg <- paste(
      "The IV regression of one of the exposures",
      "on the other/s has failed.",
      "This is most likely because you have a transformation",
      "on one or more of the exposure or instrumental variables.",
      "Please create the transformed variable/s in your",
      "data.frame and refit,",
      "e.g. instead of creating your iv_robust object from",
      "iv_robust(y ~ log(x1) + x2 | z1 + z2 + z3)",
      "please create dat$logx1 = log(x1) in your data.frame",
      "and fit iv_robust(y ~ logx1 + x2 | z1 + z2 + z3)."
    )
    if (inherits(condmod, "try-error")) {
      stop(condmoderrmsg)
    }

    # Residuals of the condit. model:
    condres <- dat[[endogoutcome]] - condmod$fitted.values

    # Unrestricted model for Wald test:
    # Estimate regression of condit. residuals against instruments (and any exogenous regressors):
    resmod <- stats::lm(stats::as.formula(equations$unrestricted), data = dat)
    # summary(resmod)

    # Restricted model for Wald test:
    # Estimate regression of condit. residuals against intercept (and any exogenous regressors):
    resbase <- stats::lm(stats::as.formula(equations$restricted), data = dat)
    # summary(resbase)

    # Compute conditional F-Statistic using Wald test for restricted vs unrestricted model:
    fsw_wald_test(resbase, resmod, nendog)
  })

  # Prepare results vectors:
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
  return(output)
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

  # Names and number of endogenous variables:
  namesendog <- object$iv_endo_names
  nendog <- length(object$iv_endo_names)

  # Error for less than 2 endogenous variables:
  if (nendog < 2) {
    stop("The number of exposures must be 2 or more.")
  }

  # Names and number of excluded instruments:
  namesinstruments <- object$iv_inst_names
  ninstruments <- object$iv_n_inst

  # Names and number of exogenous explanatory variables:
  namesexog <- attr(
    stats::terms(Formula::Formula(object$fml), rhs = 1),
    "term.labels"
  )
  nexogenous <- length(namesexog)

  # Error for factor variables among endogenous variables:
  endogfcterrmsg <- paste(
    "One or more of your exposure variables is a factor.",
    "Please convert to numeric with say as.numeric(),",
    "refit your iv_robust() model, and rerun fsw()."
  )
  if ("factor" %in% lapply(get_data(object)[namesendog], class)) {
    stop(endogfcterrmsg)
  }

  # Sample size:
  n <- object$nobs

  # Create plus-separated string of excluded instruments (e.g. "z1 + z2 + ..."):
  instrplus <- paste(namesinstruments, collapse = " + ")

  # Create plus-separated string of exogenous explanatory variables (if any):
  exogplus <- NULL
  if (nexogenous > 0) {
    exogplus <- paste(namesexog, collapse = " + ")
  }

  # Create equations of the unrestricted and restricted models compared in the Wald test:
  equations <- wald_equations(nexogenous, instrplus, exogplus)

  dat <- get_data(object)

  # Obtain conditional F statistic for each endogenous explanatory variables:
  fswres <- sapply(namesendog, function(endogoutcome) {
    # Names of other endogenous explanatory variables:
    endogothers <- namesendog[!(namesendog %in% endogoutcome)]

    # Create conditional model equation:
    if (nexogenous > 0) {
      # with exogenous explanatory variables
      # Create condit. model equation (endog. expl. var regressed against other endog. expl. vars and exog. vars, instrumented with excl. instruments)
      modelstr <- paste(
        endogoutcome,
        "~",
        exogplus,
        "|",
        paste(endogothers, collapse = " + "),
        "~",
        instrplus
      )
    } else {
      # without exogenous explanatory variables
      # Create condit. model equation (endog. expl. var regressed against other endog. expl. vars, instrumented with excl. instruments)
      modelstr <- paste(
        endogoutcome,
        "~",
        "1 |",
        paste(endogothers, collapse = " + "),
        "~",
        instrplus
      )
    }

    # Convert condit. model equation to formula:
    modelfor <- stats::as.formula(modelstr)
    # Estimate condit. model using feols:
    condmod <- try(fixest::feols(modelfor, data = dat), silent = TRUE)

    # Error message if condit. model estimation fails:
    condmoderrmsg <- paste(
      "The IV regression of one of the exposures",
      "on the other/s has failed.",
      "This is most likely because you have a transformation",
      "on one or more of the exposure or instrumental variables.",
      "Please create the transformed variable/s in your",
      "data.frame and refit,",
      "e.g. instead of creating your fixest object from",
      "feols(y ~ 1 | log(x1) + x2 ~ z1 + z2 + z3)",
      "please create dat$logx1 = log(x1) in your data.frame",
      "and fit feols(y ~ 1 | logx1 + x2 ~ z1 + z2 + z3)."
    )
    if (inherits(condmod, "try-error")) {
      stop(condmoderrmsg)
    }

    # Residuals of the condit. model:
    condres <- condmod$residuals

    # Unrestricted model for Wald test:
    # Estimate regression of condit. residuals against instruments (and any exogenous regressors):
    resmod <- stats::lm(stats::as.formula(equations$unrestricted), data = dat)
    # summary(resmod)

    # Restricted model for Wald test:
    # Estimate regression of condit. residuals against intercept (and any exogenous regressors):
    resbase <- stats::lm(stats::as.formula(equations$restricted), data = dat)
    # summary(resbase)

    # Compute conditional F-Statistic using Wald test for restricted vs unrestricted model:
    fsw_wald_test(resbase, resmod, nendog)
  })

  # Prepare results vector:
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
  return(output)
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
