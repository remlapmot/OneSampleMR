#' Methods for Fitted Models
#'
#' Extract the estimated parameters, their variance-covariance matrix, Wald
#' confidence intervals, the number of observations, and the model formula
#' from models fitted by [msmm()], [lsmm()], [tsps()], and [tsri()].
#'
#' The parameters are those of the underlying GMM (or, for the `"tsls"` and
#' `"tslsalt"` methods of [msmm()], two-stage least squares) fit, on the
#' scale on which they are estimated. For example, the causal effect
#' parameter of [msmm()] is a log causal risk ratio and that of [lsmm()] a
#' log causal odds ratio. The exponentiated causal effect estimates are
#' available in the `crrci` (MSMM), `corci` (LSMM), and `estci` (TSPS and
#' TSRI) elements of the fitted objects, and are printed by their `print()`
#' and `summary()` methods.
#'
#' @param object a fitted model object of class `"msmm"`, `"lsmm"`,
#' `"tsps"`, or `"tsri"`.
#' @param parm a specification of which parameters are to be given
#' confidence intervals, either a vector of numbers or a vector of names. If
#' missing, all parameters are considered.
#' @param level the confidence level required.
#' @param ... further arguments passed to or from other methods.
#' @return `coef()` returns a named vector of the parameter estimates,
#' `vcov()` their variance-covariance matrix, `confint()` a matrix of Wald
#' confidence limits, `nobs()` the number of observations, and `formula()`
#' the model formula.
#' @examples
#' set.seed(123456)
#' n    <- 1000
#' G1   <- rbinom(n, 2, 0.5)
#' G2   <- rbinom(n, 2, 0.3)
#' G3   <- rbinom(n, 2, 0.4)
#' U    <- runif(n)
#' X    <- rbinom(n, 1, plogis(0.7*G1 + G2 - G3 + U))
#' Y    <- rbinom(n, 1, plogis(-2 + 0.5*X + U))
#' dat  <- data.frame(G1, G2, G3, X, Y)
#' fit  <- msmm(Y ~ X | G1 + G2 + G3, data = dat)
#' coef(fit)
#' vcov(fit)
#' confint(fit)
#' nobs(fit)
#' formula(fit)
#' @name OneSampleMR-methods
NULL

coef_fit <- function(object, ...) {
  stats::coef(object$fit)
}

vcov_fit <- function(object, ...) {
  v <- stats::vcov(object$fit)
  dimnames(v) <- list(names(stats::coef(object)), names(stats::coef(object)))
  v
}

nobs_fit <- function(object, ...) {
  if (inherits(object$fit, "gmm")) {
    object$fit$n
  } else {
    stats::nobs(object$fit)
  }
}

#' @rdname OneSampleMR-methods
#' @export
coef.msmm <- function(object, ...) coef_fit(object, ...)

#' @rdname OneSampleMR-methods
#' @export
coef.lsmm <- function(object, ...) coef_fit(object, ...)

#' @rdname OneSampleMR-methods
#' @export
coef.tsps <- function(object, ...) coef_fit(object, ...)

#' @rdname OneSampleMR-methods
#' @export
coef.tsri <- function(object, ...) coef_fit(object, ...)

#' @rdname OneSampleMR-methods
#' @export
vcov.msmm <- function(object, ...) vcov_fit(object, ...)

#' @rdname OneSampleMR-methods
#' @export
vcov.lsmm <- function(object, ...) vcov_fit(object, ...)

#' @rdname OneSampleMR-methods
#' @export
vcov.tsps <- function(object, ...) vcov_fit(object, ...)

#' @rdname OneSampleMR-methods
#' @export
vcov.tsri <- function(object, ...) vcov_fit(object, ...)

#' @rdname OneSampleMR-methods
#' @export
confint.msmm <- function(object, parm, level = 0.95, ...) {
  stats::confint.default(object, parm, level, ...)
}

#' @rdname OneSampleMR-methods
#' @export
confint.lsmm <- function(object, parm, level = 0.95, ...) {
  stats::confint.default(object, parm, level, ...)
}

#' @rdname OneSampleMR-methods
#' @export
confint.tsps <- function(object, parm, level = 0.95, ...) {
  stats::confint.default(object, parm, level, ...)
}

#' @rdname OneSampleMR-methods
#' @export
confint.tsri <- function(object, parm, level = 0.95, ...) {
  stats::confint.default(object, parm, level, ...)
}

#' @rdname OneSampleMR-methods
#' @export
nobs.msmm <- function(object, ...) nobs_fit(object, ...)

#' @rdname OneSampleMR-methods
#' @export
nobs.lsmm <- function(object, ...) nobs_fit(object, ...)

#' @rdname OneSampleMR-methods
#' @export
nobs.tsps <- function(object, ...) nobs_fit(object, ...)

#' @rdname OneSampleMR-methods
#' @export
nobs.tsri <- function(object, ...) nobs_fit(object, ...)

#' @rdname OneSampleMR-methods
#' @param x a fitted model object of class `"msmm"`, `"lsmm"`, `"tsps"`,
#' or `"tsri"`.
#' @export
formula.msmm <- function(x, ...) x$formula

#' @rdname OneSampleMR-methods
#' @export
formula.lsmm <- function(x, ...) x$formula

#' @rdname OneSampleMR-methods
#' @export
formula.tsps <- function(x, ...) x$formula

#' @rdname OneSampleMR-methods
#' @export
formula.tsri <- function(x, ...) x$formula
