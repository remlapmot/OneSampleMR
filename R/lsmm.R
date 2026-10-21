#' Logistic structural mean model
#'
#' Function to estimate the double logistic structural mean model (LSMM) of
#' Vansteelandt and Goetghebeur (2003) by the generalized method of moments
#' (GMM), as described by Clarke et al. (2015).
#'
#' The LSMM is
#' \deqn{\mathrm{logit}\, E(Y \mid X, Z) - \mathrm{logit}\, E\{Y(0) \mid X, Z\} = \psi X,}
#' so that \eqn{\exp(\psi)} is a causal odds ratio among the exposed. Unlike
#' the additive and multiplicative structural mean models, the LSMM also
#' requires a model for the observed outcome mean, the association model,
#' which here is a logistic regression of the outcome on the exposures, the
#' instruments, and any covariates. The association model and the causal model
#' are fitted jointly by two-step GMM. The moment conditions for the
#' association model are its logistic regression score equations, and those
#' for the causal model are that the predicted exposure-free potential outcome
#' is mean independent of the instruments. The initial weight matrix is block
#' diagonal, with blocks equal to the inverse of the crossproduct of the
#' regressors of each model, and the second step uses the optimal weight
#' matrix. This is the same estimator as the `ivlsmm` command in the Stata
#' package ivonesamplemr.
#'
#' Covariates are adjusted for by including them in both the exposure and the
#' instrument parts of the formula, as in `y ~ x + w | z1 + z2 + w`. They are
#' then included in both the association model and the causal model, as in
#' `ivlsmm`.
#'
#' @inheritParams msmm
#' @param t0 A vector of starting values for the gmm optimizer. This should
#' have length equal to the number of association model parameters (the
#' intercept, exposures, instruments, and covariates), plus the number of
#' exposures and covariates, plus 1. By default the starting values are the
#' estimates from a logistic regression for the association model and from a
#' two-stage residual inclusion logistic regression for the causal model.
#' @return An object of class `"lsmm"`. A list with the following items:
#'
#' \item{fit}{The object from the second step [gmm::gmm()] fit.}
#' \item{corci}{The causal odds ratio/s of the exposure/s and the
#' corresponding 95% confidence interval limits.}
#' \item{ey0ci}{The estimate of the exposure free potential outcome mean and
#' its 95% confidence interval limits.}
#' \item{covariates}{A character vector of the names of any covariates.}
#' @references
#' Clarke PS, Palmer TM, Windmeijer F. Estimating structural
#' mean models with multiple instrumental variables using the
#' Generalised Method of Moments. Statistical Science, 2015, 30, 1,
#' 96-117. \doi{10.1214/14-STS503}
#'
#' Vansteelandt S, Goetghebeur E. Causal inference with generalized
#' structural mean models. Journal of the Royal Statistical Society: Series B,
#' 2003, 65, 4, 817-835. \doi{10.1046/j.1369-7412.2003.00417.x}
#'
#' Vansteelandt S, Bowden J, Babanezhad M, Goetghebeur E. On instrumental
#' variables estimation of causal odds ratios. Statistical Science, 2011, 26,
#' 3, 403-422. \doi{10.1214/11-STS360}
#' @examples
#' # Multiple instrument example
#' set.seed(123456)
#' n    <- 1000
#' psi0 <- 0.5
#' G1   <- rbinom(n, 2, 0.5)
#' G2   <- rbinom(n, 2, 0.3)
#' G3   <- rbinom(n, 2, 0.4)
#' U    <- runif(n)
#' pX   <- plogis(0.7*G1 + G2 - G3 + U)
#' X    <- rbinom(n, 1, pX)
#' pY   <- plogis(-2 + psi0*X + U)
#' Y    <- rbinom(n, 1, pY)
#' dat  <- data.frame(G1, G2, G3, X, Y)
#' fit  <- lsmm(Y ~ X | G1 + G2 + G3, data = dat)
#' fit
#' summary(fit)
#' @export
lsmm <- function(
  formula,
  instruments,
  data,
  subset,
  na.action,
  contrasts = NULL,
  t0 = NULL,
  ...
) {
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
  stopifnot(length(formula)[1L] == 1L, length(formula)[2L] == 2L)
  ## call model.frame()
  mf$formula <- formula
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  ## extract response, terms, model matrices
  Y <- stats::model.response(mf, "numeric")
  mtX <- stats::terms(formula, data = data, rhs = 1)
  X <- stats::model.matrix(mtX, mf, contrasts)
  mtZ <- stats::delete.response(stats::terms(formula, data = data, rhs = 2))
  Z <- stats::model.matrix(mtZ, mf, contrasts)
  # end of code from ivreg::ivreg()

  if (!all(Y %in% 0:1)) {
    stop("The outcome must be binary, i.e. take values 0 or 1.")
  }

  # covariates are the variables in both the exposure and instrument parts
  # of the formula
  xnames <- colnames(X)[-1]
  znames <- colnames(Z)[-1]
  covariatenames <- intersect(xnames, znames)
  exposurenames <- setdiff(xnames, covariatenames)
  instrumentnames <- setdiff(znames, covariatenames)
  if (length(exposurenames) == 0) {
    stop(
      "At least one exposure (a variable in the exposure part but not ",
      "the instrument part of the formula) must be specified."
    )
  }
  if (length(instrumentnames) < length(exposurenames)) {
    stop("There must be at least as many instruments as exposures.")
  }

  # association model regressors (with intercept), causal model regressors,
  # and causal model instruments (with intercept)
  amnames <- c(exposurenames, instrumentnames, covariatenames)
  cmnames <- c(exposurenames, covariatenames)
  A1 <- cbind(1, X[, exposurenames, drop = FALSE], Z[, c(instrumentnames, covariatenames), drop = FALSE])
  C <- X[, cmnames, drop = FALSE]
  Z1 <- Z[, c("(Intercept)", instrumentnames, covariatenames), drop = FALSE]
  na <- ncol(A1)
  nc <- ncol(C)
  n <- length(Y)

  parnames <- c(
    paste0("assoc:", c("(Intercept)", amnames)),
    cmnames,
    "E[Y(0)]"
  )

  lsmmMoments <- function(theta, x) {
    lp <- drop(A1 %*% theta[1:na])
    p2 <- stats::plogis(lp - drop(C %*% theta[na + seq_len(nc)]))
    cbind(
      (Y - stats::plogis(lp)) * A1,
      (p2 - theta[na + nc + 1]) * Z1
    )
  }

  lsmmGradient <- function(theta, x) {
    lp <- drop(A1 %*% theta[1:na])
    p1 <- stats::plogis(lp)
    p2 <- stats::plogis(lp - drop(C %*% theta[na + seq_len(nc)]))
    d1 <- -p1 * (1 - p1)
    d2 <- p2 * (1 - p2)
    rbind(
      cbind(crossprod(A1, d1 * A1) / n, matrix(0, na, nc + 1)),
      cbind(
        crossprod(Z1, d2 * A1) / n,
        -crossprod(Z1, d2 * C) / n,
        -colMeans(Z1)
      )
    )
  }

  # starting values: logistic regression for the association model, and
  # two-stage residual inclusion logistic regression for the causal model
  if (is.null(t0)) {
    stage1 <- stats::lm(
      X[, exposurenames] ~ Z[, c(instrumentnames, covariatenames)]
    )
    res <- stats::residuals(stage1)
    amfit <- stats::glm(Y ~ -1 + A1, family = stats::binomial)
    cmfit <- stats::glm(Y ~ C + res, family = stats::binomial)
    t0 <- c(
      stats::coef(amfit),
      stats::coef(cmfit)[1 + seq_len(nc)],
      mean(Y)
    )
  }
  if (length(t0) != length(parnames)) {
    stop("t0 must have length ", length(parnames), ".")
  }
  names(t0) <- parnames

  # step 1: block diagonal initial weight matrix
  nz <- ncol(Z1)
  w0 <- matrix(0, na + nz, na + nz)
  w0[1:na, 1:na] <- solve(crossprod(A1) / n)
  w0[na + 1:nz, na + 1:nz] <- solve(crossprod(Z1) / n)
  dat <- matrix(0, n, 1)
  fit1 <- gmm::gmm(
    lsmmMoments,
    x = dat,
    t0 = t0,
    gradv = lsmmGradient,
    weightsMatrix = w0,
    method = "BFGS",
    control = list(reltol = 1e-14, maxit = 1E4)
  )

  # step 2: optimal weight matrix evaluated at the step 1 estimates
  gi <- lsmmMoments(gmm::coef.gmm(fit1), dat)
  w1 <- solve(crossprod(gi) / n)
  fit <- gmm::gmm(
    lsmmMoments,
    x = dat,
    t0 = gmm::coef.gmm(fit1),
    gradv = lsmmGradient,
    weightsMatrix = w1,
    vcov = "TrueFixed",
    method = "BFGS",
    control = list(reltol = 1e-14, maxit = 1E4)
  )

  # describe the fit as two-step GMM, which it is overall, rather than as
  # the fixed weight matrix fit of its second step
  fit$met <- "twoStep"

  # variance-covariance matrix with the optimal weight matrix evaluated at
  # the final estimates, as in Stata's gmm command
  s2 <- crossprod(fit$gt) / n
  fit$vcov <- solve(crossprod(fit$G, solve(s2, fit$G))) / n
  dimnames(fit$vcov) <- list(parnames, parnames)

  if (fit$algoInfo$convergence != 0) {
    warning(
      "The GMM fit has not converged, perhaps try different initial parameter values"
    )
  }

  estci <- cbind(gmm::coef.gmm(fit), gmm::confint.gmm(fit)$test)
  corci <- exp(estci[exposurenames, , drop = FALSE])
  colnames(corci)[1] <- "COR"
  ey0ci <- estci["E[Y(0)]", ]

  output <- list(
    fit = fit,
    corci = corci,
    ey0ci = ey0ci,
    covariates = covariatenames
  )
  class(output) <- "lsmm"
  output
}

#' Summarizing LSMM Fits
#'
#' S3 print and summary methods for objects of class `"lsmm"` and print method
#' for objects of class `"summary.lsmm"`.
#'
#' @param object an object of class `"lsmm"`.
#' @param x an object of class `"summary.lsmm"`.
#' @param digits the number of significant digits to print.
#' @param ... further arguments passed to or from other methods.
#' @return `summary.lsmm()` returns an object of class `"summary.lsmm"`. A list
#' with the following elements:
#'
#' \item{smry}{An object from a call to [gmm::summary.gmm()]}
#' \item{object}{The object of class `"lsmm"` passed to the function.}
#' @examples
#' # For examples see the examples at the bottom of help('lsmm')
#' @export
summary.lsmm <- function(object, ...) {
  smry <- gmm::summary.gmm(object$fit)
  # the weight matrix is not a HAC estimate, so do not report a kernel
  smry$kernel <- NULL
  res <- list(smry = smry, object = object)
  class(res) <- "summary.lsmm"
  res
}

#' @rdname summary.lsmm
#' @export
print.lsmm <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\n")
  gmm::print.gmm(x$fit)
  print_lsmm_results(x, digits = digits, ...)
  invisible(x)
}

#' @rdname summary.lsmm
#' @export
print.summary.lsmm <- function(
  x,
  digits = max(3, getOption("digits") - 3),
  ...
) {
  cat("\nGMM fit summary:\n")
  gmm::print.summary.gmm(x$smry)
  print_lsmm_results(x$object, digits = digits, ...)
  invisible(x)
}

print_lsmm_results <- function(x, digits, ...) {
  hascovs <- length(x$covariates) > 0
  if (hascovs) {
    cat("\nE[Y(0)] when the covariates are 0 with 95% CI:\n")
  } else {
    cat("\nE[Y(0)] with 95% CI:\n")
  }
  print(x$ey0ci, digits = digits, ...)

  cat("\nCausal odds ratio with 95% CI:\n")
  print(x$corci, digits = digits, ...)
  if (hascovs) {
    cat("\nAdjusted for:", paste(x$covariates, collapse = ", "), "\n")
  }
  cat("\n")
}
