#' Multiplicative structural mean model
#'
#' Function providing several methods to estimate the multiplicative
#' structural mean model (MSMM) of Robins (1989).
#'
#' Function providing several methods to estimate the multiplicative
#' structural mean model (MSMM) of Robins (1989). These are the methods
#' described in Clarke, Palmer, and Windmeijer (2015), most notably
#' generalised method of moments (GMM) estimation of the MSMM.
#'
#' An equivalent estimator to the MSMM was proposed in Econometrics by Mullahy (1997) and
#' then discussed in several articles by Windmeijer (1997, 2002) and Cameron
#' and Trivedi (2013). This was implemented in the user-written Stata command `ivpois`
#' (Nichols, 2007) and then implemented in official Stata in the `ivpoisson`
#' command (StataCorp., 2013).
#'
#' @param formula,instruments formula specification(s) of the regression
#' relationship and the instruments. Either \code{instruments} is missing and
#' \code{formula} has three parts as in \code{y ~ x1 + x2 | z1 + z2 + z3}
#' (recommended) or \code{formula} is \code{y ~ x1 + x2} and \code{instruments}
#' is a one-sided formula \code{~ z1 + z2 + z3} (only for backward
#' compatibility).
#' @param data an optional data frame containing the variables in the model.
#' By default the variables are taken from the environment of the
#' \code{formula}.
#' @param subset an optional vector specifying a subset of observations to be
#' used in fitting the model.
#' @param na.action a function that indicates what should happen when the data
#' contain \code{NA}s. The default is set by the \code{na.action} option.
#' @param contrasts an optional list. See the \code{contrasts.arg} of
#' \code{\link[stats:model.matrix]{model.matrix.default}}.
#' @param estmethod Estimation method, please use one of
#'
#'    * `"gmm"` GMM estimation of the MSMM (the default).
#'    * `"gmmalt"` GMM estimation of the alternative moment conditions
#'    for the MSMM as per Clarke et al. (2015). These are the same moment
#'    conditions fit by the user-written Stata command `ivpois` (Nichols, 2007)
#'    and by the official Stata command `ivpoisson gmm ..., multiplicative`
#'    (StataCorp., 2013).
#'    * `"tsls"` the TSLS method of fitting the MSMM of Clarke et al. (2015).
#'    For binary \eqn{Y} and \eqn{X} this uses \eqn{Y*(1-X)} as the outcome and
#'    \eqn{Y*X} as the exposure.
#'    * `"tslsalt"` the alternative TSLS method of fitting the MSMM of
#'    Clarke et al. (2015). For binary \eqn{Y} and \eqn{X} this uses \eqn{Y*X}
#'    as the outcome and \eqn{Y*(1-X)} as the exposure.
#' @param ... further arguments passed to or from other methods.
#' @references
#' Cameron AC, Trivedi PK. Regression analysis of count data. 2nd ed. 2013.
#' New York, Cambridge University Press.
#'
#' Clarke PS, Palmer TM, Windmeijer F. Estimating structural
#' mean models with multiple instrumental variables using the
#' Generalised Method of Moments. Statistical Science, 2015, 30, 1,
#' 96-117. \doi{10.1214/14-STS503}
#'
#' Hernan and Robins. Instruments for causal inference: An
#' Epidemiologist's dream? Epidemiology, 2006, 17, 360-372.
#' \doi{10.1097/01.ede.0000222409.00878.37}
#'
#' Mullahy J. Instrumental-variable estimation of count data models:
#' applications to models of cigarette smoking and behavior. The Review of
#' Economics and Statistics. 1997, 79, 4, 586-593.
#' \doi{https://doi.org/10.1162/003465397557169}
#'
#' Nichols A. ivpois: Stata module for IV/GMM Poisson regression. 2007.
#' [url](http://ideas.repec.org/c/boc/bocode/s456890.html)
#'
#' Robins JM. The analysis of randomised and
#' nonrandomised AIDS treatment trials using a new approach to
#' causal inference in longitudinal studies.
#' In Health Service Research Methodology: A Focus on AIDS
#' (L. Sechrest, H. Freeman and A. Mulley, eds.). 1989. 113–159.
#' US Public Health Service, National Center for Health Services Research,
#' Washington, DC.
#'
#' StataCorp. Stata Base Reference Manual. Release 13.
#' ivpoisson - Poisson model with continuous endogenous covariates. 2013.
#' [url](https://www.stata.com/manuals13/rivpoisson.pdf)
#'
#' Windmeijer FAG, Santos Silva JMC. Endogeneity in Count Data Models:
#' An Application to Demand for Health Care. Journal of Applied Econometrics.
#' 1997, 12, 3, 281-294.
#' <https://doi.org/10.1002/(SICI)1099-1255(199705)12:3%3C281::AID-JAE436%3E3.0.CO;2-1>
#'
#' Windmeijer, F. ExpEnd, A Gauss programme for non-linear GMM estimation of
#' EXPonential models with ENDogenous regressors for cross section and panel
#' data. CEMMAP working paper CWP14/02. 2002. [url](https://www.cemmap.ac.uk/wp-content/uploads/2020/08/CWP1402.pdf)
#' @examples
#' # Single instrument example
#' # Data generation from the example in the ivtools::ivglm() helpfile
#' set.seed(9)
#' n <- 1000
#' psi0 <- 0.5
#' Z <- rbinom(n, 1, 0.5)
#' X <- rbinom(n, 1, 0.7*Z + 0.2*(1 - Z))
#' m0 <- plogis(1 + 0.8*X - 0.39*Z)
#' Y <- rbinom(n, 1, plogis(psi0*X + log(m0/(1 - m0))))
#' dat <- data.frame(Z, X, Y)
#' msmm(Y ~ X | Z, data = dat)
#' msmm(Y ~ X | Z, data = dat, estmethod = "gmm")
#' msmm(Y ~ X | Z, data = dat, estmethod = "gmmalt")
#' msmm(Y ~ X | Z, data = dat, estmethod = "tsls")
#' msmm(Y ~ X | Z, data = dat, estmethod = "tslsalt")
#'
#' mod <- msmm(Y ~ X | Z, data = dat)
#' smy <- summary(mod)
#' class(mod)
#' class(smy)
#' mod
#' smy
#' print(mod)
#' print(smy)
#'
#' mod2 <- msmm(Y ~ X | Z, data = dat, estmethod = "tsls")
#' mod2
#' summary(mod2)
#'
#' # check variables with different names
#' dat$E <- dat$X
#' dat$R <- dat$Y
#' dat$W <- dat$Z
#' msmm(R ~ E | W, data = dat)
#'
#' # non-binary y fail
#' set.seed(9)
#' n <- 1000
#' psi0 <- 0.5
#' Z <- rbinom(n, 1, 0.5)
#' X <- rbinom(n, 1, 0.7*Z + 0.2*(1 - Z))
#' m0 <- plogis(1 + 0.8*X - 0.39*Z)
#' Y <- rbinom(n, 1, plogis(psi0*X + log(m0/(1 - m0))))
#' dat <- data.frame(Z, X, Y)
#' dat$Y[1] <- 2
#' try(msmm(Y ~ X | Z, data = dat))
#'
#' # non-binary x with tsls, tslsalt methods fail
#' set.seed(9)
#' n <- 1000
#' psi0 <- 0.5
#' Z <- rbinom(n, 1, 0.5)
#' X <- rbinom(n, 1, 0.7*Z + 0.2*(1 - Z))
#' m0 <- plogis(1 + 0.8*X - 0.39*Z)
#' Y <- rbinom(n, 1, plogis(psi0*X + log(m0/(1 - m0))))
#' dat <- data.frame(Z, X, Y)
#' dat$X[1] <- 2
#' try(msmm(Y ~ X | Z, data = dat, estmethod = "tsls"))
#' try(msmm(Y ~ X | Z, data = dat, estmethod = "tslsalt"))
#'
#' # Multiple instrument example
#' set.seed(123456)
#' n <- 1000
#' psi0 <- 0.5
#' G1 <- rbinom(n, 2, 0.5)
#' G2 <- rbinom(n, 2, 0.3)
#' G3 <- rbinom(n, 2, 0.4)
#' U <- runif(n)
#' pX <- plogis(0.7*G1 + G2 - G3 + U)
#' X <- rbinom(n, 1, pX)
#' pY <- plogis(-2 + psi0*X + U)
#' Y <- rbinom(n, 1, pY)
#' dat <- data.frame(G1, G2, G3, X, Y)
#' msmm(Y ~ X | G1 + G2 + G3, data = dat)
#' msmm(Y ~ X | G1 + G2 + G3, data = dat, estmethod = "gmm")
#' msmm(Y ~ X | G1 + G2 + G3, data = dat, estmethod = "gmmalt")
#' msmm(Y ~ X | G1 + G2 + G3, data = dat, estmethod = "tsls")
#' msmm(Y ~ X | G1 + G2 + G3, data = dat, estmethod = "tslsalt")
#'
#' # Multiple exposure example
#' set.seed(123456)
#' n <- 1000
#' psi0 <- 0.5
#' psi1 <- 0.4
#' G1 <- rbinom(n, 2, 0.5)
#' G2 <- rbinom(n, 2, 0.3)
#' G3 <- rbinom(n, 2, 0.4)
#' U <- runif(n)
#' pX1 <- plogis(0.7*G1 + G2 - G3 + U)
#' X1 <- rbinom(n, 1, pX1)
#' pX2 <- plogis(-1 + 0.2*G1 - 0.2*G2 + 0.4*G3 + U)
#' X2 <- rbinom(n, 1, pX2)
#' pY <- plogis(-2 + psi0*X1 + psi1*X2 + U)
#' Y <- rbinom(n, 1, pY)
#' table(Y)
#' dat <- data.frame(G1, G2, G3, X1, X2, Y)
#' msmm(Y ~ X1 + X2 | G1 + G2 + G3, data = dat)
#' msmm(Y ~ X1 + X2 | G1 + G2 + G3, data = dat, estmethod = "gmm")
#' msmm(Y ~ X1 + X2 | G1 + G2 + G3, data = dat, estmethod = "gmmalt")
#' try(msmm(Y ~ X1 + X2 | G1 + G2 + G3, data = dat, estmethod = "tsls"))
#' try(msmm(Y ~ X1 + X2 | G1 + G2 + G3, data = dat, estmethod = "tslsalt"))
#'
#' # With different variable names
#' E1 <- X1
#' E2 <- X2
#' R <- Y
#' dat <- data.frame(G1, G2, G3, E1, E2, R)
#' msmm(Y ~ E1 + E2 | G1 + G2 + G3, data = dat)
#'
#' @export
#' @importFrom stats coef confint delete.response model.matrix model.response terms update vcov
msmm <- function(formula, instruments, data, subset, na.action,
                 contrasts = NULL,
                 estmethod = c("gmm", "gmmalt", "tsls", "tslsalt"),
                 ...) {
  #  ivreg::ivreg() arguments I haven't implemented:
  # weights, offset,
  # model = TRUE, y = TRUE, x = FALSE,

  # code from beginning for ivreg::ivreg()
  estmethod <- match.arg(estmethod, c("gmm", "gmmalt", "tsls", "tslsalt"))
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

  estmethod <- match.arg(estmethod, c("gmm", "gmmalt", "tsls", "tslsalt"))

  # check y binary
  if (!all(Y %in% 0:1))
    stop("The outcome must be binary, i.e. take values 0 or 1.")

  # for TSLS methods check X binary
  if (estmethod %in% c("tsls", "tslsalt") & !all(X %in% 0:1))
    stop("The exposure must be binary, i.e. take values 0 or 1.")

  # check for only 1 exposure for tsls methods
  nX <- ncol(X) - 1
  if (nX != 1 & estmethod %in% c("tsls", "tslsalt"))
    stop("With tsls and tslsalt only 1 exposure variable is allowed.")

  if (estmethod == "gmm")
    output = msmm_gmm(x = X[,-1], y = Y, z = Z[,-1])
  if (estmethod == "gmmalt")
    output = msmm_gmm_alt(x = X[,-1], y = Y, z = Z[,-1])
  if (estmethod == "tsls")
    output = msmm_tsls(x = X[,-1], y = Y, z = Z[,-1])
  if (estmethod == "tslsalt")
    output = msmm_tsls_alt(x = X[,-1], y = Y, z = Z[,-1])

  class(output) <- append("msmm", class(output))
  output
}

msmm_tsls <- function(x, y, z) {

  outcome <- y * (1 - x)
  exposure <- y * x

  # first stage
  stage1 <- lm(exposure ~ z)

  # tsls fit
  fit <- ivreg::ivreg(outcome ~ exposure | z)

  # transformed causal risk ratio estimate
  beta <- coef(fit)

  # log crr
  logcrr <- log(-1 / beta[2])

  # delta-method SE for log crr
  estvar <- vcov(fit)
  logcrrse <- msm::deltamethod(~ log(-1 / x2), beta, estvar)

  # crr with 95% CI
  crrci <- unname(c(-1/beta[2], exp(logcrr - 1.96*logcrrse), exp(logcrr + 1.96*logcrrse)))

  # baseline risk
  ey0ci <- cbind(coef(fit), confint(fit))[1,]

  # list of results to return
  reslist <- list(stage1 = stage1,
                  fit = fit,
                  crrci = crrci,
                  ey0ci = ey0ci,
                  estmethod = "tsls")
  return(reslist)
}

msmm_tsls_alt <- function(x, y, z) {

  outcome <- y * x
  exposure <- y * (1 - x)

  # first stage
  stage1 <- lm(exposure ~ z)

  # tsls fit
  fit <- ivreg::ivreg(outcome ~ exposure | z)

  # transformed causal risk ratio estimate
  beta <- coef(fit)

  # log crr
  logcrr <- log(-1 * beta[2])

  # delta-method SE for log crr
  estvar <- vcov(fit)
  logcrrse <- msm::deltamethod(~ log(-1 * x2), beta, estvar)

  # crr with 95% CI
  crrci <- unname(c(-1*beta[2], exp(logcrr - 1.96*logcrrse), exp(logcrr + 1.96*logcrrse)))

  # list of results to return
  reslist <- list(stage1 = stage1,
                  fit = fit,
                  crrci = crrci,
                  estmethod = "tslsalt")
  return(reslist)
}

msmmMoments <- function(theta, x){
  # extract variables from x
  Y <- as.matrix(x[,"y"])
  xcolstop <- length(theta)
  X <- as.matrix(x[,2:xcolstop])
  zcolstart <- 1 + length(theta) # 1 is y, length(theta) is nX
  zcolstop <- ncol(x)
  Z <- as.matrix(x[,zcolstart:zcolstop])
  nZ <- zcolstop - zcolstart + 1
  nZp1 <- nZ + 1

  linearpredictor <- -1 * X %*% t(theta[-1])

  # moments
  moments <- matrix(nrow = nrow(x), ncol = nZp1, NA)
  # moments[,1] <- (Y*exp(-1*X*theta[2]) - theta[1])
  moments[,1] <- (Y*exp(linearpredictor) - theta[1])
  for (i in 1:nZ) {
    j <- i + 1
    # moments[,j] <- (Y*exp(-1*X*theta[2]) - theta[1])*Z[,i]
    moments[,j] <- (Y*exp(linearpredictor) - theta[1])*Z[,i]
  }
  return(moments)
}

msmm_gmm <- function(x, y, z){

  x <- as.matrix(x)
  if (ncol(x) >= 2)
    xnames <- colnames(x)

  dat = data.frame(y, x, z)

  t0 <- rep(0, ncol(x) + 1)

  # gmm fit
  fit <- gmm::gmm(msmmMoments, x = dat, t0 = t0, vcov = "iid")

  if (fit$algoInfo$convergence != 0)
    warning("The GMM fit has not converged, perhaps try different initial parameter values")

  # causal risk ratio
  crrci <- exp(cbind(gmm::coef.gmm(fit), gmm::confint.gmm(fit)$test)[2,])

  # E[Y(0)]
  ey0ci <- cbind(gmm::coef.gmm(fit), gmm::confint.gmm(fit)$test)[1,]

  reslist <- list(fit = fit,
                  crrci = crrci,
                  ey0ci = ey0ci,
                  estmethod = "gmm")
  return(reslist)
}

msmmAltMoments <- function(theta, x){
  # extract variables from x
  Y <- x[,"y"]
  X <- x[,"x"]

  znames <- names(x)[!names(x) %in% c("x", "y")]
  Z <- x[znames]
  nZ <- ncol(Z)
  nZp1 <- nZ + 1

  # moments
  moments <- matrix(nrow = nrow(Z), ncol = nZp1, NA)
  moments[,1] <- (Y*exp(-theta[1] - X*theta[2]) - 1)
  for (i in 1:nZ) {
    j <- i + 1
    moments[,j] <- (Y*exp(-theta[1] - X*theta[2]) - 1)*Z[,i]
  }
  return(moments)
}

msmm_gmm_alt <- function(x, y, z) {
  dat = data.frame(x, y, z)

  # gmm fit
  fit <- gmm::gmm(msmmAltMoments, x = dat, t0 = c(0, 0), vcov = "iid")

  if (fit$algoInfo$convergence != 0)
    warning("The GMM fit has not converged, perhaps try different initial parameter values")

  # exponentiate estimates
  expests <- exp(cbind(gmm::coef.gmm(fit), gmm::confint.gmm(fit)$test))
  crrci <- expests[2,]
  ey0ci <- expests[1,]

  reslist <- list(fit = fit,
                  crrci = crrci,
                  ey0ci = ey0ci,
                  estmethod = "gmmalt")
}

#' Summarizing MSMM Fits
#'
#' @param object an object of class `"msmm"`.
#' @param x an object of class `"summary.msmm"`.
#' @param digits the number of significant digits to use when printing.
#' @param ... further arguments passed to or from other methods.
#'
#' S3 summary and print methods for objects of class `msmm` and `summary.msmm`.
#' @examples
#' # For examples see the examples at the bottom of help('msmm')
#' @export
summary.msmm <- function(object, ...) {

  if (object$estmethod %in% c("tsls", "tslsalt")) {
    requireNamespace("ivreg", quietly = TRUE)
    smry <- summary(object$fit)
  }

  if (object$estmethod %in% c("gmm", "gmmalt")) {
    smry <- gmm::summary.gmm(object$fit)
  }

  res <- list(smry = smry,
              object = object)

  class(res) <- append(class(res), "summary.msmm")
  return(res)
}

#' @rdname summary.msmm
#' @export
print.msmm <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\n")
  cat("Estimation method:", x$estmethod)
  cat("\n")

  if (x$estmethod %in% c("tsls", "tslsalt")) {
    requireNamespace("ivreg", quietly = TRUE)
    print(x$fit)
  }

  if (x$estmethod %in% c("gmm", "gmmalt")) {
    cat("\n")
    gmm::print.gmm(x$fit)
  }

  if (x$estmethod != "tslsalt") {
    cat("\nE[Y(0)] with 95% CI:\n")
    print(x$ey0ci, digits = digits, ...)
  }

  cat("\nCausal risk ratio with 95% CI:\n")
  print(x$crrci, digits = digits, ...)

  cat("\n")
  invisible(x)
}

#' @rdname summary.msmm
#' @export
print.summary.msmm <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\n")

  cat("Estimation method:", x$object$estmethod, "\n")
  if (x$object$estmethod %in% c("tsls", "tslsalt")) {
    cat("\nStage 1 summary:\n")
    print(summary(x$object$stage1))

    requireNamespace("ivreg", quietly = TRUE)
    cat("TSLS fit summary:\n")
    print(x$smry)
  }

  if (x$object$estmethod %in% c("gmm", "gmmalt")) {
    cat("\nGMM fit summary:\n")
    gmm::print.summary.gmm(x$smry)
  }

  if (x$object$estmethod != "tslsalt") {
    cat("\nE[Y(0)] with 95% CI:\n")
    print(x$object$ey0ci, digits = digits, ...)
  }

  cat("\nCausal risk ratio with 95% CI:\n")
  print(x$object$crrci, digits = digits, ...)

  cat("\n")
  invisible(x)
}
