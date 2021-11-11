#' Conditional F-statistic of Sanderson and Windmeijer (2016)
#'
#' `fsw` calculates the conditional F-statistic of
#' Sanderson and Windmeijer (2016) for each endogenous variable
#' in the model.
#' @param object An object of class `"ivreg"` containing the results of
#' an IV model fitted by [`ivreg::ivreg`] for which to calculate
#' the conditional F-statistics for each endogenous variable.
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
#' set.seed(12345)
#' n <- 4000
#' z1 <- rnorm(n)
#' z2 <- rnorm(n)
#' w1 <- rnorm(n)
#' w2 <- rnorm(n)
#' u <- rnorm(n)
#' x1 <- z1 + z2 + 0.2*u + 0.1*w1 + rnorm(n)
#' x2 <- z1 + 0.94*z2 - 0.3*u + 0.1*w2 + rnorm(n)
#' y <- x1 + x2 + w1 + w2 + u
#' dat <- data.frame(w1, w2, x1, x2, y, z1, z2)
#' mod <- ivreg::ivreg(y ~ x1 + x2 + w1 + w2 | z1 + z2 + w1 + w2, data = dat)
#' fsw(mod)
#'
#' @references
#' Sanderson E and Windmeijer F. A weak instrument *F*-test in linear
#' IV models with multiple endogenous variables. Journal of Econometrics,
#' 2016, 190, 2, 212-221, \doi{10.1016/j.jeconom.2015.06.004}.
#' @export
fsw <- function(object) UseMethod("fsw", object)

#' @rdname fsw
#' @importFrom stats as.formula lm pf
#' @importFrom ivreg ivreg
#' @importFrom lmtest waldtest
#' @export
fsw.ivreg <- function(object) {

  if (is.null(object$model)) stop("Please re-run your ivreg() model with the option model=TRUE")

  nendog <- length(object$endogenous)
  if (nendog < 2) stop("The number of exposures must be 2 or more.")

  ninstruments <- length(object$instruments)
  nexogenous <- length(object$exogenous) - 1
  namesendog <- names(object$endogenous)
  namesexog <- names(object$exogenous[-1])
  namesinstruments <- names(object$instruments)
  n <- object$n
  fsw <- fswdf <- fswresdf <- fswp <- numeric(nendog)
  names(fsw) <- names(fswp) <- namesendog
  instrplus <- paste(namesinstruments, collapse = " + ")

  for (i in 1:nendog) {

    endogoutcome <- namesendog[i]
    endogothers <- namesendog[-i]

    if (nexogenous > 0) {
      exogplus <- paste(namesexog, collapse = " + ")
      modelstr <- paste(endogoutcome,
                        "~",
                        paste(endogothers, collapse = " + "),
                        "+",
                        exogplus,
                        "|",
                        instrplus, "+", exogplus)
    } else {
      modelstr <- paste(endogoutcome, "~",
                        paste(endogothers, collapse = " + "),
                        "|", instrplus)
    }
    modelfor <- as.formula(modelstr)
    condmod <- try(ivreg::ivreg(modelfor, data = object$model), silent = TRUE)
    condmoderrmsg = paste("The IV regression of one of the exposures",
                          "on the other/s has failed.",
                          "This is most likely because you have a transformation",
                          "on one or more of the exposure or instrumental variables.",
                          "Please create the transformed variable/s in your",
                          "data.frame and refit,",
                          "e.g. instead of creating your ivreg object from",
                          "ivreg(y ~ log(x1) + x2 | z1 + z2 + z3)",
                          "please create dat$logx1 = log(x1) in your data.frame",
                          "and fit ivreg(y ~ logx1 + x2 | z1 + z2 + z3).")
    if (class(condmod) == "try-error") stop(condmoderrmsg)
    condres <- condmod$residuals
    if (nexogenous > 0) {
      resfor <- as.formula(paste("condres", "~", instrplus, "+", exogplus))
    } else {
      resfor <- as.formula(paste("condres", "~", instrplus))
    }
    resmod <- lm(resfor, data = object$model)
    # summary(resmod)
    if (nexogenous > 0) {
      resbasefor <- as.formula(paste("condres ~ 1 +", exogplus))
    } else {
      resbasefor <- as.formula(paste("condres ~ 1"))
    }
    resbase <- lm(resbasefor, data = object$model)
    # summary(resbase)
    wldtst <- lmtest::waldtest(resbase, resmod)
    # wldtst
    fsw[i] <- (wldtst$F[2L] * wldtst$Df[2L]) / (wldtst$Df[2L] - (nendog - 1))
    fswdf[i] <- wldtst$Df[2L] - (nendog - 1)
    fswresdf[i] <- wldtst$Res.Df[2L]
    fswp[i] <- pf(fsw[i], nendog, wldtst$Res.Df[2L], lower.tail= FALSE)
  }

  fswres = cbind(fsw, fswdf, fswresdf, fswp)
  rownames(fswres) = namesendog
  colnames(fswres) = c("F value","d.f.","Residual d.f.","Pr(>F)")

  output <- list(fswres = fswres,
                 namesendog = namesendog,
                 nendog = nendog,
                 n = n)
  class(output) <- append("fsw", class(output))
  return(output)
}

#' @importFrom stats printCoefmat
#' @export
print.fsw <- function(x, digits = getOption("digits"), ...) {
  cat("\nModel sample size: ", x$n, "\n")
  cat("\nSanderson-Windmeijer conditional F-statistics for first stage model:\n")
  printCoefmat(x$fswres,
               cs.ind = 2L:3L,
               tst.ind = 1L,
               has.Pvalue = TRUE,
               P.values = TRUE,
               digits = digits,
               ...)
  cat("\n")
  invisible(x)
}
