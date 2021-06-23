#' Calculate the conditional F-statistic of Sanderson and Windmeijer (2016)
#' @param mod A fitted model from `ivreg::ivreg()`, i.e. an object of class ivreg.
#' @return An object of class fsw with the following elements:
#'
#' - fsw vector of conditional *F*-statistics
#'
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
#' @importFrom stats as.formula lm
#' @export
fsw.ivreg <- function(mod) {

  if (is.null(mod$model)) stop("Please re-run your ivreg() model with the option model==TRUE")

  nendog <- length(mod$endogenous)
  ninstruments <- length(mod$instruments)
  nexogenous <- length(mod$exogenous) - 1
  namesendog <- names(mod$endogenous)
  namesexog <- names(mod$exogenous[-1])
  namesinstruments <- names(mod$instruments)
  n <- mod$n
  fsw <- numeric(nendog)
  names(fsw) <- namesendog
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
    condmod <- ivreg::ivreg(modelfor, data = mod$model)
    condres <- condmod$residuals
    if (nexogenous > 0) {
      resfor <- as.formula(paste("condres", "~", instrplus, "+", exogplus))
    } else {
      resfor <- as.formula(paste("condres", "~", instrplus))
    }
    resmod <- lm(resfor, data = mod$model)
    # summary(resmod)
    if (nexogenous > 0) {
      resbasefor <- as.formula(paste("condres ~ 1 +", exogplus))
    } else {
      resbasefor <- as.formula(paste("condres ~ 1"))
    }
    resbase <- lm(resbasefor, data = mod$model)
    # summary(resbase)
    wldtst <- lmtest::waldtest(resbase, resmod)
    # wldtst
    fsw[i] <- (wldtst$F[2] * wldtst$Df[2]) / (wldtst$Df[2] - (nendog - 1))
  }

  output <- list(fsw = fsw)
  class(output) <- append("fsw.ivreg", class(output))
  return(output)
}

#' @export
print.fsw.ivreg <- function(x, digits = getOption("digits"), ...) {
  print(x, digits = digits, ...)
  invisible(x)
}
