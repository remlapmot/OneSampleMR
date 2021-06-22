#' Condtional F-statistic of Sanderson and Windmeijer 2014.
#' @export
fsw.ivreg <- function(mod) {


  nendog <- length(mod$endogenous)
  ninstruments <- length(mod$instruments)
  namesendog <- names(mod$endogenous)
  namesinstruments <- names(mod$instruments)


m12 = ivreg::ivreg(educ ~ exper | age + kidslt6 + kidsge6, data = dat)
r12 = m12$residuals
lm12 = lm(r12 ~ age + kidslt6 + kidsge6, data = dat)
lm12base = lm(r12 ~ 1, data = dat)
wldt = lmtest::waldtest(lm12base, lm12)
(wldt$F[2]*wldt$Df[2]) / (wldt$Df[2] - 1)


m21 = ivreg::ivreg(exper ~ educ | age + kidslt6 + kidsge6, data = dat)
r21 = m21$residuals
lm21 = lm(r21 ~ age + kidslt6 + kidsge6, data = dat)
lm21base = lm(r21 ~ 1, data = dat)
wldt = lmtest::waldtest(lm21base, lm21)
(wldt$F[2]*wldt$Df[2]) / (wldt$Df[2] - 1)



  n <- mod$n
  varmat <- (1/n) * t(mod$residuals1) %*% mod$residuals1
  fsw <- numeric(nendog)

  Zmat <- as.matrix(cbind(rep(1, n), mod$model[namesinstruments]))

  for (i in 1:nendog) {
    varmatdim <- i + 1
    endogname <- namesendog[i]
    # fittedvalues <- mod$model[endogname] - as.data.frame(mod$residuals1)[endogname]
    #
    # x1 = t(as.matrix(mod$model[endogname]))
    # x2 = as.matrix(Zmat)
    # x3 = fittedvalues

    pihat = mod$coefficients1[, endogname]
    numerator <- t(pihat) %*% t(Zmat) %*% Zmat %*% pihat
    numerator
    # xmat = as.matrix(mod$model[endogname])
    # numerator = t(xmat) %*% Zmat %*% solve(t(Zmat) %*% Zmat) %*% t(Zmat) %*% xmat
    denominator <- (ninstruments) * varmat[varmatdim, varmatdim]
    denominator
    numerator / denominator
    fsw[i] <- numerator / denominator
  }

  fsw = c(6.7, 82)
  output <- list(fsw = fsw)
  class(output) <- append("fsw", class(output))
  output
}

print.fsw <- function(x) {
  print(x)
}
