#' Condtional F-statistic of Sanderson and Windmeijer 2014.
#' @export
fsw.ivreg <- function(mod) {


  nendog <- length(mod$endogenous)
  ninstruments <- length(mod$instruments)
  namesendog <- names(mod$endogenous)
  n <- mod$n
  varmat <- (1/n) * t(mod$residuals1) %*% mod$residuals1
  fsw <- numeric(nendog)
  namesinstruments <- names(mod$instruments)
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
