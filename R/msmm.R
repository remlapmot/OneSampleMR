#' Multiplicative structural mean model
#'
#' Function providing several methods to estimate the multiplicative
#' structural mean model (MSMM) of Robins (1994).
#'
#' @param formula The model formula
#' @param estmethod Estimation method, these are
#'
#'    * `"gmm"` GMM estimation of the MSMM
#'    * `"gmmalt"` GMM estimation of the alternative moment conditions
#'    for the MSMM as per Clarke et al. (2015)
#'    * `"tsls"` the TSLS method of fitting MSMM Clarke et al. (2015)
#'    * `"tslsalt"` the alternative TSLS method of Clarke et al. (2015)
#' @references Clarke PS, Palmer TM Windmeijer F. Estimating structural
#' mean models with multiple instrumental variables using the
#' Generalised Method of Moments. Statistical Science, 2015, 30, 1,
#' 96-117. \doi{10.1214/14-STS503}
#'
#' Hernan and Robins. Instruments for causal inference: An
#' epidemiologist's dream? Epidemiology, 2006, 17, 360-372.
#' \doi{10.1097/01.ede.0000222409.00878.37}
#'
#' Robins JM. ROBINS, J. M. (1989). The analysis of randomised and
#' nonrandomised AIDS treatment trials using a new approach to
#' causal inference in longitudinal studies.
#' In Health Service Research Methodology: A Focus on AIDS
#' (L. Sechrest, H. Freeman and A. Mulley, eds.) 113–159.
#' US Public Health Service, National Center for Health Services Research,
#' Washington, DC.
#' @export
msmm <- function(formula, estmethod = "gmm", data, subset, ...) {

  # From Formula package vignette
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
  mf <- mf[c(1, m)]

  f <- Formula::Formula(formula)
  mf[[1]] <- as.name("model.frame")
  mf$formula <- f
  mf <- eval(mf, parent.frame())

  y <- model.response(mf)
  x <- model.matrix(f, data = mf, rhs = 1)
  z <- model.matrix(f, data = mf, rhs = 2)

  estmethod <- match.arg(estmethod, c("gmm", "gmmalt", "tsls", "tslsalt"))
  if (estmethod == "gmm") output = msmm_gmm(formula)
  if (estmethod == "gmmalt") output = msmm_gmm_alt(formula)
  if (estmethod == "tsls") output = msmm_tsls(formula)
  if (estmethod == "tslsalt") output = msmm_tsls_alt(formula)

  class(output) <- append("msmm", class(output))
  output
}

msmm_tsls <- function(formula) {
  outcome <- y * (x - 1)
  exposure <- y * x
  ivreg::ivreg(outcome ~ exposure | instruments)
}

msmm_tsls_alt <- function() {
  outcome <- y * x
  exposure <- y * (x - 1)
  ivreg::ivreg(outcome ~ exposure | instruments)
}

msmm_gmm <- function(formula){

}

msmm_gmm_alt <- function(formula) {

}
