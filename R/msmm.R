#' Multiplicative structural mean model
#'
#' Function providing several methods to estimate the multiplicative
#' structural mean model (MSMM) of Robins (1994).
#'
#' @param formula The model formula
#' @param estmethod Estimation method, these are
#'
#'    * "`gmm`"
#'    * `"tsls"` the TSLS method of fitting MSMM Clarke et al. (2015)
#'    * `"tslsalt"` the alternative TSLS method of Clarke et al. (2015)
#' @references Clarke PS, Palmer TM Windmeijer F. Estimating structural
#' mean models with multiple instrumental variables using the
#' Generalised Method of Moments. Statistical Science, 2015, 30, 1,
#' 96-117, \doi{10.1214/14-STS503}.
#' @export
msmm <- function(formula, estmethod = "gmm") {

  if (estmethod == "gmm") msmm_gmm(formula)
  if (estmethod == "tsls") msmm_tsls(formula)
  if (estmethod == "tslsalt") msmm_tsls_alt(formula)

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
