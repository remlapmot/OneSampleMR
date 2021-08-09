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
#' @examples
#' # Data generation from the example in the [`ivtools::ivglm()`] helpfile
#' set.seed(9)
#' n <- 1000
#' psi0 <- 0.5
#' psi1 <- 0.2
#' Z <- rbinom(n, 1, 0.5)
#' X <- rbinom(n, 1, 0.7*Z + 0.2*(1 - Z))
#' m0 <- plogis(1 + 0.8*X - 0.39*Z)
#' Y <- rbinom(n, 1, plogis(psi0*X + log(m0/(1 - m0))))
#' dat <- data.frame(Z, X, Y)
#' msmm(Y ~ X | Z, data = dat, estmethod = "tsls")
#' @export
msmm <- function(formula, instruments, data, subset, na.action, weights, offset,
                 contrasts = NULL, model = TRUE, y = TRUE, x = FALSE,
                 estmethod = c("gmm", "gmmalt", "tsls", "tslsalt"),
                 ...) {

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
  weights <- model.weights(mf)
  offset <- model.offset(mf)
  if(is.null(offset)) offset <- 0
  if(length(offset) == 1) offset <- rep(offset, NROW(Y))
  offset <- as.vector(offset)
  # end of code from ivreg::ivreg()

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
