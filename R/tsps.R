#' Two-stage predictor substitution (TSPS) estimators
#'
#' TSPS estimators
#' `tsps` implements
#' @export
tsps <- function(formula, instruments, data, subset, na.action,
                 contrasts = NULL,
                 t0 = NULL,
                 ...) {
  # ivreg::ivreg() arguments I haven't implemented:
  # weights, offset,
  # model = TRUE, y = TRUE, x = FALSE,

  # code from beginning for ivreg::ivreg()
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

  xnames <- colnames(X)
  xnames <- xnames[-1]

  estmethod <- match.arg(estmethod, c("gmm", "gmmalt", "tsls", "tslsalt"))

  # check y binary
  if (!all(Y %in% 0:1))
    stop("For tsls and tslsalt, the outcome must be binary, i.e. take values 0 or 1.")



  class(output) <- append("tsps", class(output))
  output
}
