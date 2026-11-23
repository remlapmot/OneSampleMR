#' Estimation within Quantiles of the First Stage Residuals
#'
#' `ivxtile()` fits an IV estimator within quantile groups of the first stage
#' residuals (equivalently of the IV-free exposure), to explore whether the
#' causal effect varies with the level of the exposure, as described by
#' Burgess et al. (2014). It is the equivalent of the `ivxtile` prefix
#' command in the Stata package ivonesamplemr.
#'
#' The first stage linear regression of the exposure on the instruments and
#' any covariates is fitted in the full data. The observations are then
#' divided into `nq` groups by the quantiles of its residuals, defined in the
#' same way as by Stata's `xtile` command, and the estimator is fitted within
#' each group.
#'
#' The approach relies on an additional assumption, that the effect of the
#' instruments on the exposure is the same in each group (the constant
#' genetic effect assumption). When it is violated the estimates within the
#' groups can be biased (Burgess, 2023).
#'
#' @param formula,instruments formula specification(s) of the model, as for
#' the estimator, with a single exposure, e.g. `y ~ x + w | z1 + z2 + w`.
#' @param data a data frame containing the variables in the model.
#' @param estimator the function used to fit the model within each group,
#' e.g. [ivreg::ivreg()], [msmm()], [lsmm()], [tsps()], or [tsri()]. It must
#' accept `formula` and `data` arguments and return an object with [coef()]
#' and [vcov()] methods.
#' @param nq the number of quantile groups.
#' @param par the name of the parameter to collect from each fit. By default
#' the parameter of the exposure, which is the causal effect on the scale on
#' which it is estimated, for example the log causal risk ratio for
#' [msmm()].
#' @param ... further arguments passed to `estimator`, e.g. `link` for
#' [tsps()] and [tsri()] or `estmethod` for [msmm()].
#' @return An object of class `"ivxtile"`, a data frame with one row for
#' each quantile group and the columns `quantile`, `n` (the number of
#' observations), `beta` (the estimate of `par`), `se` (its standard error),
#' `medexposure` (the median of the exposure), and `lowci` and `uppci` (the
#' limits of the 95% confidence interval, `beta` plus and minus 1.96 times
#' `se`). If the estimator fails in a group its estimates are `NA`. The
#' object has `print()` and `plot()` methods.
#' @references
#' Burgess S, Davies NM, Thompson SG. Instrumental variable analysis with a
#' nonlinear exposure-outcome relationship. Epidemiology, 2014, 25, 6,
#' 877-885. \doi{10.1097/EDE.0000000000000161}
#'
#' Burgess S. Violation of the constant genetic effect assumption can
#' result in biased estimates for non-linear Mendelian randomization. Human
#' Heredity, 2023, 88, 1, 79-90. \doi{10.1159/000531659}
#' @seealso [ivmw()]
#' @examples
#' set.seed(12345)
#' n   <- 5000
#' z   <- rbinom(n, 2, 0.3)
#' u   <- rnorm(n)
#' x   <- 0.5 * z + u + rnorm(n)
#' y   <- 0.5 * x + 0.2 * x^2 + u + rnorm(n)
#' dat <- data.frame(y, x, z)
#' res <- ivxtile(y ~ x | z, data = dat, nq = 5)
#' res
#' plot(res)
#' @export
ivxtile <- function(
  formula,
  instruments,
  data,
  estimator = ivreg::ivreg,
  nq = 5,
  par = NULL,
  ...
) {
  setup <- nonlinear_setup(formula, instruments, data, par)
  group <- xtile(setup$res, nq)

  out <- lapply(seq_len(nq), function(q) {
    rows <- which(group == q)
    nonlinear_fit(setup, rows, estimator, ...)
  })
  out <- data.frame(quantile = seq_len(nq), do.call(rbind, out))
  structure(
    out,
    class = c("ivxtile", "data.frame"),
    par = setup$par,
    exposure = setup$exposure
  )
}

#' Moving Window Estimation
#'
#' `ivmw()` fits an IV estimator in moving (sliding) windows of consecutive
#' observations ordered by the first stage residuals (equivalently by the
#' IV-free exposure), to explore whether the causal effect varies with the
#' level of the exposure, as described by Burgess et al. (2014). It is the
#' equivalent of the `ivmw` prefix command in the Stata package
#' ivonesamplemr.
#'
#' The first stage linear regression of the exposure on the instruments and
#' any covariates is fitted in the full data, and the observations are
#' ordered by its residuals. The estimator is then fitted in windows of
#' `window` consecutive observations, starting at observations 1, 1 +
#' `step`, 1 + 2 `step`, and so on. With the default `step = 1` there are
#' `n - window + 1` windows, as for Stata's `ivmw` command.
#'
#' The approach relies on the same additional assumption as [ivxtile()],
#' that the effect of the instruments on the exposure is the same in each
#' window (Burgess, 2023).
#'
#' @inheritParams ivxtile
#' @param window the number of consecutive observations in each window.
#' @param step the number of observations by which successive windows are
#' moved.
#' @return An object of class `"ivmw"`, a data frame with one row for each
#' window and the columns `start` and `end` (the first and last observation
#' of the window in the ordering by the first stage residuals), `n`, `beta`,
#' `se`, `medexposure`, `lowci`, and `uppci`, as for [ivxtile()]. The
#' object has `print()` and `plot()` methods.
#' @inherit ivxtile references
#' @seealso [ivxtile()]
#' @examples
#' set.seed(12345)
#' n   <- 5000
#' z   <- rbinom(n, 2, 0.3)
#' u   <- rnorm(n)
#' x   <- 0.5 * z + u + rnorm(n)
#' y   <- 0.5 * x + 0.2 * x^2 + u + rnorm(n)
#' dat <- data.frame(y, x, z)
#' res <- ivmw(y ~ x | z, data = dat, window = 2500, step = 250)
#' res
#' plot(res)
#' @export
ivmw <- function(
  formula,
  instruments,
  data,
  estimator = ivreg::ivreg,
  window,
  step = 1,
  par = NULL,
  ...
) {
  setup <- nonlinear_setup(formula, instruments, data, par)
  n <- length(setup$res)
  if (window > n) {
    stop("window must not be larger than the number of observations, ", n, ".")
  }
  ord <- order(setup$res)
  starts <- seq(1, n - window + 1, by = step)

  out <- lapply(starts, function(s) {
    rows <- ord[s:(s + window - 1)]
    nonlinear_fit(setup, rows, estimator, ...)
  })
  out <- data.frame(
    start = starts,
    end = starts + window - 1,
    do.call(rbind, out)
  )
  structure(
    out,
    class = c("ivmw", "data.frame"),
    par = setup$par,
    exposure = setup$exposure
  )
}

# Quantile groups as defined by Stata's xtile command: the cut points are
# the order statistics x_(ceiling(n i / nq)), or the average of x_(n i / nq)
# and x_(n i / nq + 1) when n i / nq is an integer, for i = 1, ..., nq - 1,
# and observations equal to a cut point are in the lower group. Integer
# arithmetic is used so that whether n i / nq is an integer is exact.
xtile <- function(x, nq) {
  n <- length(x)
  xs <- sort(x)
  cuts <- vapply(
    seq_len(nq - 1),
    function(i) {
      if ((n * i) %% nq == 0) {
        j <- (n * i) %/% nq
        (xs[j] + xs[j + 1]) / 2
      } else {
        xs[ceiling(n * i / nq)]
      }
    },
    numeric(1)
  )
  findInterval(x, cuts, left.open = TRUE) + 1
}

# Fit the first stage regression and identify the exposure and parameter
# for ivxtile() and ivmw().
nonlinear_setup <- function(formula, instruments, data, par) {
  if (!missing(instruments)) {
    formula <- Formula::as.Formula(formula, instruments)
  } else {
    formula <- Formula::as.Formula(formula)
  }
  if (length(formula)[2L] != 2L) {
    stop(
      "formula must have two parts on the right hand side, ",
      "e.g. y ~ x + w | z1 + z2 + w."
    )
  }
  # complete cases of the model variables
  mf <- stats::model.frame(formula, data = data)
  data <- data[rownames(mf), , drop = FALSE]
  X <- stats::model.matrix(formula, data = mf, rhs = 1)
  Z <- stats::model.matrix(formula, data = mf, rhs = 2)
  xnames <- colnames(X)[-1]
  znames <- colnames(Z)[-1]
  covariatenames <- intersect(xnames, znames)
  exposure <- setdiff(xnames, covariatenames)
  if (length(exposure) != 1) {
    stop("Only 1 exposure variable is allowed.")
  }
  if (is.null(par)) {
    par <- exposure
  }

  stage1 <- stats::.lm.fit(Z, X[, exposure])
  list(
    formula = formula,
    data = data,
    exposure = exposure,
    exposurevalues = X[, exposure],
    par = par,
    res = stage1$residuals
  )
}

# Fit the estimator in a subset of the observations and collect the
# estimate of the parameter, its standard error, and the median exposure.
nonlinear_fit <- function(setup, rows, estimator, ...) {
  beta <- NA_real_
  se <- NA_real_
  fit <- tryCatch(
    estimator(setup$formula, data = setup$data[rows, , drop = FALSE], ...),
    error = function(e) NULL
  )
  if (!is.null(fit)) {
    est <- stats::coef(fit)
    if (!(setup$par %in% names(est))) {
      stop(
        "par = \"",
        setup$par,
        "\" is not one of the parameters of the fitted model: ",
        paste(names(est), collapse = ", "),
        "."
      )
    }
    beta <- unname(est[setup$par])
    se <- sqrt(unname(diag(stats::vcov(fit))[setup$par]))
  }
  data.frame(
    n = length(rows),
    beta = beta,
    se = se,
    medexposure = stats::median(setup$exposurevalues[rows]),
    lowci = beta - 1.96 * se,
    uppci = beta + 1.96 * se
  )
}

#' @rdname ivxtile
#' @param x an object of class `"ivxtile"` or `"ivmw"`.
#' @param digits the number of significant digits to use when printing.
#' @export
print.ivxtile <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat(
    "\nEstimates of", attr(x, "par"),
    "within quantiles of the first stage residuals:\n\n"
  )
  print(as.data.frame(x), digits = digits, row.names = FALSE, ...)
  cat("\n")
  invisible(x)
}

#' @rdname ivmw
#' @param x an object of class `"ivmw"`.
#' @param digits the number of significant digits to use when printing.
#' @export
print.ivmw <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat(
    "\nEstimates of", attr(x, "par"),
    "in moving windows of the first stage residuals:\n\n"
  )
  print(as.data.frame(x), digits = digits, row.names = FALSE, ...)
  cat("\n")
  invisible(x)
}

#' @rdname ivxtile
#' @param xlab,ylab axis labels.
#' @param ylim the y-axis limits. By default the range of the confidence
#' limits, extended upwards to leave space for the legend.
#' @param legend the position of the legend, as for [graphics::legend()], or
#' `NULL` for no legend.
#' @export
plot.ivxtile <- function(
  x,
  xlab = "Median of exposure in quantile",
  ylab = "Estimated causal effect",
  ylim = NULL,
  legend = "topleft",
  ...
) {
  nonlinear_plot(x, xlab = xlab, ylab = ylab, ylim = ylim, legend = legend, ...)
}

#' @rdname ivmw
#' @inheritParams plot.ivxtile
#' @export
plot.ivmw <- function(
  x,
  xlab = "Median of exposure in moving window",
  ylab = "Estimated causal effect",
  ylim = NULL,
  legend = "topleft",
  ...
) {
  nonlinear_plot(x, xlab = xlab, ylab = ylab, ylim = ylim, legend = legend, ...)
}

nonlinear_plot <- function(x, xlab, ylab, ylim, legend, ...) {
  x <- x[order(x$medexposure), ]
  if (is.null(ylim)) {
    ylim <- range(c(x$lowci, x$uppci), na.rm = TRUE)
    # leave space above the lines for the legend
    if (!is.null(legend)) {
      ylim[2] <- ylim[2] + 0.25 * diff(ylim)
    }
  }
  graphics::plot(
    x$medexposure,
    x$beta,
    type = "l",
    lwd = 2,
    ylim = ylim,
    xlab = xlab,
    ylab = ylab,
    ...
  )
  graphics::lines(x$medexposure, x$lowci, col = "grey60", lwd = 2)
  graphics::lines(x$medexposure, x$uppci, col = "grey60", lwd = 2)
  if (!is.null(legend)) {
    graphics::legend(
      legend,
      legend = c("Estimated causal effect", "95% CI limits"),
      col = c("black", "grey60"),
      lwd = 2,
      bty = "n"
    )
  }
  invisible(x)
}
