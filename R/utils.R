# Solve the log-multiplicative second stage estimating equations,
# sum_i R_i (Y_i exp(-R_i b) - 1) = 0, by Newton-Raphson. Used to obtain
# starting values for the gmm fits of tsps() and tsri() with
# link = "logmult".
logmult_coef <- function(y, r, b, tol = 1e-12, maxit = 100) {
  for (i in seq_len(maxit)) {
    w <- as.vector(y * exp(-r %*% b))
    score <- crossprod(r, w - 1)
    # the Newton step solves H step = score, where the Hessian
    # H = r' diag(w) r is symmetric positive definite, so use its Cholesky
    # factorization
    ch <- chol(crossprod(r, w * r))
    step <- backsolve(ch, forwardsolve(ch, score, upper.tri = TRUE, transpose = TRUE))
    b <- b + as.vector(step)
    if (max(abs(step)) < tol) {
      break
    }
  }
  b
}

# Print the exponentiated causal effect estimate (causal risk ratio or causal
# odds ratio) of the exposure from a tsps() or tsri() fit. Only the exposure
# parameter is exponentiated, since the other second stage parameters (the
# intercept, the first stage residuals in tsri(), and any covariates) are not
# causal effects. The exposure parameter directly follows the second stage
# intercept.
print_causal_ratio <- function(estci, link, digits, ...) {
  if (!(link %in% c("logadd", "logmult", "logit"))) {
    return(invisible(NULL))
  }
  parname <- if (link == "logit") "Causal odds ratio" else "Causal risk ratio"
  xrow <- which(rownames(estci) == "(Intercept)") + 1
  cat("\n", parname, " with 95% CI limits:\n", sep = "")
  print(exp(estci[xrow, , drop = FALSE]), digits = digits, ...)
  invisible(NULL)
}

# Coefficient table with Wald z statistics and p-values, for printing with
# printCoefmat().
coef_table <- function(est, vcov) {
  se <- sqrt(diag(vcov))
  z <- est / se
  cbind(
    Estimate = est,
    `Std. Error` = se,
    `z value` = z,
    `Pr(>|z|)` = 2 * stats::pnorm(-abs(z))
  )
}

# Hansen J-test of the overidentifying restrictions of a gmm fit, or NULL if
# the model is exactly identified.
hansen_jtest <- function(fit) {
  if (fit$df == 0) {
    return(NULL)
  }
  j <- gmm::specTest(fit)$test
  c(J = as.numeric(j[1, 1]), df = fit$df, p = as.numeric(j[1, 2]))
}

print_jtest <- function(jtest, digits) {
  if (is.null(jtest)) {
    return(invisible(NULL))
  }
  cat(
    "\nHansen J-test of the overidentifying restrictions:\nJ = ",
    format(jtest[["J"]], digits = digits),
    " on ",
    jtest[["df"]],
    " degrees of freedom, p-value: ",
    format.pval(jtest[["p"]], digits = digits),
    "\n",
    sep = ""
  )
  invisible(NULL)
}

print_call <- function(call) {
  if (!is.null(call)) {
    cat("\nCall:\n")
    print(call)
  }
  invisible(NULL)
}
