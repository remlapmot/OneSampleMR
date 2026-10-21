# Solve the log-multiplicative second stage estimating equations,
# sum_i R_i (Y_i exp(-R_i b) - 1) = 0, by Newton-Raphson. Used to obtain
# starting values for the gmm fits of tsps() and tsri() with
# link = "logmult".
logmult_coef <- function(y, r, b, tol = 1e-12, maxit = 100) {
  for (i in seq_len(maxit)) {
    w <- as.vector(y * exp(-r %*% b))
    score <- crossprod(r, w - 1)
    step <- solve(crossprod(r, w * r), score)
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
