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
