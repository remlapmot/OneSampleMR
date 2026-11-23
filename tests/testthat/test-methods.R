# tests for the coef(), vcov(), confint(), nobs(), and formula() methods

set.seed(123456)
n <- 1000
G1 <- rbinom(n, 2, 0.5)
G2 <- rbinom(n, 2, 0.3)
G3 <- rbinom(n, 2, 0.4)
U <- runif(n)
C <- runif(n)
X <- rbinom(n, 1, plogis(0.7 * G1 + G2 - G3 + U + C))
Y <- rbinom(n, 1, plogis(-2 + 0.5 * X + U + C))
dat <- data.frame(G1, G2, G3, X, Y, C)
f <- Y ~ X + C | G1 + G2 + G3 + C

fits <- list(
  msmm = msmm(f, data = dat),
  msmmalt = msmm(f, data = dat, estmethod = "gmmalt"),
  lsmm = lsmm(f, data = dat),
  tsps = tsps(f, data = dat, link = "logadd"),
  tsri = tsri(f, data = dat, link = "logit")
)

test_that("Methods are consistent with the underlying fits", {
  skip_on_cran()
  for (fit in fits) {
    est <- coef(fit)
    v <- vcov(fit)
    expect_type(est, "double")
    expect_identical(names(est), rownames(v))
    expect_identical(names(est), colnames(v))
    expect_equal(nobs(fit), n)
    expect_equal(formula(fit), f)

    ci <- confint(fit)
    expect_identical(rownames(ci), names(est))
    expect_equal(
      unname(ci[, 1]),
      unname(est - stats::qnorm(0.975) * sqrt(diag(v)))
    )
    expect_identical(rownames(confint(fit, "X")), "X")
    expect_true(all(confint(fit, level = 0.9)[, 1] > ci[, 1]))

    smry <- summary(fit)
    expect_equal(unname(smry$coefficients[, 1]), unname(est))
    expect_equal(unname(smry$coefficients[, 2]), unname(sqrt(diag(v))))
    expect_output(print(smry), "Call:")
    expect_output(print(smry), "Pr\\(>\\|z\\|\\)")
  }
})

test_that("Parameter labels", {
  skip_on_cran()
  expect_identical(names(coef(fits$msmm)), c("E[Y(0)]", "X", "C"))
  expect_identical(names(coef(fits$msmmalt)), c("log E[Y(0)]", "X", "C"))
  expect_equal(log(fits$msmm$crrci["X", 1]), coef(fits$msmm)[["X"]])
  expect_equal(log(fits$lsmm$corci["X", 1]), coef(fits$lsmm)[["X"]])
})

test_that("Hansen J-test is reported only for overidentified models", {
  skip_on_cran()
  expect_named(summary(fits$msmm)$jtest, c("J", "df", "p"))
  expect_equal(summary(fits$msmm)$jtest[["df"]], 2)
  expect_null(summary(msmm(Y ~ X | G1, data = dat))$jtest)
  expect_no_match(
    capture.output(print(summary(msmm(Y ~ X | G1, data = dat)))),
    "Hansen"
  )
  expect_null(summary(fits$tsps)$jtest)
})

test_that("Methods work for the tsls methods of msmm()", {
  skip_on_cran()
  fit <- msmm(Y ~ X | G1 + G2 + G3, data = dat, estmethod = "tsls")
  expect_identical(names(coef(fit)), c("(Intercept)", "exposure"))
  expect_equal(nobs(fit), n)
  expect_identical(dimnames(fit$crrci), list("X", c("CRR", "0.025", "0.975")))
  expect_output(print(summary(fit)), "two-stage least squares")
})

test_that("formula() returns a two-part formula when instruments are given separately", {
  skip_on_cran()
  fit <- tsps(Y ~ X, ~ G1 + G2 + G3, data = dat)
  expect_s3_class(formula(fit), "Formula")
})
