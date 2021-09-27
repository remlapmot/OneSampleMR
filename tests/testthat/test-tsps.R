# tests for TSPS

test_that("Single instrument example - identity link", {
  # Data generation from the example in the ivtools::ivglm() helpfile
  set.seed(9)
  n <- 1000
  psi0 <- 0.5
  Z <- rbinom(n, 1, 0.5)
  X <- rbinom(n, 1, 0.7*Z + 0.2*(1 - Z))
  m0 <- plogis(1 + 0.8*X - 0.39*Z)
  Y <- rbinom(n, 1, plogis(psi0*X + log(m0/(1 - m0))))
  dat <- data.frame(Z, X, Y)

  # ivtools for comparison fit
  library(ivtools)
  fitZ.L <- glm(Z ~ 1, data = dat)
  fitY.LZX <- glm(Y ~ X + Z, family = binomial(link = "identity"), data = dat)
  fitLogGest <- ivglm(estmethod = "g",
                      X = "X",
                      fitZ.L = fitZ.L,
                      fitY.LZX = fitY.LZX,
                      data = dat,
                      link = "identity",
                      Y = "Y")
  logcrd <- fitLogGest$est["X"]
  logcrdse <- sqrt(fitLogGest$vcov)

  fit01 <- tsps(Y ~ X | Z, data = dat, link = "identity")
  expect_equal(log(fit01$estci[1]), logcrd, tolerance = 0.05, ignore_attr = "names")

  expect_s3_class(fit01, "tsps")

  smy <- summary(fit01)
  expect_s3_class(smy, "summary.tsps")

  expect_output(print(fit01))
  expect_output(print(smy))
})
