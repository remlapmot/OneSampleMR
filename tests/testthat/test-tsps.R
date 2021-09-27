# tests for TSPS

# Data generation from the example in the ivtools::ivglm() helpfile
set.seed(9)
n <- 1000
psi0 <- 0.5
Z <- rbinom(n, 1, 0.5)
X <- rbinom(n, 1, 0.7*Z + 0.2*(1 - Z))
m0 <- plogis(1 + 0.8*X - 0.39*Z)
Y <- rbinom(n, 1, plogis(psi0*X + log(m0/(1 - m0))))
dat <- data.frame(Z, X, Y)

test_that("Single instrument example - identity link", {
  # ivtools for comparison fit
  library(ivtools)
  fitZ.L <- glm(Z ~ 1, data = dat)
  fitY.LZX <- glm(Y ~ X + Z, family = binomial(link = "identity"), data = dat)
  fitIdentGest <- ivglm(estmethod = "g",
                      X = "X",
                      fitZ.L = fitZ.L,
                      fitY.LZX = fitY.LZX,
                      data = dat,
                      link = "identity",
                      Y = "Y")
  crd <- fitIdentGest$est["X"]
  crdse <- sqrt(fitIdentGest$vcov)

  fit01 <- tsps(Y ~ X | Z, data = dat, link = "identity")
  expect_equal(log(fit01$estci[1]), crd, tolerance = 0.05, ignore_attr = "names")

  expect_s3_class(fit01, "tsps")

  smy01 <- summary(fit01)
  expect_s3_class(smy01, "summary.tsps")

  expect_output(print(fit01))
  expect_output(print(smy01))
})

test_that("Single instrument example - log link", {
  # ivtools for comparison fit
  library(ivtools)
  fitZ.L <- glm(Z ~ 1, data = dat)
  fitY.LZX <- glm(Y ~ X + Z, family = binomial(link = "log"), data = dat)
  fitLogGest <- ivglm(estmethod = "g",
                      X = "X",
                      fitZ.L = fitZ.L,
                      fitY.LZX = fitY.LZX,
                      data = dat,
                      link = "log",
                      Y = "Y")
  logcrr <- fitLogGest$est["X"]
  logcrrse <- sqrt(fitLogGest$vcov)

  fit11 <- tsps(Y ~ X | Z, data = dat, link = "log")
  expect_equal(log(fit11$estci[1]), logcrr, tolerance = 0.05, ignore_attr = "names")

  expect_s3_class(fit11, "tsps")

  smy11 <- summary(fit11)
  expect_s3_class(smy, "summary.tsps")

  expect_output(print(fit11))
  expect_output(print(smy11))
})

test_that("Single instrument example - logit link", {
  # ivtools for comparison fit
  library(ivtools)
  fitZ.L <- glm(Z ~ 1, data = dat)
  fitY.LZX <- glm(Y ~ X + Z, family = binomial(link = "logit"), data = dat)
  fitLogitGest <- ivglm(estmethod = "g",
                      X = "X",
                      fitZ.L = fitZ.L,
                      fitY.LZX = fitY.LZX,
                      data = dat,
                      link = "logit",
                      Y = "Y")
  logcor <- fitLogitGest$est["X"]
  logcorse <- sqrt(fitLogitGest$vcov)

  fit21 <- tsps(Y ~ X | Z, data = dat, link = "logit")
  expect_equal(log(fit21$estci[1]), logcor, tolerance = 0.05, ignore_attr = "names")

  expect_s3_class(fit21, "tsps")

  smy21 <- summary(fit21)
  expect_s3_class(smy21, "summary.tsps")

  expect_output(print(fit21))
  expect_output(print(smy))
})
