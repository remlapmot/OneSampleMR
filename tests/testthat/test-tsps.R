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
  skip_on_cran()

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
  expect_equal(fit01$estci[4,1], crd, ignore_attr = "names")

  expect_s3_class(fit01, "tsps")

  smy01 <- summary(fit01)
  expect_s3_class(smy01, "summary.tsps")

  expect_output(print(fit01))
  expect_output(print(smy01))
})

# test_that("Single instrument example - logadd link", {
#   skip_on_cran()
#   # ivtools for comparison fit
#   library(ivtools)
#   fitZ.L <- glm(Z ~ 1, data = dat)
#   fitY.LZX <- glm(Y ~ X + Z, family = binomial(link = "log"), data = dat)
#   fitLogGest <- ivglm(estmethod = "g",
#                       X = "X",
#                       fitZ.L = fitZ.L,
#                       fitY.LZX = fitY.LZX,
#                       data = dat,
#                       link = "log",
#                       Y = "Y")
#   logcrr <- fitLogGest$est["X"]
#   logcrrse <- sqrt(fitLogGest$vcov)
#
#   fit11 <- tsps(Y ~ X | Z, data = dat, link = "logadd")
#   expect_equal(log(fit11$estci[1]), logcrr, tolerance = 0.05, ignore_attr = "names")
#
#   expect_s3_class(fit11, "tsps")
#
#   smy11 <- summary(fit11)
#   expect_s3_class(smy, "summary.tsps")
#
#   expect_output(print(fit11))
#   expect_output(print(smy11))
# })
#
# test_that("Single instrument example - logmult link", {
#   skip_on_cran()
#   # ivtools for comparison fit
#   library(ivtools)
#   fitZ.L <- glm(Z ~ 1, data = dat)
#   fitY.LZX <- glm(Y ~ X + Z, family = binomial(link = "log"), data = dat)
#   fitLogGest <- ivglm(estmethod = "g",
#                       X = "X",
#                       fitZ.L = fitZ.L,
#                       fitY.LZX = fitY.LZX,
#                       data = dat,
#                       link = "log",
#                       Y = "Y")
#   logcrr <- fitLogGest$est["X"]
#   logcrrse <- sqrt(fitLogGest$vcov)
#
#   fit11 <- tsps(Y ~ X | Z, data = dat, link = "logmult")
#   expect_equal(log(fit11$estci[1]), logcrr, tolerance = 0.05, ignore_attr = "names")
#
#   expect_s3_class(fit11, "tsps")
#
#   smy11 <- summary(fit11)
#   expect_s3_class(smy, "summary.tsps")
#
#   expect_output(print(fit11))
#   expect_output(print(smy11))
# })

test_that("Single instrument example - logit link", {
  skip_on_cran()
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
  expect_equal(fit21$estci[4,1], logcor, tolerance = 0.1, ignore_attr = "names")

  expect_s3_class(fit21, "tsps")

  smy21 <- summary(fit21)
  expect_s3_class(smy21, "summary.tsps")

  expect_output(print(fit21))
  expect_output(print(smy21))

  # manual fit for comparison
  stage1 <- lm(X ~ Z, data = dat)
  betamanual <- coef(stage1)
  xhat <- fitted.values(stage1)
  stage2 <- glm(Y ~ xhat, family = binomial)
  betamanual <- c(betamanual, coef(stage2))
  expect_equal(fit21$estci[,1], betamanual, ignore_attr = TRUE)
})

# Data generation for multiple instrument tests
set.seed(123456)
n <- 1000
psi0 <- 0.5
G1 <- rbinom(n, 2, 0.5)
G2 <- rbinom(n, 2, 0.3)
G3 <- rbinom(n, 2, 0.4)
C1 <- runif(n)
C2 <- runif(n)
U <- runif(n)
pX <- plogis(0.7*G1 + G2 - G3 + U + C1 + C2)
X <- rbinom(n, 1, pX)
pY <- plogis(-2 + psi0*X + U + C1 + C2)
Y <- rbinom(n, 1, pY)
dat <- data.frame(G1, G2, G3, X, Y, C1, C2)

test_that("Multiple instrument example with covariates - identity link", {
  skip_on_cran()

  fit30 <- tsps(Y ~ X + C1 + C2 | G1 + G2 + G3 + C1 + C2, data = dat)
  expect_output(print(fit30))
  smry30 <- summary(fit30)
  expect_output(print(smry30))

  # manual fit for comparison
  stage1 <- lm(X ~ G1 + G2 + G3 + C1 + C2, data = dat)
  betamanual <- coef(stage1)
  xhat <- fitted.values(stage1)
  stage2 <- lm(Y ~ xhat  + C1 + C2)
  betamanual <- c(betamanual, coef(stage2))
  expect_equal(fit30$estci[,1], betamanual, ignore_attr = TRUE)
})

test_that("Multiple instrument example with covariates - logit link", {
  skip_on_cran()

  fit31 <- tsps(Y ~ X + C1 + C2 | G1 + G2 + G3 + C1 + C2, data = dat, link = "logit")
  expect_output(print(fit31))
  smry31 <- summary(fit31)
  expect_output(print(smry31))

  # manual fit for comparison
  stage1 <- lm(X ~ G1 + G2 + G3 + C1 + C2, data = dat)
  betamanual <- coef(stage1)
  xhat <- fitted.values(stage1)
  stage2 <- glm(Y ~ xhat  + C1 + C2, family = binomial)
  betamanual <- c(betamanual, coef(stage2))
  expect_equal(fit31$estci[,1], betamanual, ignore_attr = TRUE)
})
