# tests for TSPS

# Data generation from the example in the ivtools::ivglm() helpfile ----
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
  skip_if_not_available("ivtools")
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

  # manual fit
  stage1 <- lm(X ~ Z)
  betamanual <- coef(stage1)
  xhat <- fitted.values(stage1)
  stage2 <- lm(Y ~ xhat)
  betamanual <- c(betamanual, coef(stage2))
  expect_equal(fit01$estci[,1], betamanual, ignore_attr = TRUE)
})

test_that("Single instrument example - logadd link", {
  skip_on_cran()
  skip_if_not_available("ivtools")
  # ivtools for comparison fit
  library(ivtools)
  fitZ.L <- glm(Z ~ 1, data = dat)
  fitY.LZX <- glm(Y ~ X + Z, family = poisson, data = dat) # binomial(link = "log")
  fitLogGest <- ivglm(estmethod = "g",
                      X = "X",
                      fitZ.L = fitZ.L,
                      fitY.LZX = fitY.LZX,
                      data = dat,
                      link = "log",
                      Y = "Y")
  logcrr <- fitLogGest$est["X"]
  logcrrse <- sqrt(fitLogGest$vcov)

  fit11 <- tsps(Y ~ X | Z, data = dat, link = "logadd")
  expect_equal(fit11$estci[4,1], logcrr, tolerance = 0.05, ignore_attr = "names")

  expect_s3_class(fit11, "tsps")

  smy11 <- summary(fit11)
  expect_s3_class(smy11, "summary.tsps")

  expect_output(print(fit11))
  expect_output(print(smy11))

  # manual estimation check
  stage1 <- lm(X ~ Z, data = dat)
  betamanual <- coef(stage1)
  xhat <- fitted.values(stage1)
  stage2 <- glm(Y ~ xhat, family = poisson) # binomial(link = "log")
  betamanual <- c(betamanual, coef(stage2))
  expect_equal(fit11$estci[,1], betamanual, ignore_attr = "names")
})

test_that("Single instrument example - logmult link", {
  skip_on_cran()
  skip_if_not_available("ivtools")
  # ivtools for comparison fit
  library(ivtools)
  fitZ.L <- glm(Z ~ 1, data = dat)
  dat$Y[dat$Y == 0] <- 0.001
  fitY.LZX <- glm(Y ~ X + Z, family = Gamma(link = "log"), data = dat)
  fitLogGest <- ivglm(estmethod = "g",
                      X = "X",
                      fitZ.L = fitZ.L,
                      fitY.LZX = fitY.LZX,
                      data = dat,
                      link = "log",
                      Y = "Y")
  logcrr <- fitLogGest$est["X"]
  logcrrse <- sqrt(fitLogGest$vcov)

  fit12 <- tsps(Y ~ X | Z, data = dat, link = "logmult")
  expect_equal(fit12$estci[4,1], logcrr, tolerance = 0.05, ignore_attr = "names")

  expect_s3_class(fit12, "tsps")

  smy12 <- summary(fit12)
  expect_s3_class(smy12, "summary.tsps")

  expect_output(print(fit12))
  expect_output(print(smy12))

  # manual fit for comparison
  stage1 <- lm(X ~ Z, data = dat)
  betamanual <- coef(stage1)
  xhat <- fitted.values(stage1)
  Y[Y == 0] <- 0.001
  stage2 <- glm(Y ~ xhat, family = Gamma(link = "log"))
  betamanual <- c(betamanual, coef(stage2))
  expect_equal(fit12$estci[,1], betamanual, ignore_attr = "names")
})

test_that("Single instrument example - logit link", {
  skip_on_cran()
  skip_if_not_available("ivtools")
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

# Subset of observations ----

test_that("Test subset argument", {
  skip_on_cran()
  datfifty <- dat[1:50,]
  fitcompare <- tsps(Y ~ X | Z, data = datfifty)
  fitsubset <- tsps(Y ~ X | Z, data = dat, subset = 1:50)
  expect_equal(fitsubset$estci, fitcompare$estci)
})

# Data generation for multiple instrument tests ----
set.seed(123456)
n <- 1000
psi0 <- 0.8
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
  stage2 <- lm(Y ~ xhat + C1 + C2)
  betamanual <- c(betamanual, coef(stage2))
  expect_equal(fit30$estci[,1], betamanual, ignore_attr = TRUE)
})

test_that("Multiple instrument example with covariates - logadd link", {
  skip_on_cran()

  fit31 <- tsps(Y ~ X + C1 + C2 | G1 + G2 + G3 + C1 + C2, data = dat, link = "logadd")
  expect_output(print(fit31))
  smry31 <- summary(fit31)
  expect_output(print(smry31))

  # manual fit for comparison
  stage1 <- lm(X ~ G1 + G2 + G3 + C1 + C2, data = dat)
  betamanual <- coef(stage1)
  xhat <- fitted.values(stage1)
  stage2 <- glm(Y ~ xhat + C1 + C2, family = poisson)
  betamanual <- c(betamanual, coef(stage2))
  expect_equal(fit31$estci[,1], betamanual, ignore_attr = TRUE)
})

test_that("Multiple instrument example with covariates - logmult link", {
  skip_on_cran()

  fit32 <- tsps(Y ~ X + C1 + C2 | G1 + G2 + G3 + C1 + C2, data = dat, link = "logmult")
  expect_output(print(fit32))
  smry32 <- summary(fit32)
  expect_output(print(smry32))

  # manual fit for comparison
  stage1 <- lm(X ~ G1 + G2 + G3 + C1 + C2, data = dat)
  betamanual <- coef(stage1)
  xhat <- fitted.values(stage1)
  Y[Y == 0] <- 0.001
  stage2 <- glm(Y ~ xhat + C1 + C2, family = Gamma(link = "log"), control = list(maxit = 1E2))
  betamanual <- c(betamanual, coef(stage2))
  expect_equal(fit32$estci[,1], betamanual, tolerance = 0.01, ignore_attr = TRUE)
})

test_that("Multiple instrument example with covariates - logit link", {
  skip_on_cran()

  fit33 <- tsps(Y ~ X + C1 + C2 | G1 + G2 + G3 + C1 + C2, data = dat, link = "logit")
  expect_output(print(fit33))
  smry33 <- summary(fit33)
  expect_output(print(smry33))

  # manual fit for comparison
  stage1 <- lm(X ~ G1 + G2 + G3 + C1 + C2, data = dat)
  betamanual <- coef(stage1)
  xhat <- fitted.values(stage1)
  stage2 <- glm(Y ~ xhat + C1 + C2, family = binomial)
  betamanual <- c(betamanual, coef(stage2))
  expect_equal(fit33$estci[,1], betamanual, ignore_attr = TRUE)
})
