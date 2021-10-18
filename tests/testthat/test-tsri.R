# tests for TSRI

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
                        Y = "Y",
                        ctrl = TRUE)
  crd <- fitIdentGest$est["X"]
  crdse <- sqrt(fitIdentGest$vcov)

  fit01 <- tsri(Y ~ X | Z, data = dat, link = "identity")
  expect_equal(fit01$estci[4,1], crd, ignore_attr = "names")

  expect_s3_class(fit01, "tsri")

  smy01 <- summary(fit01)
  expect_s3_class(smy01, "summary.tsri")

  expect_output(print(fit01))
  expect_output(print(smy01))

  # manual fit
  stage1 <- lm(X ~ Z)
  betamanual <- coef(stage1)
  res <- residuals(stage1)
  stage2 <- lm(Y ~ X + res)
  betamanual <- c(betamanual, coef(stage2))
  expect_equal(fit01$estci[,1], betamanual, ignore_attr = TRUE)
})


