# tests for LSMM

# Data generation for the tests ----
set.seed(123456)
n <- 1000
psi0 <- 0.5
G1 <- rbinom(n, 2, 0.5)
G2 <- rbinom(n, 2, 0.3)
G3 <- rbinom(n, 2, 0.4)
U <- runif(n)
C <- runif(n)
X <- rbinom(n, 1, plogis(0.7 * G1 + G2 - G3 + U + C))
Y <- rbinom(n, 1, plogis(-2 + psi0 * X + U + C))
dat <- data.frame(G1, G2, G3, X, Y, C)

# The comparison values are from the Stata command ivlsmm in the
# ivonesamplemr package applied to dat, i.e.
# . ivlsmm Y (X = G1 G2 G3)
# . ivlsmm Y C (X = G1 G2 G3)

test_that("Comparison with Stata ivlsmm", {
  skip_on_cran()

  fit <- lsmm(Y ~ X | G1 + G2 + G3, data = dat)
  expect_s3_class(fit, "lsmm")
  # b0, xb_X, xb_G1, xb_G2, xb_G3, cmxb_X, ey0
  expect_equal(
    unname(gmm::coef.gmm(fit$fit)),
    c(-1.337279, .9957832, -.0809518, -.0839571, .0977643, .298774, .2993605),
    tolerance = 1e-3
  )
  expect_equal(
    unname(sqrt(diag(stats::vcov(fit$fit)))),
    c(.1600054, .1864225, .0573268, .0577643, .0658922, .4628326, .0786098),
    tolerance = 1e-3
  )
  expect_equal(
    unname(gmm::specTest(fit$fit)$test[1, 1]),
    2.86207,
    tolerance = 1e-3
  )

  fit2 <- lsmm(Y ~ X + C | G1 + G2 + G3 + C, data = dat)
  # b0, xb_X, xb_G1, xb_G2, xb_G3, xb_C, cmxb_X, cmxb_C, ey0
  expect_equal(
    unname(gmm::coef.gmm(fit2$fit)),
    c(
      -1.792906, .9297955, -.0788098, -.0836855, .0977609, .9858228,
      .2114122, 1.09354, .2084894
    ),
    tolerance = 1e-3
  )
  expect_equal(
    unname(sqrt(diag(stats::vcov(fit2$fit)))),
    c(
      .1952162, .1876928, .0562571, .0580089, .067142, .240485,
      .502509, .2566001, .0643046
    ),
    tolerance = 1e-3
  )
})

test_that("Parameters are labelled and causal odds ratios are for exposures only", {
  skip_on_cran()

  fit <- lsmm(Y ~ X + C | G1 + G2 + G3 + C, data = dat)
  expect_identical(
    names(gmm::coef.gmm(fit$fit)),
    c(
      "assoc:(Intercept)", "assoc:X", "assoc:G1", "assoc:G2", "assoc:G3",
      "assoc:C", "X", "C", "E[Y(0)]"
    )
  )
  expect_identical(rownames(fit$corci), "X")
  expect_identical(colnames(fit$corci)[1], "COR")
  expect_equal(
    fit$corci[1, 1],
    exp(gmm::coef.gmm(fit$fit)[["X"]])
  )
  expect_identical(fit$covariates, "C")

  expect_output(print(fit), "Causal odds ratio with 95% CI")
  expect_output(print(fit), "Adjusted for: C")
  expect_output(print(fit), "E\\[Y\\(0\\)\\] when the covariates are 0")
  smry <- summary(fit)
  expect_s3_class(smry, "summary.lsmm")
  expect_output(print(smry), "J-Test")
  expect_output(print(smry), "Adjusted for: C")

  fit0 <- lsmm(Y ~ X | G1 + G2 + G3, data = dat)
  expect_identical(fit0$covariates, character(0))
  expect_no_match(capture.output(print(fit0)), "Adjusted for")
})

test_that("User specified t0 gives the same estimates", {
  skip_on_cran()

  fit <- lsmm(Y ~ X | G1 + G2 + G3, data = dat)
  fitt0 <- lsmm(
    Y ~ X | G1 + G2 + G3,
    data = dat,
    t0 = c(-1, 1, 0, 0, 0, 0, 0.3)
  )
  expect_equal(
    gmm::coef.gmm(fitt0$fit),
    gmm::coef.gmm(fit$fit),
    tolerance = 1e-5
  )
})

test_that("Clear error messages", {
  dat2 <- dat
  dat2$Y[1] <- 2
  expect_error(
    lsmm(Y ~ X | G1 + G2 + G3, data = dat2),
    "outcome must be binary"
  )
  expect_error(
    lsmm(Y ~ C | G1 + C, data = dat),
    "At least one exposure"
  )
  expect_error(
    lsmm(Y ~ X + G2 | G1, data = dat),
    "at least as many instruments as exposures"
  )
  expect_error(
    lsmm(Y ~ X | G1 + G2 + G3, data = dat, t0 = c(0, 0)),
    "t0 must have length 7"
  )
})
