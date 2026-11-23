# tests for ivxtile() and ivmw()

set.seed(12345)
n <- 2000
z <- rbinom(n, 2, 0.3)
w <- rnorm(n)
u <- rnorm(n)
x <- 0.5 * z + 0.3 * w + u + rnorm(n)
y <- 0.5 * x + w + u + rnorm(n)
yb <- rbinom(n, 1, plogis(-1 + 0.3 * x + w + u))
dat <- data.frame(y, yb, x, w, z)

test_that("xtile() gives groups of the expected sizes", {
  expect_equal(as.vector(table(xtile(1:10, 5))), rep(2, 5))
  expect_equal(as.vector(table(xtile(1:2000, 5))), rep(400, 5))
  # observations equal to a cut point are in the lower group
  expect_equal(xtile(c(1, 2, 3, 4), 2), c(1, 1, 2, 2))
  expect_equal(xtile(c(1, 2, 3), 2), c(1, 1, 2))
})

test_that("ivxtile() fits the estimator within quantile groups", {
  skip_on_cran()
  res <- ivxtile(y ~ x + w | z + w, data = dat, nq = 4)
  expect_s3_class(res, "ivxtile")
  expect_equal(nrow(res), 4)
  expect_equal(sum(res$n), n)
  expect_equal(attr(res, "par"), "x")

  # the estimates in the first group are those of the estimator fitted to it
  stage1 <- lm(x ~ z + w, data = dat)
  q1 <- xtile(residuals(stage1), 4) == 1
  fit1 <- ivreg::ivreg(y ~ x + w | z + w, data = dat[q1, ])
  expect_equal(res$beta[1], unname(coef(fit1)["x"]))
  expect_equal(res$se[1], unname(sqrt(diag(vcov(fit1)))["x"]))
  expect_equal(res$medexposure[1], median(dat$x[q1]))
  expect_equal(res$lowci, res$beta - 1.96 * res$se)

  expect_output(print(res), "within quantiles")
  pdf(NULL)
  expect_silent(plot(res))
  expect_silent(plot(res, ylim = c(-1, 2), legend = "bottomright"))
  expect_silent(plot(res, legend = NULL))
  dev.off()
})

test_that("ivxtile() works with the other estimators and their arguments", {
  skip_on_cran()
  res <- ivxtile(yb ~ x | z, data = dat, estimator = msmm, nq = 3)
  expect_false(anyNA(res$beta))
  res2 <- ivxtile(
    yb ~ x | z,
    data = dat,
    estimator = tsps,
    link = "logadd",
    nq = 3
  )
  expect_false(anyNA(res2$beta))
  expect_error(
    ivxtile(y ~ x | z, data = dat, par = "nonexistent"),
    "is not one of the parameters"
  )
})

test_that("ivmw() fits the estimator in moving windows", {
  skip_on_cran()
  res <- ivmw(y ~ x | z, data = dat, window = 1500, step = 100)
  expect_s3_class(res, "ivmw")
  expect_equal(res$start, seq(1, n - 1500 + 1, by = 100))
  expect_equal(res$end, res$start + 1500 - 1)
  expect_true(all(res$n == 1500))

  # the estimates in the first window are those of the estimator fitted to
  # the 1500 observations with the smallest first stage residuals
  stage1 <- lm(x ~ z, data = dat)
  rows <- order(residuals(stage1))[1:1500]
  fit1 <- ivreg::ivreg(y ~ x | z, data = dat[rows, ])
  expect_equal(res$beta[1], unname(coef(fit1)["x"]))

  expect_output(print(res), "moving windows")
  pdf(NULL)
  expect_silent(plot(res))
  dev.off()

  expect_equal(nrow(ivmw(y ~ x | z, data = dat, window = 1995)), 6)
  expect_error(ivmw(y ~ x | z, data = dat, window = n + 1), "window")
})

test_that("Clear error messages", {
  expect_error(ivxtile(y ~ x, data = dat), "two parts")
  expect_error(
    ivxtile(y ~ x + w | z, data = dat),
    "Only 1 exposure variable is allowed"
  )
})
