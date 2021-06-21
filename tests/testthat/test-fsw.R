# ivreg package

library(ivreg)

## data
data("CigaretteDemand", package = "ivreg")

## model
m <- ivreg(log(packs) ~ log(rprice) + log(rincome) | salestax + log(rincome),
           data = CigaretteDemand)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# lfe package - modified example from condfstat() helpfile

library(lfe)

set.seed(12345)
n <- 4000

z1 <- rnorm(n)
z2 <- rnorm(n)
u <- rnorm(n)
# make x1, x2 correlated with errors u

x1 <- z1 + z2 + 0.2*u + rnorm(n)
x2 <- z1 + 0.94*z2 - 0.3*u + rnorm(n)
y <- x1 + x2 + u
est <- felm(y ~ 1 | 0 | (x1 | x2 ~ z1 + z2))
summary(est)
## Not run:
summary(est$stage1, lhs='x1')
summary(est$stage1, lhs='x2')

## End(Not run)

# the joint significance of the instruments in both the first stages are ok:
t(sapply(est$stage1$lhs, function(lh) waldtest(est$stage1, ~ z1|z2, lhs = lh)))
# everything above looks fine, t-tests for instruments,
# as well as F-tests for excluded instruments in the 1st stages.
# The conditional F-test reveals that the instruments are jointly weak
# (it's close to being only one instrument, z1+z2, for both x1 and x2)
condfstat(est, quantiles = c(0.05, 0.95))
