library(ivreg)

## data
data("CigaretteDemand", package = "ivreg")

## model
m <- ivreg(log(packs) ~ log(rprice) + log(rincome) | salestax + log(rincome),
           data = CigaretteDemand)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
