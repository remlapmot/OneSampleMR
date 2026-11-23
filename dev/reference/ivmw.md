# Moving Window Estimation

`ivmw()` fits an IV estimator in moving (sliding) windows of consecutive
observations ordered by the first stage residuals (equivalently by the
IV-free exposure), to explore whether the causal effect varies with the
level of the exposure, as described by Burgess et al. (2014). It is the
equivalent of the `ivmw` prefix command in the Stata package
ivonesamplemr.

## Usage

``` r
ivmw(
  formula,
  instruments,
  data,
  estimator = ivreg::ivreg,
  window,
  step = 1,
  par = NULL,
  ...
)

# S3 method for class 'ivmw'
print(x, digits = max(3, getOption("digits") - 3), ...)

# S3 method for class 'ivmw'
plot(
  x,
  xlab = "Median of exposure in moving window",
  ylab = "Estimated causal effect",
  ...
)
```

## Arguments

- formula, instruments:

  formula specification(s) of the model, as for the estimator, with a
  single exposure, e.g. `y ~ x + w | z1 + z2 + w`.

- data:

  a data frame containing the variables in the model.

- estimator:

  the function used to fit the model within each group, e.g.
  [`ivreg::ivreg()`](https://zeileis.github.io/ivreg/reference/ivreg.html),
  [`msmm()`](https://remlapmot.github.io/OneSampleMR/dev/reference/msmm.md),
  [`lsmm()`](https://remlapmot.github.io/OneSampleMR/dev/reference/lsmm.md),
  [`tsps()`](https://remlapmot.github.io/OneSampleMR/dev/reference/tsps.md),
  or
  [`tsri()`](https://remlapmot.github.io/OneSampleMR/dev/reference/tsri.md).
  It must accept `formula` and `data` arguments and return an object
  with [`coef()`](https://rdrr.io/r/stats/coef.html) and
  [`vcov()`](https://rdrr.io/r/stats/vcov.html) methods.

- window:

  the number of consecutive observations in each window.

- step:

  the number of observations by which successive windows are moved.

- par:

  the name of the parameter to collect from each fit. By default the
  parameter of the exposure, which is the causal effect on the scale on
  which it is estimated, for example the log causal risk ratio for
  [`msmm()`](https://remlapmot.github.io/OneSampleMR/dev/reference/msmm.md).

- ...:

  further arguments passed to `estimator`, e.g. `link` for
  [`tsps()`](https://remlapmot.github.io/OneSampleMR/dev/reference/tsps.md)
  and
  [`tsri()`](https://remlapmot.github.io/OneSampleMR/dev/reference/tsri.md)
  or `estmethod` for
  [`msmm()`](https://remlapmot.github.io/OneSampleMR/dev/reference/msmm.md).

- x:

  an object of class `"ivmw"`.

- digits:

  the number of significant digits to use when printing.

- xlab, ylab:

  axis labels.

## Value

An object of class `"ivmw"`, a data frame with one row for each window
and the columns `start` and `end` (the first and last observation of the
window in the ordering by the first stage residuals), `n`, `beta`, `se`,
`medexposure`, `lowci`, and `uppci`, as for
[`ivxtile()`](https://remlapmot.github.io/OneSampleMR/dev/reference/ivxtile.md).
The object has [`print()`](https://rdrr.io/r/base/print.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods.

## Details

The first stage linear regression of the exposure on the instruments and
any covariates is fitted in the full data, and the observations are
ordered by its residuals. The estimator is then fitted in windows of
`window` consecutive observations, starting at observations 1, 1 +
`step`, 1 + 2 `step`, and so on. With the default `step = 1` there are
`n - window + 1` windows, as for Stata's `ivmw` command.

The approach relies on the same additional assumption as
[`ivxtile()`](https://remlapmot.github.io/OneSampleMR/dev/reference/ivxtile.md),
that the effect of the instruments on the exposure is the same in each
window (Burgess, 2023).

## References

Burgess S, Davies NM, Thompson SG. Instrumental variable analysis with a
nonlinear exposure-outcome relationship. Epidemiology, 2014, 25, 6,
877-885.
[doi:10.1097/EDE.0000000000000161](https://doi.org/10.1097/EDE.0000000000000161)

Burgess S. Violation of the constant genetic effect assumption can
result in biased estimates for non-linear Mendelian randomization. Human
Heredity, 2023, 88, 1, 79-90.
[doi:10.1159/000531659](https://doi.org/10.1159/000531659)

## See also

[`ivxtile()`](https://remlapmot.github.io/OneSampleMR/dev/reference/ivxtile.md)

## Examples

``` r
set.seed(12345)
n   <- 5000
z   <- rbinom(n, 2, 0.3)
u   <- rnorm(n)
x   <- 0.5 * z + u + rnorm(n)
y   <- 0.5 * x + 0.2 * x^2 + u + rnorm(n)
dat <- data.frame(y, x, z)
res <- ivmw(y ~ x | z, data = dat, window = 2500, step = 250)
res
#> 
#> Estimates of x in moving windows of the first stage residuals:
#> 
#>  start  end    n   beta      se medexposure  lowci  uppci
#>      1 2500 2500 0.3113 0.07918    -0.67168 0.1561 0.4665
#>    251 2750 2500 0.4138 0.07621    -0.46808 0.2644 0.5632
#>    501 3000 2500 0.5580 0.07234    -0.27253 0.4163 0.6998
#>    751 3250 2500 0.6412 0.07162    -0.06634 0.5008 0.7816
#>   1001 3500 2500 0.6930 0.07147     0.13194 0.5529 0.8330
#>   1251 3750 2500 0.8123 0.06963     0.31071 0.6758 0.9487
#>   1501 4000 2500 0.8352 0.07041     0.47866 0.6972 0.9732
#>   1751 4250 2500 0.8634 0.07061     0.65569 0.7250 1.0018
#>   2001 4500 2500 0.9563 0.06897     0.84681 0.8211 1.0915
#>   2251 4750 2500 1.0507 0.06867     1.05877 0.9161 1.1853
#>   2501 5000 2500 1.1288 0.07326     1.26509 0.9853 1.2724
#> 
plot(res)
```
