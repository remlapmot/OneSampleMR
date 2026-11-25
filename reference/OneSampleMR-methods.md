# Methods for Fitted Models

Extract the estimated parameters, their variance-covariance matrix, Wald
confidence intervals, the number of observations, and the model formula
from models fitted by
[`msmm()`](https://remlapmot.github.io/OneSampleMR/reference/msmm.md),
[`lsmm()`](https://remlapmot.github.io/OneSampleMR/reference/lsmm.md),
[`tsps()`](https://remlapmot.github.io/OneSampleMR/reference/tsps.md),
and
[`tsri()`](https://remlapmot.github.io/OneSampleMR/reference/tsri.md).

## Usage

``` r
# S3 method for class 'msmm'
coef(object, ...)

# S3 method for class 'lsmm'
coef(object, ...)

# S3 method for class 'tsps'
coef(object, ...)

# S3 method for class 'tsri'
coef(object, ...)

# S3 method for class 'msmm'
vcov(object, ...)

# S3 method for class 'lsmm'
vcov(object, ...)

# S3 method for class 'tsps'
vcov(object, ...)

# S3 method for class 'tsri'
vcov(object, ...)

# S3 method for class 'msmm'
confint(object, parm, level = 0.95, ...)

# S3 method for class 'lsmm'
confint(object, parm, level = 0.95, ...)

# S3 method for class 'tsps'
confint(object, parm, level = 0.95, ...)

# S3 method for class 'tsri'
confint(object, parm, level = 0.95, ...)

# S3 method for class 'msmm'
nobs(object, ...)

# S3 method for class 'lsmm'
nobs(object, ...)

# S3 method for class 'tsps'
nobs(object, ...)

# S3 method for class 'tsri'
nobs(object, ...)

# S3 method for class 'msmm'
formula(x, ...)

# S3 method for class 'lsmm'
formula(x, ...)

# S3 method for class 'tsps'
formula(x, ...)

# S3 method for class 'tsri'
formula(x, ...)
```

## Arguments

- object:

  a fitted model object of class `"msmm"`, `"lsmm"`, `"tsps"`, or
  `"tsri"`.

- ...:

  further arguments passed to or from other methods.

- parm:

  a specification of which parameters are to be given confidence
  intervals, either a vector of numbers or a vector of names. If
  missing, all parameters are considered.

- level:

  the confidence level required.

- x:

  a fitted model object of class `"msmm"`, `"lsmm"`, `"tsps"`, or
  `"tsri"`.

## Value

[`coef()`](https://rdrr.io/r/stats/coef.html) returns a named vector of
the parameter estimates, [`vcov()`](https://rdrr.io/r/stats/vcov.html)
their variance-covariance matrix,
[`confint()`](https://rdrr.io/r/stats/confint.html) a matrix of Wald
confidence limits, [`nobs()`](https://rdrr.io/r/stats/nobs.html) the
number of observations, and
[`formula()`](https://rdrr.io/r/stats/formula.html) the model formula.

## Details

The parameters are those of the underlying GMM (or, for the `"tsls"` and
`"tslsalt"` methods of
[`msmm()`](https://remlapmot.github.io/OneSampleMR/reference/msmm.md),
two-stage least squares) fit, on the scale on which they are estimated.
For example, the causal effect parameter of
[`msmm()`](https://remlapmot.github.io/OneSampleMR/reference/msmm.md) is
a log causal risk ratio and that of
[`lsmm()`](https://remlapmot.github.io/OneSampleMR/reference/lsmm.md) a
log causal odds ratio. The exponentiated causal effect estimates are
available in the `crrci` (MSMM), `corci` (LSMM), and `estci` (TSPS and
TSRI) elements of the fitted objects, and are printed by their
[`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) methods.

## Examples

``` r
set.seed(123456)
n    <- 1000
G1   <- rbinom(n, 2, 0.5)
G2   <- rbinom(n, 2, 0.3)
G3   <- rbinom(n, 2, 0.4)
U    <- runif(n)
X    <- rbinom(n, 1, plogis(0.7*G1 + G2 - G3 + U))
Y    <- rbinom(n, 1, plogis(-2 + 0.5*X + U))
dat  <- data.frame(G1, G2, G3, X, Y)
fit  <- msmm(Y ~ X | G1 + G2 + G3, data = dat)
coef(fit)
#>   E[Y(0)]         X 
#> 0.1826755 0.4502279 
vcov(fit)
#>              E[Y(0)]           X
#> E[Y(0)]  0.002703329 -0.02131077
#> X       -0.021310766  0.17479198
confint(fit)
#>               2.5 %   97.5 %
#> E[Y(0)]  0.08077005 0.284581
#> X       -0.36919642 1.269652
nobs(fit)
#> [1] 1000
formula(fit)
#> Y ~ X | G1 + G2 + G3
#> <environment: 0x5d9e92a855f8>
```
