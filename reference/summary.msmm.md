# Summarizing MSMM Fits

Summarizing MSMM Fits

## Usage

``` r
# S3 method for class 'msmm'
summary(object, ...)

# S3 method for class 'msmm'
print(x, digits = max(3, getOption("digits") - 3), ...)

# S3 method for class 'summary.msmm'
print(
  x,
  digits = max(3, getOption("digits") - 3),
  signif.stars = getOption("show.signif.stars"),
  ...
)
```

## Arguments

- object:

  an object of class `"msmm"`.

- ...:

  further arguments passed to or from other methods.

  S3 summary and print methods for objects of class `msmm` and
  `summary.msmm`.

- x:

  an object of class `"summary.msmm"`.

- digits:

  the number of significant digits to use when printing.

- signif.stars:

  logical; if `TRUE`, p-values are additionally encoded visually as
  significance stars.

## Value

`summary.msmm()` returns an object of class `"summary.msmm"`. A list
with the following elements:

- call:

  The call of the fitted model.

- estmethod:

  The estimation method.

- coefficients:

  A matrix of the parameter estimates, their standard errors, Wald test
  statistics, and p-values.

- jtest:

  For the `"gmm"` and `"gmmalt"` methods with more instruments than
  exposures and covariates, the Hansen J-statistic of the
  overidentifying restrictions, its degrees of freedom, and p-value;
  otherwise `NULL`.

- smry:

  An object from a call to either
  [`gmm::summary.gmm()`](https://rdrr.io/pkg/gmm/man/summary.html) or
  [`ivreg::summary.ivreg()`](https://zeileis.github.io/ivreg/reference/summary.ivreg.html).

- object:

  The object of class `msmm` passed to the function.

## Examples

``` r
# For examples see the examples at the bottom of help('msmm')
```
