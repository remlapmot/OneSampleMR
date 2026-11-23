# Summarizing TSPS Fits

S3 print and summary methods for objects of class `"tsps"` and print
method for objects of class `"summary.tsps"`.

## Usage

``` r
# S3 method for class 'tsps'
summary(object, ...)

# S3 method for class 'tsps'
print(x, digits = max(3, getOption("digits") - 3), ...)

# S3 method for class 'summary.tsps'
print(
  x,
  digits = max(3, getOption("digits") - 3),
  signif.stars = getOption("show.signif.stars"),
  ...
)
```

## Arguments

- object:

  an object of class `"tsps"`.

- ...:

  further arguments passed to or from other methods.

- x:

  an object of class `"summary.tsps"`.

- digits:

  the number of significant digits to use when printing.

- signif.stars:

  logical; if `TRUE`, p-values are additionally encoded visually as
  significance stars.

## Value

`summary.tsps()` returns an object of class `"summary.tsps"`. A list
with the following elements:

- call:

  The call of the fitted model.

- link:

  The second stage link function.

- coefficients:

  A matrix of the parameter estimates, their standard errors, Wald z
  statistics, and p-values.

- smry:

  An object from a call to
  [`gmm::summary.gmm()`](https://rdrr.io/pkg/gmm/man/summary.html).

- object:

  The object of class `tsps` passed to the function.

## Examples

``` r
# See the examples at the bottom of help('tsps')
```
