# Summarizing MSMM Fits

Summarizing MSMM Fits

## Usage

``` r
# S3 method for class 'msmm'
summary(object, ...)

# S3 method for class 'msmm'
print(x, digits = max(3, getOption("digits") - 3), ...)

# S3 method for class 'summary.msmm'
print(x, digits = max(3, getOption("digits") - 3), ...)
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

## Value

`summary.msmm()` returns an object of class `"summary.msmm"`. A list
with the following elements:

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
