# Summarizing LSMM Fits

S3 print and summary methods for objects of class `"lsmm"` and print
method for objects of class `"summary.lsmm"`.

## Usage

``` r
# S3 method for class 'lsmm'
summary(object, ...)

# S3 method for class 'lsmm'
print(x, digits = max(3, getOption("digits") - 3), ...)

# S3 method for class 'summary.lsmm'
print(x, digits = max(3, getOption("digits") - 3), ...)
```

## Arguments

- object:

  an object of class `"lsmm"`.

- ...:

  further arguments passed to or from other methods.

- x:

  an object of class `"summary.lsmm"`.

- digits:

  the number of significant digits to print.

## Value

`summary.lsmm()` returns an object of class `"summary.lsmm"`. A list
with the following elements:

- smry:

  An object from a call to
  [`gmm::summary.gmm()`](https://rdrr.io/pkg/gmm/man/summary.html)

- object:

  The object of class `"lsmm"` passed to the function.

## Examples

``` r
# For examples see the examples at the bottom of help('lsmm')
```
