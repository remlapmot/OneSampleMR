# Summarizing TSRI Fits

S3 print and summary methods for objects of class `"tsri"` and print
method for objects of class `"summary.tsri"`.

## Usage

``` r
# S3 method for class 'tsri'
summary(object, ...)

# S3 method for class 'tsri'
print(x, digits = max(3, getOption("digits") - 3), ...)

# S3 method for class 'summary.tsri'
print(x, digits = max(3, getOption("digits") - 3), ...)
```

## Arguments

- object:

  an object of class `"tsri"`.

- ...:

  further arguments passed to or from other methods.

- x:

  an object of class `"summary.tsri"`.

- digits:

  the number of significant digits to use when printing.

## Value

`summary.tsri()` returns an object of class `"summary.tsri"`. A list
with the following elements:

- smry:

  An object from a call to
  [`gmm::summary.gmm()`](https://rdrr.io/pkg/gmm/man/summary.html)

- object:

  The object of class `tsps` passed to the function.

## Examples

``` r
# See the examples at the bottom of help('tsri')
```
