# Conditional F-statistic of Sanderson and Windmeijer (2016)

`fsw` calculates the conditional F-statistic of Sanderson and Windmeijer
(2016) for each endogenous variable in the model.

## Usage

``` r
fsw(object)

# S3 method for class 'ivreg'
fsw(object)
```

## Arguments

- object:

  An object of class `"ivreg"` containing the results of an IV model
  fitted by
  [`ivreg::ivreg()`](https://zeileis.github.io/ivreg/reference/ivreg.html)
  for which to calculate the conditional F-statistics for each
  endogenous variable.

## Value

An object of class `"fsw"` with the following elements:

- fswres:

  matrix with columns for the conditional *F*-statistics, degrees of
  freedom, residual degrees of freedom, and p-value. 1 row per
  endogenous variable.

- namesendog:

  a character vector of the variable names of the endogenous variables.

- nendog:

  the number of endogenous variables.

- n:

  the sample size used for the fitted model.

## References

Sanderson E and Windmeijer F. A weak instrument *F*-test in linear IV
models with multiple endogenous variables. Journal of Econometrics,
2016, 190, 2, 212-221,
[doi:10.1016/j.jeconom.2015.06.004](https://doi.org/10.1016/j.jeconom.2015.06.004)
.

## Examples

``` r
require(ivreg)
#> Loading required package: ivreg
set.seed(12345)
n   <- 4000
z1  <- rnorm(n)
z2  <- rnorm(n)
w1  <- rnorm(n)
w2  <- rnorm(n)
u   <- rnorm(n)
x1  <- z1 + z2 + 0.2*u + 0.1*w1 + rnorm(n)
x2  <- z1 + 0.94*z2 - 0.3*u + 0.1*w2 + rnorm(n)
y   <- x1 + x2 + w1 + w2 + u
dat <- data.frame(w1, w2, x1, x2, y, z1, z2)
mod <- ivreg::ivreg(y ~ x1 + x2 + w1 + w2 | z1 + z2 + w1 + w2, data = dat)
fsw(mod)
#> 
#> Model sample size:  4000 
#> 
#> Sanderson-Windmeijer conditional F-statistics for first stage model:
#>    F value d.f. Residual d.f.   Pr(>F)  
#> x1 3.42466    1          3995 0.032656 *
#> x2 3.42443    1          3995 0.032663 *
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
```
