# Comparison fits of the multiplicative structural mean model

``` r

library(OneSampleMR)
```

## Comparison fits

- Example from the **ivtools** `ivglm()` helpfile. First simulate some
  example data.

``` r

set.seed(12345)
n    <- 5000
psi0 <- 0.5
psi1 <- 0.2
Z    <- rbinom(n, 1, 0.5)
X    <- rbinom(n, 1, 0.7 * Z + 0.2 * (1 - Z))
m0   <- plogis(1 + 0.8 * X - 0.39 * Z)
Y    <- rbinom(n, 1, plogis(psi0 * X + log(m0 / (1 - m0))))
dat  <- data.frame(Z, X, Y)
```

- Comparison fit using
  [`msmm()`](https://remlapmot.github.io/OneSampleMR/dev/reference/msmm.md).

``` r

fit02 <- msmm(Y ~ X | Z, data = dat)
summary(fit02)
#> 
#> Call:
#> msmm(formula = Y ~ X | Z, data = dat)
#> 
#> Estimation method: gmm 
#> 
#> Coefficients:
#>         Estimate Std. Error z value Pr(>|z|)    
#> E[Y(0)]  0.75000    0.01233   60.81  < 2e-16 ***
#> X        0.10186    0.02822    3.61 0.000306 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> E[Y(0)] with 95% CI:
#>         0.025  0.975 
#> 0.7500 0.7258 0.7742 
#> 
#> Causal risk ratio with 95% CI:
#>     CRR 0.025 0.975
#> X 1.107 1.048  1.17
```

- Comparison fit using the alternative GMM moment conditions.

``` r

fit03 <- msmm(Y ~ X | Z, data = dat, estmethod = "gmmalt")
summary(fit03)
#> 
#> Call:
#> msmm(formula = Y ~ X | Z, data = dat, estmethod = "gmmalt")
#> 
#> Estimation method: gmmalt 
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> log E[Y(0)] -0.28768    0.01644  -17.50  < 2e-16 ***
#> X            0.10186    0.02822    3.61 0.000306 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> E[Y(0)] with 95% CI:
#>         0.025  0.975 
#> 0.7500 0.7262 0.7746 
#> 
#> Causal risk ratio with 95% CI:
#>     CRR 0.025 0.975
#> X 1.107 1.048  1.17
```

- Comparison fit using transformed variables using two-stage least
  squares.

``` r

fit04 <- msmm(Y ~ X | Z, data = dat, estmethod = "tsls")
summary(fit04)
#> 
#> Call:
#> msmm(formula = Y ~ X | Z, data = dat, estmethod = "tsls")
#> 
#> Estimation method: tsls 
#> 
#> Coefficients of the two-stage least squares fit of the transformed
#> variables:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.75000    0.01173   63.95   <2e-16 ***
#> exposure    -0.90315    0.02545  -35.49   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> E[Y(0)] with 95% CI:
#>         2.5 % 97.5 % 
#>  0.750  0.727  0.773 
#> 
#> Causal risk ratio with 95% CI:
#>     CRR 0.025 0.975
#> X 1.107 1.048  1.17
```

- Comparison fit using the alternative transformed variables approach
  using two-stage least squares.

``` r

fit05 <- msmm(Y ~ X | Z, data = dat, estmethod = "tslsalt")
summary(fit05)
#> 
#> Call:
#> msmm(formula = Y ~ X | Z, data = dat, estmethod = "tslsalt")
#> 
#> Estimation method: tslsalt 
#> 
#> Coefficients of the two-stage least squares fit of the transformed
#> variables:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.83043    0.01341   61.94   <2e-16 ***
#> exposure    -1.10723    0.03120  -35.49   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Causal risk ratio with 95% CI:
#>     CRR 0.025 0.975
#> X 1.107 1.048  1.17
```
