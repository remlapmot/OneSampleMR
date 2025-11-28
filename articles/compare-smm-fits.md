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
  [`msmm()`](https://remlapmot.github.io/OneSampleMR/reference/msmm.md).

``` r
fit02 <- msmm(Y ~ X | Z, data = dat)
summary(fit02)
#> 
#> Estimation method: gmm 
#> 
#> GMM fit summary:
#> 
#> Call:
#> gmm::gmm(g = msmmMoments, x = dat, t0 = t0, vcov = "iid")
#> 
#> 
#> Method:  twoStep 
#> 
#> Coefficients:
#>           Estimate    Std. Error  t value     Pr(>|t|)  
#> Theta[1]  7.4993e-01  1.2332e-02  6.0810e+01  0.0000e+00
#> Theta[2]  1.0198e-01  2.8219e-02  3.6140e+00  3.0156e-04
#> 
#> J-Test: degrees of freedom is 0 
#>                 J-test                P-value             
#> Test E(g)=0:    3.31877269936619e-06  *******             
#> 
#> #############
#> Information related to the numerical optimization
#> Convergence code =  0 
#> Function eval. =  61 
#> Gradian eval. =  NA 
#> 
#> E[Y(0)] with 95% CI:
#>         0.025  0.975 
#> 0.7499 0.7258 0.7741 
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
#> Estimation method: gmmalt 
#> 
#> GMM fit summary:
#> 
#> Call:
#> gmm::gmm(g = msmmAltMoments, x = dat, t0 = t0, vcov = "iid")
#> 
#> 
#> Method:  twoStep 
#> 
#> Coefficients:
#>           Estimate     Std. Error   t value      Pr(>|t|)   
#> Theta[1]  -2.8759e-01   1.6444e-02  -1.7489e+01   1.7357e-68
#> Theta[2]   1.0173e-01   2.8217e-02   3.6053e+00   3.1178e-04
#> 
#> J-Test: degrees of freedom is 0 
#>                 J-test                P-value             
#> Test E(g)=0:    3.75832932731903e-06  *******             
#> 
#> #############
#> Information related to the numerical optimization
#> Convergence code =  0 
#> Function eval. =  55 
#> Gradian eval. =  NA 
#> 
#> E[Y(0)] with 95% CI:
#>         0.025  0.975 
#> 0.7501 0.7263 0.7746 
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
#> Estimation method: tsls 
#> 
#> Stage 1 summary:
#> 
#> Call:
#> stats::lm(formula = exposure ~ z)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -0.6206 -0.1873 -0.1873  0.3794  0.8127 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 0.187272   0.008878   21.09   <2e-16 ***
#> z           0.433336   0.012474   34.74   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.441 on 4998 degrees of freedom
#> Multiple R-squared:  0.1945, Adjusted R-squared:  0.1943 
#> F-statistic:  1207 on 1 and 4998 DF,  p-value: < 2.2e-16
#> 
#> TSLS fit summary:
#> 
#> Call:
#> ivreg::ivreg(formula = outcome ~ exposure | z)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -0.7500  0.1532  0.1532  0.2500  0.2500 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.75000    0.01173   63.95   <2e-16 ***
#> exposure    -0.90315    0.02545  -35.49   <2e-16 ***
#> 
#> Diagnostic tests:
#>                   df1  df2 statistic p-value    
#> Weak instruments    1 4998    1206.9  <2e-16 ***
#> Wu-Hausman          1 4997     147.1  <2e-16 ***
#> Sargan              0   NA        NA      NA    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.3898 on 4998 degrees of freedom
#> Multiple R-Squared: 0.3569,  Adjusted R-squared: 0.3567 
#> Wald test:  1260 on 1 and 4998 DF,  p-value: < 2.2e-16 
#> 
#> 
#> E[Y(0)] with 95% CI:
#>         2.5 % 97.5 % 
#>  0.750  0.727  0.773 
#> 
#> Causal risk ratio with 95% CI:
#> [1] 1.107 1.048 1.170
```

- Comparison fit using the alternative transformed variables approach
  using two-stage least squares.

``` r
fit05 <- msmm(Y ~ X | Z, data = dat, estmethod = "tslsalt")
summary(fit05)
#> 
#> Estimation method: tslsalt 
#> 
#> Stage 1 summary:
#> 
#> Call:
#> stats::lm(formula = exposure ~ z)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -0.5809 -0.1895 -0.1895  0.4191  0.8105 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.580867   0.008959   64.84   <2e-16 ***
#> z           -0.391369   0.012587  -31.09   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.445 on 4998 degrees of freedom
#> Multiple R-squared:  0.1621, Adjusted R-squared:  0.1619 
#> F-statistic: 966.8 on 1 and 4998 DF,  p-value: < 2.2e-16
#> 
#> TSLS fit summary:
#> 
#> Call:
#> ivreg::ivreg(formula = outcome ~ exposure | z)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -0.8304  0.1696  0.1696  0.2768  0.2768 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.83043    0.01341   61.94   <2e-16 ***
#> exposure    -1.10723    0.03120  -35.49   <2e-16 ***
#> 
#> Diagnostic tests:
#>                   df1  df2 statistic p-value    
#> Weak instruments    1 4998     966.8  <2e-16 ***
#> Wu-Hausman          1 4997     354.2  <2e-16 ***
#> Sargan              0   NA        NA      NA    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.4316 on 4998 degrees of freedom
#> Multiple R-Squared: 0.2282,  Adjusted R-squared: 0.2281 
#> Wald test:  1260 on 1 and 4998 DF,  p-value: < 2.2e-16 
#> 
#> 
#> Causal risk ratio with 95% CI:
#> [1] 1.107 1.048 1.170
```
