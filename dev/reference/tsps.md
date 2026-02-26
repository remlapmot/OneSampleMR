# Two-stage predictor substitution (TSPS) estimators

Terza et al. (2008) give an excellent description of TSPS estimators.
They proceed by fitting a first stage model of the exposure regressed
upon the instruments (and possibly any measured confounders). From this
the predicted values of the exposure are obtained. A second stage model
is then fitted of the outcome regressed upon the predicted values of the
exposure (and possibly measured confounders).

## Usage

``` r
tsps(
  formula,
  instruments,
  data,
  subset,
  na.action,
  contrasts = NULL,
  t0 = NULL,
  link = "identity",
  ...
)
```

## Arguments

- formula, instruments:

  formula specification(s) of the regression relationship and the
  instruments. Either `instruments` is missing and `formula` has three
  parts as in `y ~ x1 + x2 | z1 + z2 + z3` (recommended) or `formula` is
  `y ~ x1 + x2` and `instruments` is a one-sided formula
  `~ z1 + z2 + z3` (only for backward compatibility).

- data:

  an optional data frame containing the variables in the model. By
  default the variables are taken from the environment of the `formula`.

- subset:

  an optional vector specifying a subset of observations to be used in
  fitting the model.

- na.action:

  a function that indicates what should happen when the data contain
  `NA`s. The default is set by the `na.action` option.

- contrasts:

  an optional list. See the `contrasts.arg` of
  [`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html).

- t0:

  A vector of starting values for the gmm optimizer. This should have
  length equal to the number of exposures plus 1.

- link:

  character; one of `"identity"` (the default), `"logadd"`, `"logmult"`,
  `"logit"`. This is the link function for the second stage model.
  `"identity"` corresponds to linear regression; `"logadd"` is
  log-additive and corresponds to Poisson / log-binomial regression;
  `"logmult"` is log-multiplicative and corresponds to gamma regression;
  `"logit"` corresponds to logistic regression.

- ...:

  further arguments passed to or from other methods.

## Value

An object of class `"tsps"` with the following elements

- fit:

  the fitted object of class `"gmm"` from the call to
  [`gmm::gmm()`](https://rdrr.io/pkg/gmm/man/gmm.html).

- estci:

  a matrix of the estimates with their corresponding confidence interval
  limits.

- link:

  a character vector containing the specified link function.

## Details

`tsps()` performs GMM estimation to ensure appropriate standard errors
on its estimates similar to the approach described in Clarke et al.
(2015).

## References

Burgess S, CRP CHD Genetics Collaboration. Identifying the odds ratio
estimated by a two-stage instrumental variable analysis with a logistic
regression model. Statistics in Medicine, 2013, 32, 27, 4726-4747.
[doi:10.1002/sim.5871](https://doi.org/10.1002/sim.5871)

Clarke PS, Palmer TM, Windmeijer F. Estimating structural mean models
with multiple instrumental variables using the Generalised Method of
Moments. Statistical Science, 2015, 30, 1, 96-117.
[doi:10.1214/14-STS503](https://doi.org/10.1214/14-STS503)

Dukes O, Vansteelandt S. A note on G-estimation of causal risk ratios.
American Journal of Epidemiology, 2018, 187, 5, 1079-1084.
[doi:10.1093/aje/kwx347](https://doi.org/10.1093/aje/kwx347)

Palmer TM, Sterne JAC, Harbord RM, Lawlor DA, Sheehan NA, Meng S,
Granell R, Davey Smith G, Didelez V. Instrumental variable estimation of
causal risk ratios and causal odds ratios in Mendelian randomization
analyses. American Journal of Epidemiology, 2011, 173, 12, 1392-1403.
[doi:10.1093/aje/kwr026](https://doi.org/10.1093/aje/kwr026)

Terza JV, Basu A, Rathouz PJ. Two-stage residual inclusion estimation:
Addressing endogeneity in health econometric modeling. Journal of Health
Economics, 2008, 27, 3, 531-543.
[doi:10.1016/j.jhealeco.2007.09.009](https://doi.org/10.1016/j.jhealeco.2007.09.009)

## Examples

``` r
# Two-stage predictor substitution estimator
# with second stage logistic regression
set.seed(9)
n            <- 1000
psi0         <- 0.5
Z            <- rbinom(n, 1, 0.5)
X            <- rbinom(n, 1, 0.7*Z + 0.2*(1 - Z))
m0           <- plogis(1 + 0.8*X - 0.39*Z)
Y            <- rbinom(n, 1, plogis(psi0*X + log(m0/(1 - m0))))
dat          <- data.frame(Z, X, Y)
tspslogitfit <- tsps(Y ~ X | Z , data = dat, link = "logit")
summary(tspslogitfit)
#> 
#> GMM fit summary:
#> 
#> Call:
#> gmm::gmm(g = tspsLogitMoments, x = dat, t0 = t0, vcov = "iid")
#> 
#> 
#> Method:  twoStep 
#> 
#> Coefficients:
#>               Estimate    Std. Error  t value     Pr(>|t|)  
#> Z(Intercept)  1.7647e-01  1.7169e-02  1.0278e+01  8.8297e-25
#> ZZ            5.4740e-01  2.6249e-02  2.0854e+01  1.4138e-96
#> (Intercept)   9.7133e-01  1.4042e-01  6.9173e+00  4.6027e-12
#> xhat          6.1451e-01  2.7464e-01  2.2375e+00  2.5256e-02
#> 
#> J-Test: degrees of freedom is 0 
#>                 J-test               P-value            
#> Test E(g)=0:    8.6661572346798e-23  *******            
#> 
#> #############
#> Information related to the numerical optimization
#> Convergence code =  0 
#> Function eval. =  169 
#> Gradian eval. =  NA 
#> 
#> Estimates with 95% CI limits:
#>              Estimate   0.025  0.975
#> Z(Intercept)   0.1765 0.14282 0.2101
#> ZZ             0.5474 0.49595 0.5988
#> (Intercept)    0.9713 0.69611 1.2465
#> xhat           0.6145 0.07622 1.1528
#> 
#> Causal odds ratio with 95% CI limits:
#>             Estimate 0.025 0.975
#> (Intercept)    2.641 2.006 3.478
#> xhat           1.849 1.079 3.167
#> 
```
