# Two-stage residual inclusion (TSRI) estimators

An excellent description of TSRI estimators is given by Terza et al.
(2008). TSRI estimators proceed by fitting a first stage model of the
exposure regressed upon the instruments (and possibly any measured
confounders). From this the first stage residuals are estimated. A
second stage model is then fitted of the outcome regressed upon the
exposure and first stage residuals (and possibly measured confounders).

## Usage

``` r
tsri(
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

An object of class `"tsri"` with the following elements

- fit:

  the fitted object of class `"gmm"` from the call to
  [`gmm::gmm()`](https://rdrr.io/pkg/gmm/man/gmm.html).

- estci:

  a matrix of the estimates with their corresponding confidence interval
  limits.

- link:

  a character vector containing the specified link function.

## Details

TSRI estimators are sometimes described as a special case of control
function estimators.

`tsri()` performs GMM estimation to ensure appropriate standard errors
on its estimates similar to that described that described by Clarke et
al. (2015). Terza (2017) described an alternative approach.

## References

Bowden J, Vansteelandt S. Mendelian randomization analysis of
case-control data using structural mean models. Statistics in Medicine,
2011, 30, 6, 678-694.
[doi:10.1002/sim.4138](https://doi.org/10.1002/sim.4138)

Clarke PS, Palmer TM, Windmeijer F. Estimating structural mean models
with multiple instrumental variables using the Generalised Method of
Moments. Statistical Science, 2015, 30, 1, 96-117.
[doi:10.1214/14-STS503](https://doi.org/10.1214/14-STS503)

Dukes O, Vansteelandt S. A note on G-estimation of causal risk ratios.
American Journal of Epidemiology, 2018, 187, 5, 1079-1084.
[doi:10.1093/aje/kwx347](https://doi.org/10.1093/aje/kwx347)

Palmer T, Thompson JR, Tobin MD, Sheehan NA, Burton PR. Adjusting for
bias and unmeasured confounding in Mendelian randomization studies with
binary responses. International Journal of Epidemiology, 2008, 37, 5,
1161-1168. [doi:10.1093/ije/dyn080](https://doi.org/10.1093/ije/dyn080)

Palmer TM, Sterne JAC, Harbord RM, Lawlor DA, Sheehan NA, Meng S,
Granell R, Davey Smith G, Didelez V. Instrumental variable estimation of
causal risk ratios and causal odds ratios in Mendelian randomization
analyses. American Journal of Epidemiology, 2011, 173, 12, 1392-1403.
[doi:10.1093/aje/kwr026](https://doi.org/10.1093/aje/kwr026)

Terza JV, Basu A, Rathouz PJ. Two-stage residual inclusion estimation:
Addressing endogeneity in health econometric modeling. Journal of Health
Economics, 2008, 27, 3, 531-543.
[doi:10.1016/j.jhealeco.2007.09.009](https://doi.org/10.1016/j.jhealeco.2007.09.009)

Terza JV. Two-stage residual inclusion estimation: A practitioners guide
to Stata implementation. The Stata Journal, 2017, 17, 4, 916-938.
[doi:10.1177/1536867X1801700409](https://doi.org/10.1177/1536867X1801700409)

## Examples

``` r
# Two-stage residual inclusion estimator
# with second stage logistic regression
set.seed(9)
n            <- 1000
psi0         <- 0.5
Z            <- rbinom(n, 1, 0.5)
X            <- rbinom(n, 1, 0.7*Z + 0.2*(1 - Z))
m0           <- plogis(1 + 0.8*X - 0.39*Z)
Y            <- rbinom(n, 1, plogis(psi0*X + log(m0/(1 - m0))))
dat          <- data.frame(Z, X, Y)
tsrilogitfit <- tsri(Y ~ X | Z , data = dat, link = "logit")
summary(tsrilogitfit)
#> 
#> GMM fit summary:
#> 
#> Call:
#> gmm::gmm(g = tsriLogitMoments, x = dat, t0 = t0, vcov = "iid")
#> 
#> 
#> Method:  twoStep 
#> 
#> Coefficients:
#>               Estimate    Std. Error  t value     Pr(>|t|)  
#> Z(Intercept)  1.7647e-01  1.7169e-02  1.0278e+01  8.8297e-25
#> ZZ            5.4740e-01  2.6249e-02  2.0854e+01  1.4138e-96
#> (Intercept)   1.0068e+00  1.4482e-01  6.9520e+00  3.6002e-12
#> X             6.7047e-01  2.8436e-01  2.3578e+00  1.8382e-02
#> res           4.4657e-01  3.4141e-01  1.3080e+00  1.9086e-01
#> 
#> J-Test: degrees of freedom is 0 
#>                 J-test                P-value             
#> Test E(g)=0:    4.28905429706042e-23  *******             
#> 
#> #############
#> Information related to the numerical optimization
#> Convergence code =  0 
#> Function eval. =  208 
#> Gradian eval. =  NA 
#> 
#> Estimates with 95% CI limits:
#>              Estimate   0.025  0.975
#> Z(Intercept)   0.1765  0.1428 0.2101
#> ZZ             0.5474  0.4959 0.5988
#> (Intercept)    1.0068  0.7229 1.2906
#> X              0.6705  0.1131 1.2278
#> res            0.4466 -0.2226 1.1157
#> 
#> Causal odds ratio with 95% CI limits:
#>             Estimate  0.025 0.975
#> (Intercept)    2.737 2.0605 3.635
#> X              1.955 1.1198 3.414
#> res            1.563 0.8005 3.052
#> 
```
