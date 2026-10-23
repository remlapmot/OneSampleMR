# Logistic structural mean model

Function to estimate the double logistic structural mean model (LSMM) of
Vansteelandt and Goetghebeur (2003) by the generalized method of moments
(GMM), as described by Clarke et al. (2015).

## Usage

``` r
lsmm(
  formula,
  instruments,
  data,
  subset,
  na.action,
  contrasts = NULL,
  t0 = NULL,
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
  length equal to the number of association model parameters (the
  intercept, exposures, instruments, and covariates), plus the number of
  exposures and covariates, plus 1. By default the starting values are
  the estimates from a logistic regression for the association model and
  from a two-stage residual inclusion logistic regression for the causal
  model.

- ...:

  further arguments passed to or from other methods.

## Value

An object of class `"lsmm"`. A list with the following items:

- fit:

  The object from the second step
  [`gmm::gmm()`](https://rdrr.io/pkg/gmm/man/gmm.html) fit.

- corci:

  The causal odds ratio/s of the exposure/s and the corresponding 95%
  confidence interval limits.

- ey0ci:

  The estimate of the exposure free potential outcome mean and its 95%
  confidence interval limits.

- covariates:

  A character vector of the names of any covariates.

## Details

The LSMM is \$\$\mathrm{logit}\\ E(Y \mid X, Z) - \mathrm{logit}\\
E\\Y(0) \mid X, Z\\ = \psi X,\$\$ so that \\\exp(\psi)\\ is a causal
odds ratio among the exposed. Unlike the additive and multiplicative
structural mean models, the LSMM also requires a model for the observed
outcome mean, the association model, which here is a logistic regression
of the outcome on the exposures, the instruments, and any covariates.
The association model and the causal model are fitted jointly by
two-step GMM. The moment conditions for the association model are its
logistic regression score equations, and those for the causal model are
that the predicted exposure-free potential outcome is mean independent
of the instruments. The initial weight matrix is block diagonal, with
blocks equal to the inverse of the crossproduct of the regressors of
each model, and the second step uses the optimal weight matrix. This is
the same estimator as the `ivlsmm` command in the Stata package
ivonesamplemr.

Covariates are adjusted for by including them in both the exposure and
the instrument parts of the formula, as in `y ~ x + w | z1 + z2 + w`.
They are then included in both the association model and the causal
model, as in `ivlsmm`.

## References

Clarke PS, Palmer TM, Windmeijer F. Estimating structural mean models
with multiple instrumental variables using the Generalised Method of
Moments. Statistical Science, 2015, 30, 1, 96-117.
[doi:10.1214/14-STS503](https://doi.org/10.1214/14-STS503)

Vansteelandt S, Goetghebeur E. Causal inference with generalized
structural mean models. Journal of the Royal Statistical Society: Series
B, 2003, 65, 4, 817-835.
[doi:10.1046/j.1369-7412.2003.00417.x](https://doi.org/10.1046/j.1369-7412.2003.00417.x)

Vansteelandt S, Bowden J, Babanezhad M, Goetghebeur E. On instrumental
variables estimation of causal odds ratios. Statistical Science, 2011,
26, 3, 403-422.
[doi:10.1214/11-STS360](https://doi.org/10.1214/11-STS360)

## Examples

``` r
# Multiple instrument example
set.seed(123456)
n    <- 1000
psi0 <- 0.5
G1   <- rbinom(n, 2, 0.5)
G2   <- rbinom(n, 2, 0.3)
G3   <- rbinom(n, 2, 0.4)
U    <- runif(n)
pX   <- plogis(0.7*G1 + G2 - G3 + U)
X    <- rbinom(n, 1, pX)
pY   <- plogis(-2 + psi0*X + U)
Y    <- rbinom(n, 1, pY)
dat  <- data.frame(G1, G2, G3, X, Y)
fit  <- lsmm(Y ~ X | G1 + G2 + G3, data = dat)
fit
#> 
#> Method
#>  twoStep 
#> 
#> Objective function value:  0.0002256856 
#> 
#> assoc:(Intercept)            assoc:X           assoc:G1           assoc:G2  
#>        -1.3845855          0.4111755         -0.0016541         -0.0030709  
#>          assoc:G3                  X            E[Y(0)]  
#>         0.0040531          0.3908401          0.2024624  
#> 
#> Convergence code =  0 
#> 
#> E[Y(0)] with 95% CI:
#>         0.025  0.975 
#> 0.2025 0.1156 0.2893 
#> 
#> Causal odds ratio with 95% CI:
#>     COR  0.025 0.975
#> X 1.478 0.6991 3.126
#> 
summary(fit)
#> 
#> GMM fit summary:
#> 
#> Call:
#> gmm::gmm(g = lsmmMoments, x = dat, t0 = gmm::coef.gmm(fit1), 
#>     gradv = lsmmGradient, vcov = "TrueFixed", weightsMatrix = w1, 
#>     method = "BFGS", control = list(reltol = 1e-14, maxit = 10000))
#> 
#> 
#> Method:  twoStep 
#> 
#> Coefficients:
#>                    Estimate     Std. Error   t value      Pr(>|t|)   
#> assoc:(Intercept)  -1.3846e+00   1.5007e-01  -9.2263e+00   2.8027e-20
#> assoc:X             4.1118e-01   1.8523e-01   2.2198e+00   2.6430e-02
#> assoc:G1           -1.6541e-03   3.4843e-02  -4.7472e-02   9.6214e-01
#> assoc:G2           -3.0709e-03   6.5309e-02  -4.7021e-02   9.6250e-01
#> assoc:G3            4.0531e-03   8.6035e-02   4.7109e-02   9.6243e-01
#> X                   3.9084e-01   3.8207e-01   1.0230e+00   3.0633e-01
#> E[Y(0)]             2.0246e-01   4.4323e-02   4.5679e+00   4.9262e-06
#> 
#> J-Test: degrees of freedom is 2 
#>                 J-test   P-value
#> Test E(g)=0:    0.22569  0.89329
#> 
#> #############
#> Information related to the numerical optimization
#> Convergence code =  0 
#> Function eval. =  271 
#> Gradian eval. =  102 
#> 
#> E[Y(0)] with 95% CI:
#>         0.025  0.975 
#> 0.2025 0.1156 0.2893 
#> 
#> Causal odds ratio with 95% CI:
#>     COR  0.025 0.975
#> X 1.478 0.6991 3.126
#> 
```
