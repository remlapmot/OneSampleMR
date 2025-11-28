# Multiplicative structural mean model

Function providing several methods to estimate the multiplicative
structural mean model (MSMM) of Robins (1989).

## Usage

``` r
msmm(
  formula,
  instruments,
  data,
  subset,
  na.action,
  contrasts = NULL,
  estmethod = c("gmm", "gmmalt", "tsls", "tslsalt"),
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

- estmethod:

  Estimation method, please use one of

  - `"gmm"` GMM estimation of the MSMM (the default).

  - `"gmmalt"` GMM estimation of the alternative moment conditions for
    the MSMM as per Clarke et al. (2015). These are the same moment
    conditions fit by the user-written Stata command `ivpois`
    (Nichols, 2007) and by the official Stata command
    `ivpoisson gmm ..., multiplicative` (StataCorp., 2013).

  - `"tsls"` the TSLS method of fitting the MSMM of Clarke et al.
    (2015). For binary \\Y\\ and \\X\\ this uses \\Y\*(1-X)\\ as the
    outcome and \\Y\*X\\ as the exposure.

  - `"tslsalt"` the alternative TSLS method of fitting the MSMM of
    Clarke et al. (2015). For binary \\Y\\ and \\X\\ this uses \\Y\*X\\
    as the outcome and \\Y\*(1-X)\\ as the exposure.

- t0:

  A vector of starting values for the gmm optimizer. This should have
  length equal to the number of exposures plus 1.

- ...:

  further arguments passed to or from other methods.

## Value

An object of class `"msmm"`. A list with the following items:

- fit:

  The object from either a
  [`gmm::gmm()`](https://rdrr.io/pkg/gmm/man/gmm.html) or
  [`ivreg::ivreg()`](https://zeileis.github.io/ivreg/reference/ivreg.html)
  fit.

- crrci:

  The causal risk ratio/s and it corresponding 95% confidence interval
  limits.

- estmethod:

  The specified `estmethod`.

If `estmethod` is `"tsls"`, `"gmm"`, or `"gmmalt"`:

- ey0ci:

  The estimate of the treatment/exposure free potential outcome and its
  95% confidence interval limits.

If `estmethod` is `"tsls"` or `"tslsalt"`:

- stage1:

  An object containing the first stage regression from an
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html) fit.

## Details

Function providing several methods to estimate the multiplicative
structural mean model (MSMM) of Robins (1989). These are the methods
described in Clarke et al. (2015), most notably generalised method of
moments (GMM) estimation of the MSMM.

An equivalent estimator to the MSMM was proposed in Econometrics by
Mullahy (1997) and then discussed in several articles by Windmeijer
(1997, 2002) and Cameron and Trivedi (2013). This was implemented in the
user-written Stata command `ivpois` (Nichols, 2007) and then implemented
in official Stata in the `ivpoisson` command (StataCorp., 2013).

## References

Cameron AC, Trivedi PK. Regression analysis of count data. 2nd ed. 2013.
New York, Cambridge University Press. ISBN:1107667275

Clarke PS, Palmer TM, Windmeijer F. Estimating structural mean models
with multiple instrumental variables using the Generalised Method of
Moments. Statistical Science, 2015, 30, 1, 96-117.
[doi:10.1214/14-STS503](https://doi.org/10.1214/14-STS503)

Hernán and Robins. Instruments for causal inference: An Epidemiologist's
dream? Epidemiology, 2006, 17, 360-372.
[doi:10.1097/01.ede.0000222409.00878.37](https://doi.org/10.1097/01.ede.0000222409.00878.37)

Mullahy J. Instrumental-variable estimation of count data models:
applications to models of cigarette smoking and behavior. The Review of
Economics and Statistics. 1997, 79, 4, 586-593.
[doi:10.1162/003465397557169](https://doi.org/10.1162/003465397557169)

Nichols A. ivpois: Stata module for IV/GMM Poisson regression. 2007.
[url](https://ideas.repec.org/c/boc/bocode/s456890.html)

Palmer TM, Sterne JAC, Harbord RM, Lawlor DA, Sheehan NA, Meng S,
Granell R, Davey Smith G, Didelez V. Instrumental variable estimation of
causal risk ratios and causal odds ratios in Mendelian randomization
analyses. American Journal of Epidemiology, 2011, 173, 12, 1392-1403.
[doi:10.1093/aje/kwr026](https://doi.org/10.1093/aje/kwr026)

Robins JM. The analysis of randomised and nonrandomised AIDS treatment
trials using a new approach to causal inference in longitudinal studies.
In Health Service Research Methodology: A Focus on AIDS (L. Sechrest, H.
Freeman and A. Mulley, eds.). 1989. 113–159. US Public Health Service,
National Center for Health Services Research, Washington, DC.

StataCorp. Stata Base Reference Manual. Release 13. ivpoisson - Poisson
model with continuous endogenous covariates. 2013.
[url](https://www.stata.com/manuals13/rivpoisson.pdf)

Windmeijer FAG, Santos Silva JMC. Endogeneity in Count Data Models: An
Application to Demand for Health Care. Journal of Applied Econometrics.
1997, 12, 3, 281-294. [doi:10/fdkh4n](https://doi.org/10/fdkh4n)

Windmeijer, F. ExpEnd, A Gauss programme for non-linear GMM estimation
of EXPonential models with ENDogenous regressors for cross section and
panel data. CEMMAP working paper CWP14/02. 2002.
[url](https://www.cemmap.ac.uk/wp-content/uploads/2020/08/CWP1402.pdf)

## Examples

``` r
# Single instrument example
# Data generation from the example in the ivtools ivglm() helpfile
set.seed(9)
n    <- 1000
psi0 <- 0.5
Z    <- rbinom(n, 1, 0.5)
X    <- rbinom(n, 1, 0.7*Z + 0.2*(1 - Z))
m0   <- plogis(1 + 0.8*X - 0.39*Z)
Y    <- rbinom(n, 1, plogis(psi0*X + log(m0/(1 - m0))))
dat  <- data.frame(Z, X, Y)
fit  <- msmm(Y ~ X | Z, data = dat)
summary(fit)
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
#>           Estimate     Std. Error   t value      Pr(>|t|)   
#> Theta[1]   7.2784e-01   2.5893e-02   2.8110e+01  7.4591e-174
#> Theta[2]   1.3107e-01   6.0307e-02   2.1733e+00   2.9754e-02
#> 
#> J-Test: degrees of freedom is 0 
#>                 J-test                P-value             
#> Test E(g)=0:    1.73561467137432e-06  *******             
#> 
#> #############
#> Information related to the numerical optimization
#> Convergence code =  0 
#> Function eval. =  63 
#> Gradian eval. =  NA 
#> 
#> E[Y(0)] with 95% CI:
#>         0.025  0.975 
#> 0.7278 0.6771 0.7786 
#> 
#> Causal risk ratio with 95% CI:
#>    CRR 0.025 0.975
#> X 1.14 1.013 1.283
#> 

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
dat2 <- data.frame(G1, G2, G3, X, Y)
fit2 <- msmm(Y ~ X | G1 + G2 + G3, data = dat2)
summary(fit2)
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
#> Theta[1]  0.18250038  0.05196181  3.51220233  0.00044441
#> Theta[2]  0.45163610  0.41838256  1.07948119  0.28037327
#> 
#> J-Test: degrees of freedom is 2 
#>                 J-test   P-value
#> Test E(g)=0:    0.27121  0.87319
#> 
#> Initial values of the coefficients
#>  Theta[1]  Theta[2] 
#> 0.1831035 0.4392681 
#> 
#> #############
#> Information related to the numerical optimization
#> Convergence code =  0 
#> Function eval. =  59 
#> Gradian eval. =  NA 
#> 
#> E[Y(0)] with 95% CI:
#>           0.025   0.975 
#> 0.18250 0.08066 0.28434 
#> 
#> Causal risk ratio with 95% CI:
#>     CRR  0.025 0.975
#> X 1.571 0.6919 3.567
#> 
```
