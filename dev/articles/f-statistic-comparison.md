# Comparison of conditional F-statistics

``` r
library(haven)
library(ivreg)
#> Registered S3 methods overwritten by 'ivreg':
#>   method              from
#>   anova.ivreg         AER 
#>   hatvalues.ivreg     AER 
#>   model.matrix.ivreg  AER 
#>   predict.ivreg       AER 
#>   print.ivreg         AER 
#>   print.summary.ivreg AER 
#>   summary.ivreg       AER 
#>   terms.ivreg         AER 
#>   update.ivreg        AER 
#>   vcov.ivreg          AER
library(AER)
#> Loading required package: car
#> Loading required package: carData
#> Loading required package: lmtest
#> Loading required package: zoo
#> 
#> Attaching package: 'zoo'
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric
#> Loading required package: sandwich
#> Loading required package: survival
#> 
#> Attaching package: 'AER'
#> The following objects are masked from 'package:ivreg':
#> 
#>     ivreg, ivreg.fit
library(estimatr)
library(fixest)
library(lfe)
#> Loading required package: Matrix
#> 
#> Attaching package: 'lfe'
#> The following object is masked from 'package:fixest':
#> 
#>     fepois
#> The following object is masked from 'package:lmtest':
#> 
#>     waldtest
library(OneSampleMR)
```

## Run fsw() on ivreg() model object

``` r
url <- "http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta"
dat <- haven::read_dta(url)
mod <- ivreg::ivreg(lwage ~ educ + exper | age + kidslt6 + kidsge6, data = dat)
summary(mod)
#> 
#> Call:
#> ivreg::ivreg(formula = lwage ~ educ + exper | age + kidslt6 + 
#>     kidsge6, data = dat)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -3.04973 -0.30711  0.05531  0.38952  2.27672 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)  
#> (Intercept) -0.360182   1.033416  -0.349    0.728  
#> educ         0.105836   0.080982   1.307    0.192  
#> exper        0.016153   0.007595   2.127    0.034 *
#> 
#> Diagnostic tests:
#>                          df1 df2 statistic p-value    
#> Weak instruments (educ)    3 424     4.466 0.00421 ** 
#> Weak instruments (exper)   3 424    55.044 < 2e-16 ***
#> Wu-Hausman                 2 423     0.004 0.99609    
#> Sargan                     1  NA     1.168 0.27976    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.669 on 425 degrees of freedom
#> Multiple R-Squared: 0.1482,  Adjusted R-squared: 0.1442 
#> Wald test: 3.034 on 2 and 425 DF,  p-value: 0.04917
fsw(mod)
#> 
#> Model sample size:  428 
#> 
#> Sanderson-Windmeijer conditional F-statistics for first stage model:
#>        F value d.f. Residual d.f.   Pr(>F)    
#> educ   6.69425    2           424 0.001373 ** 
#> exper 81.81237    2           424  < 2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Run fsw() on AER::ivreg() model object

``` r
mod2 <- AER::ivreg(lwage ~ educ + exper | age + kidslt6 + kidsge6, data = dat)
summary(mod2)
#> 
#> Call:
#> AER::ivreg(formula = lwage ~ educ + exper | age + kidslt6 + kidsge6, 
#>     data = dat)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -3.04973 -0.30711  0.05531  0.38952  2.27672 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)  
#> (Intercept) -0.360182   1.033416  -0.349    0.728  
#> educ         0.105836   0.080982   1.307    0.192  
#> exper        0.016153   0.007595   2.127    0.034 *
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.669 on 425 degrees of freedom
#> Multiple R-Squared: 0.1482,  Adjusted R-squared: 0.1442 
#> Wald test: 3.034 on 2 and 425 DF,  p-value: 0.04917
fsw(mod2)
#> 
#> Model sample size:  428 
#> 
#> Sanderson-Windmeijer conditional F-statistics for first stage model:
#>        F value d.f. Residual d.f.   Pr(>F)    
#> educ   6.69425    2           424 0.001373 ** 
#> exper 81.81237    2           424  < 2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Run fsw() on estimatr::iv_robust() model object

``` r
library(estimatr)
mod3 <- iv_robust(lwage ~ educ + exper | age + kidslt6 + kidsge6, data = dat, se_type = "classical")
tidy(mod3)
#>          term    estimate   std.error  statistic    p.value     conf.low
#> 1 (Intercept) -0.36018214 1.033415610 -0.3485356 0.72761056 -2.391424035
#> 2        educ  0.10583608 0.080981803  1.3069119 0.19194932 -0.053338627
#> 3       exper  0.01615273 0.007594673  2.1268499 0.03400801  0.001224933
#>    conf.high  df outcome
#> 1 1.67105975 425   lwage
#> 2 0.26501080 425   lwage
#> 3 0.03108053 425   lwage
fsw(mod3)
#> 
#> Model sample size:  428 
#> 
#> Sanderson-Windmeijer conditional F-statistics for first stage model:
#>        F value d.f. Residual d.f.   Pr(>F)    
#> educ   6.69425    2           424 0.001373 ** 
#> exper 81.81237    2           424  < 2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Run fsw() on fixest::feols() model object

``` r
library(fixest)
mod4 <- feols(lwage ~ 1 | educ + exper ~ age + kidslt6 + kidsge6, data = dat)
#> NOTE: 325 observations removed because of NA values (LHS: 325).
summary(mod4)
#> TSLS estimation - Dep. Var.: lwage
#>                   Endo.    : educ, exper
#>                   Instr.   : age, kidslt6, kidsge6
#> Second stage: Dep. Var.: lwage
#> Observations: 428
#> Standard-errors: IID 
#>              Estimate Std. Error   t value Pr(>|t|)    
#> (Intercept) -0.360182   1.033416 -0.348536 0.727611    
#> fit_educ     0.105836   0.080982  1.306912 0.191949    
#> fit_exper    0.016153   0.007595  2.126850 0.034008 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> RMSE: 0.666683   Adj. R2: 0.007512
#> F-test (1st stage), educ : stat =  4.46617, p = 0.00421 , on 3 and 424 DoF.
#> F-test (1st stage), exper: stat = 55.04436, p < 2.2e-16 , on 3 and 424 DoF.
#>                Wu-Hausman: stat =  0.00392, p = 0.996088, on 2 and 423 DoF.
#>                    Sargan: stat =  1.16824, p = 0.279764, on 1 DoF.
fsw(mod4)
#> 
#> Model sample size:  428 
#> 
#> Sanderson-Windmeijer conditional F-statistics for first stage model:
#>        F value d.f. Residual d.f.   Pr(>F)    
#> educ   6.69425    2           424 0.001373 ** 
#> exper 81.81237    2           424  < 2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Comparison with F-statistic from lfe package

``` r
modst2 <- felm(lwage ~ 1 |
                 0 | (educ | exper ~ age + kidslt6 + kidsge6), data = dat)
summary(modst2)
#> 
#> Call:
#>    felm(formula = lwage ~ 1 | 0 | (educ | exper ~ age + kidslt6 +      kidsge6), data = dat) 
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -3.04973 -0.30711  0.05531  0.38952  2.27672 
#> 
#> Coefficients:
#>               Estimate Std. Error t value Pr(>|t|)  
#> (Intercept)  -0.360182   1.033416  -0.349    0.728  
#> `educ(fit)`   0.105836   0.080982   1.307    0.192  
#> `exper(fit)`  0.016153   0.007595   2.127    0.034 *
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.669 on 425 degrees of freedom
#>   (325 observations deleted due to missingness)
#> Multiple R-squared(full model): 0.1482   Adjusted R-squared: 0.1442 
#> Multiple R-squared(proj model): 0.1482   Adjusted R-squared: 0.1442 
#> F-statistic(full model):3.034 on 2 and 425 DF, p-value: 0.04917 
#> F-statistic(proj model): 3.034 on 2 and 425 DF, p-value: 0.04917 
#> F-statistic(endog. vars):3.034 on 2 and 425 DF, p-value: 0.04917
t(sapply(modst2$stage1$lhs, function(lh) {
  waldtest(modst2$stage1, ~ age | kidslt6 | kidsge6, lhs = lh)
}))
#>                  p      chi2 df1          p.F         F df2
#> educ  3.849465e-03  13.39851   3 4.210326e-03  4.466172 424
#> exper 1.429780e-35 165.13309   3 4.561549e-30 55.044363 424
condfstat(modst2, quantiles = c(0.025, 0.975))
#>           educ    exper
#> iid F 6.710039 82.00533
#> attr(,"df1")
#> [1] 2
#> attr(,"quantiles")
#>                2.5%      97.5%
#> educ  -0.0808003007 0.25850072
#> exper  0.0008784942 0.03091819
#> attr(,"quantiles")attr(,"q")
#> [1] 0.025 0.975
#> attr(,"quantiles")attr(,"samples")
#> [1] 100
```

## Comparison with output from ivreg2

Code run using Stata version 16.1.

``` stata
use http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta, clear

// ssc install ivreg2
ivreg2 lwage (educ exper = age kidslt6 kidsge6) if !missing(lwage, educ, exper, age, kidslt6, kidsge6), first
```

    First-stage regressions
    -----------------------


    First-stage regression of educ:

    Statistics consistent for homoskedasticity only
    Number of obs =                    428
    ------------------------------------------------------------------------------
            educ |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
    -------------+----------------------------------------------------------------
             age |  -.0185412   .0163449    -1.13   0.257    -.0506683    .0135859
         kidslt6 |   .6984283   .2966854     2.35   0.019     .1152709    1.281586
         kidsge6 |   -.222821   .0906154    -2.46   0.014    -.4009324   -.0447096
           _cons |   13.64009   .7644499    17.84   0.000     12.13751    15.14268
    ------------------------------------------------------------------------------
    F test of excluded instruments:
      F(  3,   424) =     4.47
      Prob > F      =   0.0042
    Sanderson-Windmeijer multivariate F test of excluded instruments:
      F(  2,   424) =     6.69
      Prob > F      =   0.0014


    First-stage regression of exper:

    Statistics consistent for homoskedasticity only
    Number of obs =                    428
    ------------------------------------------------------------------------------
           exper |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
    -------------+----------------------------------------------------------------
             age |   .3948754   .0496446     7.95   0.000     .2972953    .4924555
         kidslt6 |  -.7469412   .9011267    -0.83   0.408    -2.518173    1.024291
         kidsge6 |  -1.430306   .2752275    -5.20   0.000    -1.971286   -.8893254
           _cons |  -1.500019   2.321874    -0.65   0.519    -6.063837    3.063798
    ------------------------------------------------------------------------------
    F test of excluded instruments:
      F(  3,   424) =    55.04
      Prob > F      =   0.0000
    Sanderson-Windmeijer multivariate F test of excluded instruments:
      F(  2,   424) =    81.81
      Prob > F      =   0.0000



    Summary results for first-stage regressions
    -------------------------------------------

                                               (Underid)            (Weak id)
    Variable     | F(  3,   424)  P-val | SW Chi-sq(  2) P-val | SW F(  2,   424)
    educ         |       4.47    0.0042 |       13.51   0.0012 |        6.69
    exper        |      55.04    0.0000 |      165.17   0.0000 |       81.81

    Stock-Yogo weak ID F test critical values for single endogenous regressor:
                                        5% maximal IV relative bias    13.91
                                       10% maximal IV relative bias     9.08
                                       20% maximal IV relative bias     6.46
                                       30% maximal IV relative bias     5.39
                                       10% maximal IV size             22.30
                                       15% maximal IV size             12.83
                                       20% maximal IV size              9.54
                                       25% maximal IV size              7.80
    Source: Stock-Yogo (2005).  Reproduced by permission.
    NB: Critical values are for Sanderson-Windmeijer F statistic.

    Underidentification test
    Ho: matrix of reduced form coefficients has rank=K1-1 (underidentified)
    Ha: matrix has rank=K1 (identified)
    Anderson canon. corr. LM statistic       Chi-sq(2)=13.10    P-val=0.0014

    Weak identification test
    Ho: equation is weakly identified
    Cragg-Donald Wald F statistic                                       4.46

    Stock-Yogo weak ID test critical values for K1=2 and L1=3:
                                       10% maximal IV size             13.43
                                       15% maximal IV size              8.18
                                       20% maximal IV size              6.40
                                       25% maximal IV size              5.45
    Source: Stock-Yogo (2005).  Reproduced by permission.

    Weak-instrument-robust inference
    Tests of joint significance of endogenous regressors B1 in main equation
    Ho: B1=0 and orthogonality conditions are valid
    Anderson-Rubin Wald test           F(3,424)=       2.08     P-val=0.1025
    Anderson-Rubin Wald test           Chi-sq(3)=      6.29     P-val=0.0983
    Stock-Wright LM S statistic        Chi-sq(3)=      6.20     P-val=0.1023

    Number of observations               N  =        428
    Number of regressors                 K  =          3
    Number of endogenous regressors      K1 =          2
    Number of instruments                L  =          4
    Number of excluded instruments       L1 =          3

    IV (2SLS) estimation
    --------------------

    Estimates efficient for homoskedasticity only
    Statistics consistent for homoskedasticity only

                                                          Number of obs =      428
                                                          F(  2,   425) =     3.03
                                                          Prob > F      =   0.0492
    Total (centered) SS     =  223.3274513                Centered R2   =   0.1482
    Total (uncentered) SS   =   829.594813                Uncentered R2 =   0.7707
    Residual SS             =  190.2315236                Root MSE      =    .6667

    ------------------------------------------------------------------------------
           lwage |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
    -------------+----------------------------------------------------------------
            educ |   .1058361   .0806975     1.31   0.190    -.0523281    .2640003
           exper |   .0161527    .007568     2.13   0.033     .0013197    .0309858
           _cons |  -.3601821   1.029787    -0.35   0.727    -2.378528    1.658164
    ------------------------------------------------------------------------------
    Underidentification test (Anderson canon. corr. LM statistic):          13.101
                                                       Chi-sq(2) P-val =    0.0014
    ------------------------------------------------------------------------------
    Weak identification test (Cragg-Donald Wald F statistic):                4.463
    Stock-Yogo weak ID test critical values: 10% maximal IV size             13.43
                                             15% maximal IV size              8.18
                                             20% maximal IV size              6.40
                                             25% maximal IV size              5.45
    Source: Stock-Yogo (2005).  Reproduced by permission.
    ------------------------------------------------------------------------------
    Sargan statistic (overidentification test of all instruments):           1.168
                                                       Chi-sq(1) P-val =    0.2798
    ------------------------------------------------------------------------------
    Instrumented:         educ exper
    Excluded instruments: age kidslt6 kidsge6
    ------------------------------------------------------------------------------
