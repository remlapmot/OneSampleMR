# ivreg package

library(ivreg)

## data
data("CigaretteDemand", package = "ivreg")

## model
m <- ivreg(log(packs) ~ log(rprice) + log(rincome) | salestax + log(rincome),
           data = CigaretteDemand)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# lfe package - modified example from condfstat() helpfile

library(lfe)

set.seed(12345)
n <- 4000

z1 <- rnorm(n)
z2 <- rnorm(n)
u <- rnorm(n)
# make x1, x2 correlated with errors u

x1 <- z1 + z2 + 0.2*u + rnorm(n)
x2 <- z1 + 0.94*z2 - 0.3*u + rnorm(n)
y <- x1 + x2 + u
est <- felm(y ~ 1 | 0 | (x1 | x2 ~ z1 + z2))
summary(est)
## Not run:
summary(est$stage1, lhs='x1')
summary(est$stage1, lhs='x2')

## End(Not run)

# the joint significance of the instruments in both the first stages are ok:
t(sapply(est$stage1$lhs, function(lh) waldtest(est$stage1, ~ z1|z2, lhs = lh)))
# everything above looks fine, t-tests for instruments,
# as well as F-tests for excluded instruments in the 1st stages.
# The conditional F-test reveals that the instruments are jointly weak
# (it's close to being only one instrument, z1+z2, for both x1 and x2)
condfstat(est, quantiles = c(0.05, 0.95))


# Stata ivreg2 example

library(haven)
url <- "http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta"
dat <- haven::read_dta(url)

modst <- ivreg(lwage ~ educ + exper | age + kidslt6 + kidsge6, data = dat)
summary(modst)
# fsw(modst)

# . use http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta, clear
#
# .
# . ivreg2 lwage (educ exper = age kidslt6 kidsge6), first
#
# First-stage regressions
# -----------------------
#
#
#   First-stage regression of educ:
#
#   Statistics consistent for homoskedasticity only
# Number of obs =                    428
# ------------------------------------------------------------------------------
#   educ |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   age |  -.0185412   .0163449    -1.13   0.257    -.0506683    .0135859
# kidslt6 |   .6984283   .2966854     2.35   0.019     .1152709    1.281586
# kidsge6 |   -.222821   .0906154    -2.46   0.014    -.4009324   -.0447096
# _cons |   13.64009   .7644499    17.84   0.000     12.13751    15.14268
# ------------------------------------------------------------------------------
#   F test of excluded instruments:
#   F(  3,   424) =     4.47
# Prob > F      =   0.0042
# Sanderson-Windmeijer multivariate F test of excluded instruments:
#   F(  2,   424) =     6.69
# Prob > F      =   0.0014
#
#
# First-stage regression of exper:
#
#   Statistics consistent for homoskedasticity only
# Number of obs =                    428
# ------------------------------------------------------------------------------
#   exper |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   age |   .3948754   .0496446     7.95   0.000     .2972953    .4924555
# kidslt6 |  -.7469412   .9011267    -0.83   0.408    -2.518173    1.024291
# kidsge6 |  -1.430306   .2752275    -5.20   0.000    -1.971286   -.8893254
# _cons |  -1.500019   2.321874    -0.65   0.519    -6.063837    3.063798
# ------------------------------------------------------------------------------
#   F test of excluded instruments:
#   F(  3,   424) =    55.04
# Prob > F      =   0.0000
# Sanderson-Windmeijer multivariate F test of excluded instruments:
#   F(  2,   424) =    81.81
# Prob > F      =   0.0000
#
#
#
# Summary results for first-stage regressions
# -------------------------------------------
#
#   (Underid)            (Weak id)
# Variable     | F(  3,   424)  P-val | SW Chi-sq(  2) P-val | SW F(  2,   424)
# educ         |       4.47    0.0042 |       13.51   0.0012 |        6.69
# exper        |      55.04    0.0000 |      165.17   0.0000 |       81.81
#
# Stock-Yogo weak ID F test critical values for single endogenous regressor:
#   5% maximal IV relative bias    13.91
# 10% maximal IV relative bias     9.08
# 20% maximal IV relative bias     6.46
# 30% maximal IV relative bias     5.39
# 10% maximal IV size             22.30
# 15% maximal IV size             12.83
# 20% maximal IV size              9.54
# 25% maximal IV size              7.80
# Source: Stock-Yogo (2005).  Reproduced by permission.
# NB: Critical values are for Sanderson-Windmeijer F statistic.
#
# Underidentification test
# Ho: matrix of reduced form coefficients has rank=K1-1 (underidentified)
# Ha: matrix has rank=K1 (identified)
# Anderson canon. corr. LM statistic       Chi-sq(2)=13.10    P-val=0.0014
#
# Weak identification test
# Ho: equation is weakly identified
# Cragg-Donald Wald F statistic                                       4.46
#
# Stock-Yogo weak ID test critical values for K1=2 and L1=3:
#   10% maximal IV size             13.43
# 15% maximal IV size              8.18
# 20% maximal IV size              6.40
# 25% maximal IV size              5.45
# Source: Stock-Yogo (2005).  Reproduced by permission.
#
# Weak-instrument-robust inference
# Tests of joint significance of endogenous regressors B1 in main equation
# Ho: B1=0 and orthogonality conditions are valid
# Anderson-Rubin Wald test           F(3,424)=       2.08     P-val=0.1025
# Anderson-Rubin Wald test           Chi-sq(3)=      6.29     P-val=0.0983
# Stock-Wright LM S statistic        Chi-sq(3)=      6.20     P-val=0.1023
#
# Number of observations               N  =        428
# Number of regressors                 K  =          3
# Number of endogenous regressors      K1 =          2
# Number of instruments                L  =          4
# Number of excluded instruments       L1 =          3
#
# IV (2SLS) estimation
# --------------------
#
#   Estimates efficient for homoskedasticity only
# Statistics consistent for homoskedasticity only
#
# Number of obs =      428
# F(  2,   425) =     3.03
# Prob > F      =   0.0492
# Total (centered) SS     =  223.3274513                Centered R2   =   0.1482
# Total (uncentered) SS   =   829.594813                Uncentered R2 =   0.7707
# Residual SS             =  190.2315236                Root MSE      =    .6667
#
# ------------------------------------------------------------------------------
#   lwage |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   educ |   .1058361   .0806975     1.31   0.190    -.0523281    .2640003
# exper |   .0161527    .007568     2.13   0.033     .0013197    .0309858
# _cons |  -.3601821   1.029787    -0.35   0.727    -2.378528    1.658164
# ------------------------------------------------------------------------------
#   Underidentification test (Anderson canon. corr. LM statistic):          13.101
# Chi-sq(2) P-val =    0.0014
# ------------------------------------------------------------------------------
#   Weak identification test (Cragg-Donald Wald F statistic):                4.463
# Stock-Yogo weak ID test critical values: 10% maximal IV size             13.43
# 15% maximal IV size              8.18
# 20% maximal IV size              6.40
# 25% maximal IV size              5.45
# Source: Stock-Yogo (2005).  Reproduced by permission.
# ------------------------------------------------------------------------------
#   Sargan statistic (overidentification test of all instruments):           1.168
# Chi-sq(1) P-val =    0.2798
# ------------------------------------------------------------------------------
#   Instrumented:         educ exper
# Excluded instruments: age kidslt6 kidsge6
# ------------------------------------------------------------------------------
#
#   . mat list e(first)
#
# e(first)[21,2]
# educ      exper
# rmse  2.2580465  6.8583957
# sheapr2  .03061089  .28010273
# pr2  .03063228  .28029844
# F  4.4661716  55.044363
# df          3          3
# df_r        424        424
# pvalue  .00421033  4.562e-30
# SWF  6.6942505  81.812373
# SWFdf1          2          2
# SWFdf2        424        424
# SWFp  .00137303  8.961e-31
# SWchi2  13.514808  165.16838
# SWchi2p  .00116224  1.362e-36
# SWr2  .03061009  .27845108
# APF  6.6930319  82.489816
# APFdf1          2          2
# APFdf2        424        424
# APFp  .00137466  5.499e-31
# APchi2  13.512347  166.53604
# APchi2p  .00116367  6.873e-37
# APr2  .03060469  .28011093
