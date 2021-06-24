# ivreg package

library(ivreg)

## data
data("CigaretteDemand", package = "ivreg")

test_that("Expect error when model=FALSE", {
  merror <- ivreg(packs ~ rprice + rincome | salestax + rincome,
                  data = CigaretteDemand, model = FALSE)
  expect_error(fsw(merror))
})


test_that("Check run after ivreg model", {
  object <- ivreg(packs ~ rprice + rincome | salestax + cigtax + packsdiff,
                  data = CigaretteDemand)
  expect_equal(fsw(object)$fswres[1,1], 4.884, tolerance = 1e-2)
  expect_equal(fsw(object)$fswres[2,1], 3.450, tolerance = 1e-2)
})

# test_that("Check run with ivreg model object with transformations in formula", {
  # object <- ivreg(log(packs) ~ log(rprice) + log(rincome) | salestax + cigtax + packsdiff,
  #                 data = CigaretteDemand)
  # summary(object)
  # fsw(object)
#   test <- fsw(m)
# })

# Check error with a single endogenous variable
test_that("Require two or more exposures", {
  object <- ivreg(packs ~ rprice | salestax + rincome,
                  data = CigaretteDemand)
  expect_error(fsw(object))
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
dat <- data.frame(x1,x2,y,z1,z2)
est <- felm(y ~ 1 | 0 | (x1 | x2 ~ z1 + z2), data = dat)
# summary(est)
## Not run:
# summary(est$stage1, lhs='x1')
# summary(est$stage1, lhs='x2')
## End(Not run)
# the joint significance of the instruments in both the first stages are ok:
# t(sapply(est$stage1$lhs, function(lh) waldtest(est$stage1, ~ z1|z2, lhs = lh)))
# everything above looks fine, t-tests for instruments,
# as well as F-tests for excluded instruments in the 1st stages.
# The conditional F-test reveals that the instruments are jointly weak
# (it's close to being only one instrument, z1+z2, for both x1 and x2)
lfefstat <- condfstat(est, quantiles = c(0.05, 0.95))
lfefstat

mod2 <- ivreg(y ~ x1 + x2 | z1 + z2, data = dat)
fstat = fsw(mod2)

test_that("Check equivalence with lfe package", {
  expect_equal(lfefstat[1], fstat$fswres[1,1], tolerance = 1e-2)
  expect_equal(lfefstat[2], fstat$fswres[2,1], tolerance = 1e-2)
})


# Stata ivreg2 example

library(haven)
library(ivreg)
library(lfe)

url <- "http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta"
dat <- haven::read_dta(url)

mod <- ivreg(lwage ~ educ + exper | age + kidslt6 + kidsge6, data = dat)
# summary(mod)
condf <- fsw(mod)
# condf
test_that("Compare with Stata ivreg2 output", {
  expect_equal(condf$fswres[1,1], 6.69, tolerance = 1e-2)
  expect_equal(condf$fswres[2,1], 81.81, tolerance = 1e-2)
})


# Using lfe package
# modst2 <- felm(lwage ~ 1 | 0 | (educ | exper ~ age + kidslt6 + kidsge6), data = dat)
# summary(modst2)
# t(sapply(modst2$stage1$lhs, function(lh) waldtest(modst2$stage1, ~ age | kidslt6 | kidsge6, lhs = lh)))
# condfstat(modst2, quantiles = c(0.025, 0.975))

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

# Code from Appendix A.3 of Sanderson and Windmeijer
# clear
# set obs 4000
# set seed 12345 // added TP
# gen w1 = rnormal()
# gen w2 = rnormal()
# gen z1 = rnormal()
# gen z2 = rnormal()
# gen z3 = rnormal()
# gen z4 = rnormal()
# gen z5 = rnormal()
# mat covmat = (1, .5, .5 \ .5, 1, .5 \ .5, .5, 1)
# drawnorm x1 x2 x3, cov(covmat)
# ivregress 2sls x1 (x2 x3 = z1 z2 z3 z4 z5) w1 w2
# predict res123, r
# reg res123 z1 z2 z3 z4 z5 w1 w2
# test z1 z2 z3 z4 z5
# scalar Fsw = r(F)*r(df)/(r(df)-2)
# di Fsw

# . clear
#
# . set obs 4000
# number of observations (_N) was 0, now 4,000
#
# . set seed 12345 // added TP
#
# . gen w1 = rnormal()
#
# . gen w2 = rnormal()
#
# . gen z1 = rnormal()
#
# . gen z2 = rnormal()
#
# . gen z3 = rnormal()
#
# . gen z4 = rnormal()
#
# . gen z5 = rnormal()
#
# . mat covmat = (1, .5, .5 \ .5, 1, .5 \ .5, .5, 1)
#
# . drawnorm x1 x2 x3, cov(covmat)
#
# .
# . ivregress 2sls x1 (x2 x3 = z1 z2 z3 z4 z5) w1 w2
#
# Instrumental variables (2SLS) regression          Number of obs   =      4,000
# Wald chi2(4)    =       4.90
# Prob > chi2     =     0.2974
# R-squared       =     0.1259
# Root MSE        =     .94388
#
# ------------------------------------------------------------------------------
#   x1 |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   x2 |  -.1861774   .4125931    -0.45   0.652    -.9948449    .6224902
# x3 |    .667248   .5329029     1.25   0.211    -.3772225    1.711718
# w1 |     .02143   .0174925     1.23   0.221    -.0128547    .0557147
# w2 |   .0146589    .016231     0.90   0.366    -.0171534    .0464711
# _cons |    .000549    .015748     0.03   0.972    -.0303166    .0314145
# ------------------------------------------------------------------------------
#   Instrumented:  x2 x3
# Instruments:   w1 w2 z1 z2 z3 z4 z5
#
# . predict res123, r
#
# . reg res123 z1 z2 z3 z4 z5 w1 w2
#
# Source |       SS           df       MS      Number of obs   =     4,000
# -------------+----------------------------------   F(7, 3992)      =      0.22
# Model |  1.35045648         7  .192922355   Prob > F        =    0.9818
# Residual |  3562.27268     3,992  .892352877   R-squared       =    0.0004
# -------------+----------------------------------   Adj R-squared   =   -0.0014
# Total |  3563.62314     3,999  .891128567   Root MSE        =    .94464
#
# ------------------------------------------------------------------------------
#   res123 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   z1 |     .00306   .0148837     0.21   0.837    -.0261203    .0322403
# z2 |  -.0031092   .0150206    -0.21   0.836     -.032558    .0263396
# z3 |   .0123012   .0146714     0.84   0.402    -.0164629    .0410653
# z4 |   .0087919   .0148553     0.59   0.554    -.0203327    .0379166
# z5 |  -.0088948   .0150712    -0.59   0.555    -.0384428    .0206531
# w1 |   .0002732   .0149939     0.02   0.985    -.0291231    .0296695
# w2 |   .0004532   .0149238     0.03   0.976    -.0288058    .0297123
# _cons |  -.0002686   .0149467    -0.02   0.986    -.0295724    .0290353
# ------------------------------------------------------------------------------
#
#   . test z1 z2 z3 z4 z5
#
# ( 1)  z1 = 0
# ( 2)  z2 = 0
# ( 3)  z3 = 0
# ( 4)  z4 = 0
# ( 5)  z5 = 0
#
# F(  5,  3992) =    0.30
# Prob > F =    0.9115
#
# . scalar Fsw = r(F)*r(df)/(r(df)-2)
#
# . di Fsw
# .50445533
#
# .
# . ivreg2 x1 (x2 x3 = z1 z2 z3 z4 z5) w1 w2, first
#
# First-stage regressions
# -----------------------
#
#
#   First-stage regression of x2:
#
#   Statistics consistent for homoskedasticity only
# Number of obs =                   4000
# ------------------------------------------------------------------------------
#   x2 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   z1 |  -.0019387   .0157783    -0.12   0.902     -.032873    .0289955
# z2 |   .0325831   .0159235     2.05   0.041     .0013641     .063802
# z3 |   .0023125   .0155533     0.15   0.882    -.0281806    .0328056
# z4 |   .0164893   .0157482     1.05   0.295     -.014386    .0473645
# z5 |   .0065758   .0159771     0.41   0.681    -.0247482    .0378998
# w1 |   .0178871   .0158951     1.13   0.261    -.0132762    .0490504
# w2 |  -.0006284   .0158209    -0.04   0.968    -.0316461    .0303894
# _cons |  -.0133442   .0158451    -0.84   0.400    -.0444094     .017721
# ------------------------------------------------------------------------------
#   F test of excluded instruments:
#   F(  5,  3992) =     1.08
# Prob > F      =   0.3715
# Sanderson-Windmeijer multivariate F test of excluded instruments:
#   F(  4,  3992) =     1.59
# Prob > F      =   0.1750
#
#
# First-stage regression of x3:
#
#   Statistics consistent for homoskedasticity only
# Number of obs =                   4000
# ------------------------------------------------------------------------------
#   x3 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   z1 |  -.0271183   .0157085    -1.73   0.084    -.0579157    .0036792
# z2 |   .0049029   .0158531     0.31   0.757    -.0261779    .0359838
# z3 |   .0021507   .0154845     0.14   0.890    -.0282075    .0325088
# z4 |   .0015835   .0156785     0.10   0.920    -.0291552    .0323222
# z5 |  -.0064682   .0159064    -0.41   0.684    -.0376536    .0247172
# w1 |   .0127237   .0158248     0.80   0.421    -.0183018    .0437491
# w2 |  -.0118418   .0157509    -0.75   0.452    -.0427223    .0190387
# _cons |  -.0022251    .015775    -0.14   0.888    -.0331529    .0287027
# ------------------------------------------------------------------------------
#   F test of excluded instruments:
#   F(  5,  3992) =     0.65
# Prob > F      =   0.6610
# Sanderson-Windmeijer multivariate F test of excluded instruments:
#   F(  4,  3992) =     0.90
# Prob > F      =   0.4654
#
#
#
# Summary results for first-stage regressions
# -------------------------------------------
#
#   (Underid)            (Weak id)
# Variable     | F(  5,  3992)  P-val | SW Chi-sq(  4) P-val | SW F(  4,  3992)
# x2           |       1.08    0.3715 |        6.36   0.1739 |        1.59
# x3           |       0.65    0.6610 |        3.59   0.4642 |        0.90
#
# Stock-Yogo weak ID F test critical values for single endogenous regressor:
#   5% maximal IV relative bias    18.37
# 10% maximal IV relative bias    10.83
# 20% maximal IV relative bias     6.77
# 30% maximal IV relative bias     5.25
# 10% maximal IV size             26.87
# 15% maximal IV size             15.09
# 20% maximal IV size             10.98
# 25% maximal IV size              8.84
# Source: Stock-Yogo (2005).  Reproduced by permission.
# NB: Critical values are for Sanderson-Windmeijer F statistic.
#
# Underidentification test
# Ho: matrix of reduced form coefficients has rank=K1-1 (underidentified)
# Ha: matrix has rank=K1 (identified)
# Anderson canon. corr. LM statistic       Chi-sq(4)=2.99     P-val=0.5594
#
# Weak identification test
# Ho: equation is weakly identified
# Cragg-Donald Wald F statistic                                       0.60
#
# Stock-Yogo weak ID test critical values for K1=2 and L1=5:
#   5% maximal IV relative bias    13.97
# 10% maximal IV relative bias     8.78
# 20% maximal IV relative bias     5.91
# 30% maximal IV relative bias     4.79
# 10% maximal IV size             19.45
# 15% maximal IV size             11.22
# 20% maximal IV size              8.38
# 25% maximal IV size              6.89
# Source: Stock-Yogo (2005).  Reproduced by permission.
#
# Weak-instrument-robust inference
# Tests of joint significance of endogenous regressors B1 in main equation
# Ho: B1=0 and orthogonality conditions are valid
# Anderson-Rubin Wald test           F(5,3992)=      0.55     P-val=0.7395
# Anderson-Rubin Wald test           Chi-sq(5)=      2.75     P-val=0.7386
# Stock-Wright LM S statistic        Chi-sq(5)=      2.75     P-val=0.7389
#
# Number of observations               N  =       4000
# Number of regressors                 K  =          5
# Number of endogenous regressors      K1 =          2
# Number of instruments                L  =          8
# Number of excluded instruments       L1 =          5
#
# IV (2SLS) estimation
# --------------------
#
#   Estimates efficient for homoskedasticity only
# Statistics consistent for homoskedasticity only
#
# Number of obs =     4000
# F(  4,  3995) =     1.22
# Prob > F      =   0.2982
# Total (centered) SS     =  4076.803985                Centered R2   =   0.1259
# Total (uncentered) SS   =  4076.816472                Uncentered R2 =   0.1259
# Residual SS             =  3563.623141                Root MSE      =    .9439
#
# ------------------------------------------------------------------------------
#   x1 |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   x2 |  -.1861774   .4125931    -0.45   0.652    -.9948449    .6224902
# x3 |    .667248   .5329029     1.25   0.211    -.3772225    1.711718
# w1 |     .02143   .0174925     1.23   0.221    -.0128547    .0557147
# w2 |   .0146589    .016231     0.90   0.366    -.0171534    .0464711
# _cons |    .000549    .015748     0.03   0.972    -.0303166    .0314145
# ------------------------------------------------------------------------------
#   Underidentification test (Anderson canon. corr. LM statistic):           2.990
# Chi-sq(4) P-val =    0.5594
# ------------------------------------------------------------------------------
#   Weak identification test (Cragg-Donald Wald F statistic):                0.597
# Stock-Yogo weak ID test critical values:  5% maximal IV relative bias    13.97
# 10% maximal IV relative bias     8.78
# 20% maximal IV relative bias     5.91
# 30% maximal IV relative bias     4.79
# 10% maximal IV size             19.45
# 15% maximal IV size             11.22
# 20% maximal IV size              8.38
# 25% maximal IV size              6.89
# Source: Stock-Yogo (2005).  Reproduced by permission.
# ------------------------------------------------------------------------------
#   Sargan statistic (overidentification test of all instruments):           1.516
# Chi-sq(3) P-val =    0.6786
# ------------------------------------------------------------------------------
#   Instrumented:         x2 x3
# Included instruments: w1 w2
# Excluded instruments: z1 z2 z3 z4 z5
# ------------------------------------------------------------------------------
#
#   . mat list e(first)
#
# e(first)[21,2]
# x2         x3
# rmse  1.0014257  .99699546
# sheapr2   .0017641  .00106746
# pr2  .00134586  .00081439
# F  1.0759845  .65073595
# df          5          5
# df_r       3992       3992
# pvalue  .37147303  .66095627
# SWF  1.5865039  .89590568
# SWFdf1          4          4
# SWFdf2       3992       3992
# SWFp  .17498432  .46538383
# SWchi2   6.358733  3.5908043
# SWchi2p  .17391183  .46420632
# SWr2  .00158716   .0008969
# APF  1.3046394  .78902226
# APFdf1          4          4
# APFdf2       3992       3992
# APFp  .26580003  .53212677
# APchi2  5.2290154  3.1624138
# APchi2p  .26459576  .53102306
# APr2  .00130555  .00078998


# . use http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta, clear
#
# .
# end of do-file
#
# . do "C:\Users\eptmp\AppData\Local\Temp\STD620_000000.tmp"
#
# . ivreg2 lwage (educ exper = age kidslt6), first
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
#   age |  -.0028069   .0151294    -0.19   0.853    -.0325446    .0269308
# kidslt6 |   .7354883   .2980564     2.47   0.014     .1496402    1.321336
# _cons |   12.67358   .6595434    19.22   0.000     11.37721    13.96996
# ------------------------------------------------------------------------------
#   F test of excluded instruments:
#   F(  2,   425) =     3.63
# Prob > F      =   0.0273
# Sanderson-Windmeijer multivariate F test of excluded instruments:
#   F(  1,   425) =     5.74
# Prob > F      =   0.0170
#
#
# First-stage regression of exper:
#
#   Statistics consistent for homoskedasticity only
# Number of obs =                    428
# ------------------------------------------------------------------------------
#   exper |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   age |   .4958752   .0470592    10.54   0.000     .4033775    .5883729
# kidslt6 |  -.5090499   .9270899    -0.55   0.583    -2.331302    1.313202
# _cons |   -7.70411   2.051478    -3.76   0.000    -11.73642   -3.671804
# ------------------------------------------------------------------------------
#   F test of excluded instruments:
#   F(  2,   425) =    65.08
# Prob > F      =   0.0000
# Sanderson-Windmeijer multivariate F test of excluded instruments:
#   F(  1,   425) =    22.57
# Prob > F      =   0.0000
#
#
#
# Summary results for first-stage regressions
# -------------------------------------------
#
#   (Underid)            (Weak id)
# Variable     | F(  2,   425)  P-val | SW Chi-sq(  1) P-val | SW F(  1,   425)
# educ         |       3.63    0.0273 |        5.78   0.0162 |        5.74
# exper        |      65.08    0.0000 |       22.73   0.0000 |       22.57
#
# Stock-Yogo weak ID F test critical values for single endogenous regressor:
#   10% maximal IV size             19.93
# 15% maximal IV size             11.59
# 20% maximal IV size              8.75
# 25% maximal IV size              7.25
# Source: Stock-Yogo (2005).  Reproduced by permission.
# NB: Critical values are for Sanderson-Windmeijer F statistic.
#
# Underidentification test
# Ho: matrix of reduced form coefficients has rank=K1-1 (underidentified)
# Ha: matrix has rank=K1 (identified)
# Anderson canon. corr. LM statistic       Chi-sq(1)=5.70     P-val=0.0170
#
# Weak identification test
# Ho: equation is weakly identified
# Cragg-Donald Wald F statistic                                       2.87
#
# Stock-Yogo weak ID test critical values for K1=2 and L1=2:
#   10% maximal IV size              7.03
# 15% maximal IV size              4.58
# 20% maximal IV size              3.95
# 25% maximal IV size              3.63
# Source: Stock-Yogo (2005).  Reproduced by permission.
#
# Weak-instrument-robust inference
# Tests of joint significance of endogenous regressors B1 in main equation
# Ho: B1=0 and orthogonality conditions are valid
# Anderson-Rubin Wald test           F(2,425)=       0.64     P-val=0.5263
# Anderson-Rubin Wald test           Chi-sq(2)=      1.29     P-val=0.5234
# Stock-Wright LM S statistic        Chi-sq(2)=      1.29     P-val=0.5244
#
# Number of observations               N  =        428
# Number of regressors                 K  =          3
# Number of endogenous regressors      K1 =          2
# Number of instruments                L  =          3
# Number of excluded instruments       L1 =          2
#
# IV (2SLS) estimation
# --------------------
#
#   Estimates efficient for homoskedasticity only
# Statistics consistent for homoskedasticity only
#
# Number of obs =      428
# F(  2,   425) =     0.67
# Prob > F      =   0.5129
# Total (centered) SS     =  223.3274513                Centered R2   =   0.0415
# Total (uncentered) SS   =   829.594813                Uncentered R2 =   0.7420
# Residual SS             =  214.0604775                Root MSE      =    .7072
#
# ------------------------------------------------------------------------------
#   lwage |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   educ |   .0074314   .1290546     0.06   0.954    -.2455111    .2603738
# exper |    .010421   .0098027     1.06   0.288     -.008792     .029634
# _cons |   .9602385   1.694897     0.57   0.571    -2.361699    4.282176
# ------------------------------------------------------------------------------
#   Underidentification test (Anderson canon. corr. LM statistic):           5.699
# Chi-sq(1) P-val =    0.0170
# ------------------------------------------------------------------------------
#   Weak identification test (Cragg-Donald Wald F statistic):                2.868
# Stock-Yogo weak ID test critical values: 10% maximal IV size              7.03
# 15% maximal IV size              4.58
# 20% maximal IV size              3.95
# 25% maximal IV size              3.63
# Source: Stock-Yogo (2005).  Reproduced by permission.
# ------------------------------------------------------------------------------
#   Sargan statistic (overidentification test of all instruments):           0.000
# (equation exactly identified)
# ------------------------------------------------------------------------------
#   Instrumented:         educ exper
# Excluded instruments: age kidslt6
# ------------------------------------------------------------------------------
#
#   . mat list e(first)
#
# e(first)[21,2]
# educ      exper
# rmse  2.2714133  7.0651217
# sheapr2  .01346799  .18786232
# pr2  .01680838  .23445676
# F  3.6328429  65.080662
# df          2          2
# df_r        425        425
# pvalue  .02726533  2.206e-25
# SWF  5.7388476  22.573095
# SWFdf1          1          1
# SWFdf2        425        425
# SWFp  .01702542  2.772e-06
# SWchi2  5.7793571  22.732434
# SWchi2p  .01621547  1.862e-06
# SWr2  .01332326  .05043443
# APF  5.8204037  104.26978
# APFdf1          1          1
# APFdf2        425        425
# APFp  .01626387  4.869e-22
# APchi2  5.8614889   105.0058
# APchi2p   .0154757  1.218e-24
# APr2  .01351005  .19700687

# // 2 endogenous variables
#
# . use http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta, clear
#
# .
# . ivreg2 lwage (educ exper = age kidslt6 kidsge6) if !missing(lwage,educ,exper,age,kidslt6,kidsge6), first
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
#   .
# . ivregress 2sls educ (exper = age kidslt6 kidsge6) if !missing(lwage,educ,exper,age,kidslt6,kidsge6)
#
# Instrumental variables (2SLS) regression          Number of obs   =        428
# Wald chi2(1)    =       0.01
# Prob > chi2     =     0.9121
# R-squared       =     0.0002
# Root MSE        =     2.2825
#
# ------------------------------------------------------------------------------
#   educ |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   exper |  -.0028589    .025898    -0.11   0.912     -.053618    .0479002
# _cons |   12.69615   .3552098    35.74   0.000     11.99995    13.39235
# ------------------------------------------------------------------------------
#   Instrumented:  exper
# Instruments:   age kidslt6 kidsge6
#
# . cap noi drop res12
# variable res12 not found
#
# . predict res12 if !missing(lwage,educ,exper,age,kidslt6,kidsge6), r
# (325 missing values generated)
#
# . reg res12 age kidslt6 kidsge6 if !missing(lwage,educ,exper,age,kidslt6,kidsge6)
#
# Source |       SS           df       MS      Number of obs   =       428
# -------------+----------------------------------   F(3, 424)       =      4.46
# Model |  68.2525159         3  22.7508386   Prob > F        =    0.0042
# Residual |  2161.48673       424  5.09784606   R-squared       =    0.0306
# -------------+----------------------------------   Adj R-squared   =    0.0238
# Total |  2229.73925       427  5.22187177   Root MSE        =    2.2578
#
# ------------------------------------------------------------------------------
#   res12 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   age |  -.0174123   .0163434    -1.07   0.287    -.0495365    .0147119
# kidslt6 |   .6962929   .2966584     2.35   0.019     .1131886    1.279397
# kidsge6 |  -.2269101   .0906072    -2.50   0.013    -.4050053   -.0488149
# _cons |   .9396522   .7643804     1.23   0.220    -.5627945    2.442099
# ------------------------------------------------------------------------------
#
#   . test age kidslt6 kidsge6
#
# ( 1)  age = 0
# ( 2)  kidslt6 = 0
# ( 3)  kidsge6 = 0
#
# F(  3,   424) =    4.46
# Prob > F =    0.0042
#
# . scalar Fsw12 = r(F)*(r(df))/(r(df) - 1)
#
# . di Fsw12
# 6.6942504
#
# .
# . ivregress 2sls exper (educ = age kidslt6 kidsge6) if !missing(lwage,educ,exper,age,kidslt6,kidsge6)
#
# Instrumental variables (2SLS) regression          Number of obs   =        428
# Wald chi2(1)    =       0.11
# Prob > chi2     =     0.7392
# R-squared       =          .
# Root MSE        =     8.0694
#
# ------------------------------------------------------------------------------
#   exper |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   educ |  -.3250531   .9762929    -0.33   0.739    -2.238552    1.588446
# _cons |   17.15219   12.36493     1.39   0.165    -7.082621      41.387
# ------------------------------------------------------------------------------
#   Instrumented:  educ
# Instruments:   age kidslt6 kidsge6
#
# . cap noi drop res21
# variable res21 not found
#
# . predict res21 if !missing(lwage,educ,exper,age,kidslt6,kidsge6), r
# (325 missing values generated)
#
# . reg res21 age kidslt6 kidsge6 if !missing(lwage,educ,exper,age,kidslt6,kidsge6)
#
# Source |       SS           df       MS      Number of obs   =       428
# -------------+----------------------------------   F(3, 424)       =     54.54
# Model |  7760.24452         3  2586.74817   Prob > F        =    0.0000
# Residual |  20109.0836       424  47.4270839   R-squared       =    0.2785
# -------------+----------------------------------   Adj R-squared   =    0.2733
# Total |  27869.3281       427  65.2677473   Root MSE        =    6.8867
#
# ------------------------------------------------------------------------------
#   res21 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   age |   .3888485   .0498497     7.80   0.000     .2908652    .4868318
# kidslt6 |  -.5199149   .9048498    -0.57   0.566    -2.298465    1.258635
# kidsge6 |  -1.502734   .2763647    -5.44   0.000     -2.04595   -.9595189
# _cons |  -14.21846   2.331468    -6.10   0.000    -18.80113   -9.635782
# ------------------------------------------------------------------------------
#
#   . test age kidslt6 kidsge6
#
# ( 1)  age = 0
# ( 2)  kidslt6 = 0
# ( 3)  kidsge6 = 0
#
# F(  3,   424) =   54.54
# Prob > F =    0.0000
#
# . scalar Fsw21 = r(F)*(r(df))/(r(df) - 1)
#
# . di Fsw21
# 81.812373
