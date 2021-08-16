# tests for MSMM

# Test equivalence to Stata output
test_that("Stata output check", {
  skip_on_cran()
  library(haven)
  dat <- read_dta("https://www.stata-press.com/data/r17/trip.dta")
  fit1 <- msmm(trips ~ cbd + ptn + worker + weekend + tcost |
                 cbd + ptn + worker + weekend + pt,
               data = dat, estmethod = "gmmalt")
  expect_equal(log(fit1$crrci["tcost",1]), log(1.036),  tolerance = 0.05)
  # fit2 <- msmm(trips ~ cbd + ptn + worker + weekend + tcost |
  #                cbd + ptn + worker + weekend + pt,
  #              data = dat)
  # fit3 <- msmm(trips ~ cbd + ptn + worker + weekend + tcost |
  #                cbd + ptn + worker + weekend + pt,
  #              data = dat, estmethod = "gmmalt",
  #              t0 = c(.265, -.008, -.011, .662, .301, .035))
  # fit4 <- msmm(trips ~ cbd + ptn + worker + weekend + tcost |
  #                cbd + ptn + worker + weekend + pt,
  #              data = dat,
  #              t0 = c(exp(.265), -.008, -.011, .662, .301, .035))
})
# . * Setup
# . webuse trip, clear
# (Household trips)
#
# .
# . * Generalized method of moments: multiplicative errors
# . ivpoisson gmm trips cbd ptn worker weekend (tcost=pt), multiplicative nolog
#
# Final GMM criterion Q(b) =  1.27e-31
#
# note: model is exactly identified
#
# Exponential mean model with endogenous regressors
#
# Number of parameters =   6                         Number of obs  =      5,000
# Number of moments    =   6
# Initial weight matrix: Unadjusted
# GMM weight matrix:     Robust
#
# ------------------------------------------------------------------------------
#   |               Robust
# trips |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   tcost |   .0352185   .0098182     3.59   0.000     .0159752    .0544617
# cbd |   -.008398   .0020172    -4.16   0.000    -.0123517   -.0044444
# ptn |  -.0113146   .0021819    -5.19   0.000     -.015591   -.0070383
# worker |   .6623018   .0519909    12.74   0.000     .5604015     .764202
# weekend |   .3009323   .0362682     8.30   0.000     .2298479    .3720167
# _cons |   .2654423   .1550127     1.71   0.087    -.0383769    .5692616
# ------------------------------------------------------------------------------
#   Instrumented:  tcost
# Instruments:   cbd ptn worker weekend pt
#
# .
# . * Display incidence-rate ratios
# . ivpoisson, irr
#
# Exponential mean model with endogenous regressors
#
# Number of parameters =   6                         Number of obs  =      5,000
# Number of moments    =   6
# Initial weight matrix: Unadjusted
# GMM weight matrix:     Robust
#
# ------------------------------------------------------------------------------
#   |               Robust
# trips |        IRR   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   tcost |   1.035846   .0101701     3.59   0.000     1.016103    1.055972
# cbd |   .9916371   .0020003    -4.16   0.000     .9877243    .9955655
# ptn |   .9887491   .0021573    -5.19   0.000     .9845299    .9929864
# worker |   1.939251   .1008234    12.74   0.000     1.751376     2.14728
# weekend |   1.351118   .0490026     8.30   0.000     1.258409    1.450657
# _cons |   1.304008   .2021377     1.71   0.087     .9623501    1.766962
# ------------------------------------------------------------------------------
#   Note: _cons estimates baseline incidence rate.
# Instrumented:  tcost
# Instruments:   cbd ptn worker weekend pt
#
# .
# . * Control-function method
# . ivpoisson cfunction trips cbd ptn worker weekend (tcost=pt), nolog
#
# Final GMM criterion Q(b) =  9.78e-27
#
# note: model is exactly identified
#
# Exponential mean model with endogenous regressors
#
# Number of parameters =  13                         Number of obs  =      5,000
# Number of moments    =  13
# Initial weight matrix: Unadjusted
# GMM weight matrix:     Robust
#
# ------------------------------------------------------------------------------
#   |               Robust
# trips |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   trips        |
#   cbd |  -.0082567   .0020005    -4.13   0.000    -.0121777   -.0043357
# ptn |  -.0113719   .0021625    -5.26   0.000    -.0156102   -.0071335
# worker |   .6903044   .0521642    13.23   0.000     .5880645    .7925444
# weekend |   .2978149   .0356474     8.35   0.000     .2279472    .3676825
# tcost |   .0320718   .0092738     3.46   0.001     .0138955    .0502481
# _cons |   .2145986   .1359327     1.58   0.114    -.0518246    .4810218
# -------------+----------------------------------------------------------------
#   tcost        |
#   cbd |   .0165466   .0043693     3.79   0.000     .0079829    .0251102
# ptn |   -.040652   .0045946    -8.85   0.000    -.0496573   -.0316467
# worker |   1.550985   .0996496    15.56   0.000     1.355675    1.746294
# weekend |   .0423009   .0779101     0.54   0.587    -.1104002    .1950019
# pt |   .7739176   .0150072    51.57   0.000     .7445041    .8033312
# _cons |   12.13934   .1123471   108.05   0.000     11.91915    12.35954
# -------------+----------------------------------------------------------------
#   /c_tcost |   .1599984   .0111752    14.32   0.000     .1380954    .1819014
# ------------------------------------------------------------------------------
#   Instrumented:  tcost
# Instruments:   cbd ptn worker weekend pt
#
# . ivpoisson, irr
#
# Exponential mean model with endogenous regressors
#
# Number of parameters =  13                         Number of obs  =      5,000
# Number of moments    =  13
# Initial weight matrix: Unadjusted
# GMM weight matrix:     Robust
#
# ------------------------------------------------------------------------------
#   |               Robust
# trips |        IRR   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   trips        |
#   cbd |   .9917773   .0019841    -4.13   0.000     .9878961    .9956737
# ptn |   .9886926    .002138    -5.26   0.000      .984511    .9928919
# worker |   1.994323   .1040322    13.23   0.000       1.8005     2.20901
# weekend |   1.346912   .0480139     8.35   0.000     1.256019    1.444383
# tcost |   1.032592    .009576     3.46   0.001     1.013992    1.051532
# _cons |   1.239364   .1684701     1.58   0.114     .9494954    1.617727
# -------------+----------------------------------------------------------------
#   tcost        |
#   cbd |   .0165466   .0043693     3.79   0.000     .0079829    .0251102
# ptn |   -.040652   .0045946    -8.85   0.000    -.0496573   -.0316467
# worker |   1.550985   .0996496    15.56   0.000     1.355675    1.746294
# weekend |   .0423009   .0779101     0.54   0.587    -.1104002    .1950019
# pt |   .7739176   .0150072    51.57   0.000     .7445041    .8033312
# _cons |   12.13934   .1123471   108.05   0.000     11.91915    12.35954
# -------------+----------------------------------------------------------------
#   /c_tcost |   .1599984   .0111752    14.32   0.000     .1380954    .1819014
# ------------------------------------------------------------------------------
#   Note: Estimates are transformed only in the first equation.
# Note: _cons estimates baseline incidence rate.
# Instrumented:  tcost
# Instruments:   cbd ptn worker weekend pt
#
# .
# . * ivpois for comparison
# . ivpois trips cbd ptn worker weekend, endog(tcost) exog(pt)
# ------------------------------------------------------------------------------
#   trips |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   trips        |
#   cbd |   -.008398   .0020172    -4.16   0.000    -.0123517   -.0044444
# ptn |  -.0113146   .0021819    -5.19   0.000     -.015591   -.0070383
# worker |   .6623018   .0519909    12.74   0.000     .5604015     .764202
# weekend |   .3009323   .0362682     8.30   0.000     .2298479    .3720167
# tcost |   .0352185   .0098182     3.59   0.000     .0159752    .0544617
# _cons |   .2654423   .1550127     1.71   0.087    -.0383769    .5692616
# ------------------------------------------------------------------------------
#
# . * coding msmm using the gmm command
# . local y trips
#
# . local x tcost
#
# . local z pt
#
# . local covars cbd ptn worker weekend
#
# . gmm (`y'*exp(-1*`x'*{psi} - {xb:`covars'}) - {ey0}), ///
#   >         instruments(`z' `covars') tracelevel("none")
#
# Final GMM criterion Q(b) =  9.64e-34
#
# note: model is exactly identified
#
# GMM estimation
#
# Number of parameters =   6
# Number of moments    =   6
# Initial weight matrix: Unadjusted                 Number of obs   =      5,000
# GMM weight matrix:     Robust
#
# ------------------------------------------------------------------------------
#              |               Robust
#              |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
# psi          |
#        _cons |   .0352185   .0098182     3.59   0.000     .0159752    .0544617
# -------------+----------------------------------------------------------------
# xb           |
#          cbd |   -.008398   .0020172    -4.16   0.000    -.0123517   -.0044444
#          ptn |  -.0113146   .0021819    -5.19   0.000     -.015591   -.0070383
#       worker |   .6623018   .0519909    12.74   0.000     .5604015     .764202
#      weekend |   .3009323   .0362682     8.30   0.000     .2298479    .3720167
# -------------+----------------------------------------------------------------
#         /ey0 |   1.304008   .2021377     6.45   0.000      .907825     1.70019
# ------------------------------------------------------------------------------
# Instruments for equation 1: pt cbd ptn worker weekend _cons
#
# . lincom [psi]_cons, eform // causal risk ratio
#
#  ( 1)  [psi]_cons = 0
#
# ------------------------------------------------------------------------------
#              |     exp(b)   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#          (1) |   1.035846   .0101701     3.59   0.000     1.016103    1.055972
# ------------------------------------------------------------------------------
#
# . // estat overid
# .
# . * coding msmm using the gmm command with first derivatives
# . local y trips
#
# . local xlist tcost cbd ptn worker weekend
#
# . local zlist pt cbd ptn worker weekend
#
# . local covars cbd ptn worker weekend
#
# . gmm (`y'*exp(-1*{xb:`xlist'}) - {ey0}), ///
# >         instruments(`zlist') tracelevel("none") ///
#                           >         deriv(/xb = -1*`y'*exp(-1*{xb:})) /// remembering that deriv automatically multiplies by x in xb
# >         deriv(/ey0 = -1)
#
# Final GMM criterion Q(b) =  5.31e-33
#
# note: model is exactly identified
#
# GMM estimation
#
# Number of parameters =   6
# Number of moments    =   6
# Initial weight matrix: Unadjusted                 Number of obs   =      5,000
# GMM weight matrix:     Robust
#
# ------------------------------------------------------------------------------
#              |               Robust
#              |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#        tcost |   .0352185   .0098182     3.59   0.000     .0159752    .0544617
#          cbd |   -.008398   .0020172    -4.16   0.000    -.0123517   -.0044444
#          ptn |  -.0113146   .0021819    -5.19   0.000     -.015591   -.0070383
#       worker |   .6623018   .0519909    12.74   0.000     .5604015     .764202
#      weekend |   .3009323   .0362682     8.30   0.000     .2298479    .3720167
# -------------+----------------------------------------------------------------
#         /ey0 |   1.304008   .2021377     6.45   0.000      .907825     1.70019
# ------------------------------------------------------------------------------
# Instruments for equation 1: pt cbd ptn worker weekend _cons
#
# .
# . * coding msmm using the alternative moment condition using the gmm command
# . local y trips
#
# . local x tcost
#
# . local z pt
#
# . local covars cbd ptn worker weekend
#
# . gmm (`y'*exp(-1*`x'*{psi} - {xb:`covars'} - {logey0}) - 1), ///
# >         instruments(`z' `covars') tracelevel("none")
#
# Final GMM criterion Q(b) =  1.09e-28
#
# note: model is exactly identified
#
# GMM estimation
#
# Number of parameters =   6
# Number of moments    =   6
# Initial weight matrix: Unadjusted                 Number of obs   =      5,000
# GMM weight matrix:     Robust
#
# ------------------------------------------------------------------------------
#              |               Robust
#              |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
# psi          |
#        _cons |   .0352185   .0098182     3.59   0.000     .0159752    .0544617
# -------------+----------------------------------------------------------------
# xb           |
#          cbd |   -.008398   .0020172    -4.16   0.000    -.0123517   -.0044444
#          ptn |  -.0113146   .0021819    -5.19   0.000     -.015591   -.0070383
#       worker |   .6623018   .0519909    12.74   0.000     .5604015     .764202
#      weekend |   .3009323   .0362682     8.30   0.000     .2298479    .3720167
# -------------+----------------------------------------------------------------
#      /logey0 |   .2654423   .1550127     1.71   0.087     -.038377    .5692616
# ------------------------------------------------------------------------------
# Instruments for equation 1: pt cbd ptn worker weekend _cons
#
# . lincom [psi]_cons, eform // causal risk ratio
#
#  ( 1)  [psi]_cons = 0
#
# ------------------------------------------------------------------------------
#              |     exp(b)   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#          (1) |   1.035846   .0101701     3.59   0.000     1.016103    1.055972
# ------------------------------------------------------------------------------
#
# . lincom [logey0]_cons, eform // causal baseline risk
#
#  ( 1)  [logey0]_cons = 0
#
# ------------------------------------------------------------------------------
#              |     exp(b)   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#          (1) |   1.304008   .2021377     1.71   0.087     .9623501    1.766962
# ------------------------------------------------------------------------------
#
# . // estat overid
# .
# . // with first derivatives
# . local y trips
#
# . local x tcost
#
# . local z pt
#
# . local covars cbd ptn worker weekend
#
# . gmm (`y'*exp(-1*{xb:`x' `covars'} - {logey0}) - 1), ///
# >         instruments(`z' `covars') tracelevel("none") ///
# >         deriv(/xb = -1*`y'*exp(-1*{xb:} - {logey0})) ///
#                  >         deriv(/logey0 = -1*`y'*exp(-1*{xb:} - {logey0}))
#
# Final GMM criterion Q(b) =  1.09e-28
#
# note: model is exactly identified
#
# GMM estimation
#
# Number of parameters =   6
# Number of moments    =   6
# Initial weight matrix: Unadjusted                 Number of obs   =      5,000
# GMM weight matrix:     Robust
#
# ------------------------------------------------------------------------------
#              |               Robust
#              |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#        tcost |   .0352185   .0098182     3.59   0.000     .0159752    .0544617
#          cbd |   -.008398   .0020172    -4.16   0.000    -.0123517   -.0044444
#          ptn |  -.0113146   .0021819    -5.19   0.000     -.015591   -.0070383
#       worker |   .6623018   .0519909    12.74   0.000     .5604015     .764202
#      weekend |   .3009323   .0362682     8.30   0.000     .2298479    .3720167
# -------------+----------------------------------------------------------------
#      /logey0 |   .2654423   .1550127     1.71   0.087    -.0383769    .5692616
# ------------------------------------------------------------------------------
# Instruments for equation 1: pt cbd ptn worker weekend _cons
#
# . * multiple phenotype examples
# .
# . webuse trip, clear
# (Household trips)
#
# .
# . ** without covariates
# .
# . ** ivpoisson fit
# . ivpoisson gmm trips (tcost cbd = ptn worker), multiplicative nolog
#
# Final GMM criterion Q(b) =  3.12e-29
#
# note: model is exactly identified
#
# Exponential mean model with endogenous regressors
#
# Number of parameters =   3                         Number of obs  =      5,000
# Number of moments    =   3
# Initial weight matrix: Unadjusted
# GMM weight matrix:     Robust
#
# ------------------------------------------------------------------------------
#   |               Robust
# trips |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   tcost |   .4214074   .0700091     6.02   0.000     .2841921    .5586228
# cbd |    .075126   .1304184     0.58   0.565    -.1804893    .3307413
# _cons |  -5.359227    1.47909    -3.62   0.000    -8.258191   -2.460263
# ------------------------------------------------------------------------------
#   Instrumented:  tcost cbd
# Instruments:   ptn worker
#
# . lincom _cons, eform
#
# ( 1)  [trips]_cons = 0
#
# ------------------------------------------------------------------------------
#   trips |     exp(b)   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   (1) |   .0047045   .0069584    -3.62   0.000     .0002591    .0854124
# ------------------------------------------------------------------------------
#
#   .
# . *** moment conditions 1
# . local y trips
#
# . local x tcost cbd
#
# . local z ptn worker
#
# . gmm (`y'*exp(-1*{xb:`x'}) - {ey0}), ///
# >     instruments(`z') tracelevel("none")
#
# Final GMM criterion Q(b) =  5.27e-09
#
# note: model is exactly identified
#
# GMM estimation
#
# Number of parameters =   3
# Number of moments    =   3
# Initial weight matrix: Unadjusted                 Number of obs   =      5,000
# GMM weight matrix:     Robust
#
# ------------------------------------------------------------------------------
#   |               Robust
# |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   tcost |   .4213734   .0699533     6.02   0.000     .2842675    .5584793
# cbd |   .0751167   .1303905     0.58   0.565    -.1804441    .3306774
# -------------+----------------------------------------------------------------
#   /ey0 |   .0047057   .0069581     0.68   0.499    -.0089319    .0183433
# ------------------------------------------------------------------------------
#   Instruments for equation 1: ptn worker _cons
#
# .
# . *** moment conditions 2
# . local y trips
#
# . local x tcost cbd
#
# . local z ptn worker
#
# . gmm (`y'*exp(-1*{xb:`x'} - {logey0}) - 1), ///
# >     instruments(`z') tracelevel("none")
#
# Final GMM criterion Q(b) =  1.21e-33
#
# note: model is exactly identified
#
# GMM estimation
#
# Number of parameters =   3
# Number of moments    =   3
# Initial weight matrix: Unadjusted                 Number of obs   =      5,000
# GMM weight matrix:     Robust
#
# ------------------------------------------------------------------------------
#   |               Robust
# |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   tcost |   .4214074   .0700091     6.02   0.000     .2841921    .5586228
# cbd |    .075126   .1304184     0.58   0.565    -.1804893    .3307413
# -------------+----------------------------------------------------------------
#   /logey0 |  -5.359227    1.47909    -3.62   0.000    -8.258191   -2.460264
# ------------------------------------------------------------------------------
#   Instruments for equation 1: ptn worker _cons
#
# .
# . ** including covariates
# . ivpoisson gmm trips weekend (tcost cbd = ptn worker), multiplicative nolog
#
# Final GMM criterion Q(b) =  2.33e-32
#
# note: model is exactly identified
#
# Exponential mean model with endogenous regressors
#
# Number of parameters =   4                         Number of obs  =      5,000
# Number of moments    =   4
# Initial weight matrix: Unadjusted
# GMM weight matrix:     Robust
#
# ------------------------------------------------------------------------------
#   |               Robust
# trips |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   tcost |   .4252243   .0750159     5.67   0.000     .2781958    .5722528
# cbd |   .0773626   .1414159     0.55   0.584    -.1998075    .3545328
# weekend |   .2157507   .0804855     2.68   0.007      .058002    .3734994
# _cons |  -5.484431    1.57664    -3.48   0.001    -8.574588   -2.394274
# ------------------------------------------------------------------------------
#   Instrumented:  tcost cbd
# Instruments:   weekend ptn worker
#
# . lincom _cons, eform
#
# ( 1)  [trips]_cons = 0
#
# ------------------------------------------------------------------------------
#   trips |     exp(b)   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#   (1) |   .0041509   .0065445    -3.48   0.001     .0001888    .0912389
# ------------------------------------------------------------------------------
#
#   .
# . *** moment conditions 1
# . local y trips
#
# . local x tcost cbd
#
# . local z ptn worker
#
# . local covars weekend
#
# . gmm (`y'*exp(-1*{xb:`x' `covars'}) - {ey0}), ///
#   >     instruments(`z' `covars') tracelevel("none")
#
# Final GMM criterion Q(b) =  2.73e-12
#
# note: model is exactly identified
#
# GMM estimation
#
# Number of parameters =   4
# Number of moments    =   4
# Initial weight matrix: Unadjusted                 Number of obs   =      5,000
# GMM weight matrix:     Robust
#
# ------------------------------------------------------------------------------
#              |               Robust
#              |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#        tcost |   .4252238   .0750177     5.67   0.000     .2781917    .5722559
#          cbd |   .0773625   .1414325     0.55   0.584    -.1998401     .354565
#      weekend |   .2157505   .0804888     2.68   0.007     .0579953    .3735057
# -------------+----------------------------------------------------------------
#         /ey0 |   .0041509    .006545     0.63   0.526    -.0086771    .0169789
# ------------------------------------------------------------------------------
# Instruments for equation 1: ptn worker weekend _cons
#
# .
# . *** moment conditions 2
# . local y trips
#
# . local x tcost cbd
#
# . local z ptn worker
#
# . local covars weekend
#
# . gmm (`y'*exp(-1*{xb:`x' `covars'} - {logey0}) - 1), ///
# >     instruments(`z' `covars') tracelevel("none")
#
# Final GMM criterion Q(b) =  2.43e-32
#
# note: model is exactly identified
#
# GMM estimation
#
# Number of parameters =   4
# Number of moments    =   4
# Initial weight matrix: Unadjusted                 Number of obs   =      5,000
# GMM weight matrix:     Robust
#
# ------------------------------------------------------------------------------
#              |               Robust
#              |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#        tcost |   .4252243   .0750159     5.67   0.000     .2781958    .5722528
#          cbd |   .0773626   .1414159     0.55   0.584    -.1998075    .3545328
#      weekend |   .2157507   .0804855     2.68   0.007      .058002    .3734994
# -------------+----------------------------------------------------------------
#      /logey0 |  -5.484431    1.57664    -3.48   0.001    -8.574588   -2.394274
# ------------------------------------------------------------------------------
# Instruments for equation 1: ptn worker weekend _cons


# Single instrument example
test_that("Single instrument example", {
  library(ivtools)
  # Data generation from the example in the ivtools::ivglm() helpfile
  set.seed(9)
  n <- 1000
  psi0 <- 0.5
  Z <- rbinom(n, 1, 0.5)
  X <- rbinom(n, 1, 0.7*Z + 0.2*(1 - Z))
  m0 <- plogis(1 + 0.8*X - 0.39*Z)
  Y <- rbinom(n, 1, plogis(psi0*X + log(m0/(1 - m0))))
  dat <- data.frame(Z, X, Y)
  fit01 <- msmm(Y ~ X | Z, data = dat)
  summary(fit01)
  msmm(Y ~ X | Z, data = dat, estmethod = "gmm")
  msmm(Y ~ X | Z, data = dat, estmethod = "gmmalt")
  msmm(Y ~ X | Z, data = dat, estmethod = "tsls")
  msmm(Y ~ X | Z, data = dat, estmethod = "tslsalt")
  mod <- msmm(Y ~ X | Z, data = dat)
  smy <- summary(mod)
  class(mod)
  class(smy)
  mod
  smy
  print(mod)
  print(smy)


})

#' # check variables with different names
#' dat$E <- dat$X
#' dat$R <- dat$Y
#' dat$W <- dat$Z
#' msmm(R ~ E | W, data = dat)



#' # Multiple exposure example
#' set.seed(123456)
#' n <- 1000
#' psi0 <- 0.5
#' psi1 <- 0.4
#' G1 <- rbinom(n, 2, 0.5)
#' G2 <- rbinom(n, 2, 0.3)
#' G3 <- rbinom(n, 2, 0.4)
#' U <- runif(n)
#' pX1 <- plogis(0.7*G1 + G2 - G3 + U)
#' X1 <- rbinom(n, 1, pX1)
#' pX2 <- plogis(-1 + 0.2*G1 - 0.2*G2 + 0.4*G3 + U)
#' X2 <- rbinom(n, 1, pX2)
#' pY <- plogis(-2 + psi0*X1 + psi1*X2 + U)
#' Y <- rbinom(n, 1, pY)
#' table(Y)
#' dat <- data.frame(G1, G2, G3, X1, X2, Y)
#' msmm(Y ~ X1 + X2 | G1 + G2 + G3, data = dat)
#' msmm(Y ~ X1 + X2 | G1 + G2 + G3, data = dat, estmethod = "gmm")
#' msmm(Y ~ X1 + X2 | G1 + G2 + G3, data = dat, estmethod = "gmmalt")
#' try(msmm(Y ~ X1 + X2 | G1 + G2 + G3, data = dat, estmethod = "tsls"))
#' try(msmm(Y ~ X1 + X2 | G1 + G2 + G3, data = dat, estmethod = "tslsalt"))
#'
#' # With different variable names
#' E1 <- X1
#' E2 <- X2
#' R <- Y
#' dat <- data.frame(G1, G2, G3, E1, E2, R)
#' msmm(Y ~ E1 + E2 | G1 + G2 + G3, data = dat)


#' # Multiple instrument example
#' set.seed(123456)
#' n <- 1000
#' psi0 <- 0.5
#' G1 <- rbinom(n, 2, 0.5)
#' G2 <- rbinom(n, 2, 0.3)
#' G3 <- rbinom(n, 2, 0.4)
#' U <- runif(n)
#' pX <- plogis(0.7*G1 + G2 - G3 + U)
#' X <- rbinom(n, 1, pX)
#' pY <- plogis(-2 + psi0*X + U)
#' Y <- rbinom(n, 1, pY)
#' dat <- data.frame(G1, G2, G3, X, Y)
#' msmm(Y ~ X | G1 + G2 + G3, data = dat)
#' msmm(Y ~ X | G1 + G2 + G3, data = dat, estmethod = "gmm")
#' msmm(Y ~ X | G1 + G2 + G3, data = dat, estmethod = "gmmalt")
#' msmm(Y ~ X | G1 + G2 + G3, data = dat, estmethod = "tsls")
#' msmm(Y ~ X | G1 + G2 + G3, data = dat, estmethod = "tslsalt")


#' # non-binary x with tsls, tslsalt methods fail
#' set.seed(9)
#' n <- 1000
#' psi0 <- 0.5
#' Z <- rbinom(n, 1, 0.5)
#' X <- rbinom(n, 1, 0.7*Z + 0.2*(1 - Z))
#' m0 <- plogis(1 + 0.8*X - 0.39*Z)
#' Y <- rbinom(n, 1, plogis(psi0*X + log(m0/(1 - m0))))
#' dat <- data.frame(Z, X, Y)
#' dat$X[1] <- 2
#' try(msmm(Y ~ X | Z, data = dat, estmethod = "tsls"))
#' try(msmm(Y ~ X | Z, data = dat, estmethod = "tslsalt"))

#' # non-binary y fail
#' set.seed(9)
#' n <- 1000
#' psi0 <- 0.5
#' Z <- rbinom(n, 1, 0.5)
#' X <- rbinom(n, 1, 0.7*Z + 0.2*(1 - Z))
#' m0 <- plogis(1 + 0.8*X - 0.39*Z)
#' Y <- rbinom(n, 1, plogis(psi0*X + log(m0/(1 - m0))))
#' dat <- data.frame(Z, X, Y)
#' dat$Y[1] <- 2
#' try(msmm(Y ~ X | Z, data = dat))
