# Changelog

## OneSampleMR v0.1.9

CRAN release: 2026-10-21

- New function
  [`lsmm()`](https://remlapmot.github.io/OneSampleMR/reference/lsmm.md)
  fits the double logistic structural mean model (LSMM) of Vansteelandt
  and Goetghebeur (2003), which estimates a causal odds ratio, by
  two-step GMM with multiple instruments as described by Clarke et
  al. (2015). It has [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) methods, the
  summary includes the Hansen J-test of the overidentifying
  restrictions, and covariates are handled as in
  [`msmm()`](https://remlapmot.github.io/OneSampleMR/reference/msmm.md).
  It is the same estimator as the `ivlsmm` command in the Stata package
  ivonesamplemr, and its estimates agree with `ivlsmm`.

- [`tsps()`](https://remlapmot.github.io/OneSampleMR/reference/tsps.md)
  and
  [`tsri()`](https://remlapmot.github.io/OneSampleMR/reference/tsri.md)
  with `link = "logmult"` now converge to the solution of the estimating
  equations. Previously the GMM optimizer often stopped at its iteration
  limit, giving slightly inaccurate estimates and a non-convergence
  warning. The starting values are now obtained by solving the second
  stage estimating equations exactly by Newton-Raphson, rather than from
  a gamma regression with zero outcomes replaced by 0.001. The estimates
  now agree with the Stata commands `ivtsps` and `ivtsri` in the
  ivonesamplemr package.

- [`tsps()`](https://remlapmot.github.io/OneSampleMR/reference/tsps.md)
  and
  [`tsri()`](https://remlapmot.github.io/OneSampleMR/reference/tsri.md)
  no longer give a spurious non-convergence warning when the optimizer
  has reached the minimum of the GMM objective function (which is 0
  because these models are exactly identified).

- [`msmm()`](https://remlapmot.github.io/OneSampleMR/reference/msmm.md)
  with `estmethod = "gmm"` or `"gmmalt"` now uses a tighter convergence
  tolerance for the Nelder-Mead optimizer
  (`reltol = .Machine$double.eps`, `maxit = 1E4`), so that the estimates
  converge to the minimum of the GMM objective function. Previously the
  default tolerance could stop the optimizer early, so estimates
  depended on the starting values and could differ in the second or
  third decimal place from the solution of the estimating equations.
  Estimates from exactly identified models now agree with
  `ivtools::ivglm()` G-estimation and with Stata’s
  `ivpoisson gmm, multiplicative`.

- The parameters in the output of
  [`tsps()`](https://remlapmot.github.io/OneSampleMR/reference/tsps.md)
  and
  [`tsri()`](https://remlapmot.github.io/OneSampleMR/reference/tsri.md)
  are now labelled by stage and variable name. First stage parameters
  are prefixed `stage1:`, the second stage exposure and covariate
  parameters take their variable names (previously, e.g., `xhatxhat` and
  `xhatcovariates` for
  [`tsps()`](https://remlapmot.github.io/OneSampleMR/reference/tsps.md)),
  and the first stage residual in
  [`tsri()`](https://remlapmot.github.io/OneSampleMR/reference/tsri.md)
  is labelled `stage1:residuals` (previously `resres`). The same labels
  are used when the user specifies `t0`, and
  [`tsps()`](https://remlapmot.github.io/OneSampleMR/reference/tsps.md)
  and
  [`tsri()`](https://remlapmot.github.io/OneSampleMR/reference/tsri.md)
  now give a clear error message if `t0` has the wrong length. The
  documentation of `t0` has been corrected.

- The print and summary methods for
  [`tsps()`](https://remlapmot.github.io/OneSampleMR/reference/tsps.md)
  and
  [`tsri()`](https://remlapmot.github.io/OneSampleMR/reference/tsri.md)
  fits now only exponentiate the exposure parameter in the causal risk
  ratio or causal odds ratio table. Previously all the second stage
  parameters were exponentiated and presented as causal ratios,
  including the intercept, any covariates, and, for
  [`tsri()`](https://remlapmot.github.io/OneSampleMR/reference/tsri.md),
  the coefficient of the first stage residuals.

- [`msmm()`](https://remlapmot.github.io/OneSampleMR/reference/msmm.md)
  now distinguishes covariates (variables included in both the exposure
  and instrument parts of the formula) from exposures. The causal risk
  ratio table (`crrci`) now only contains the exposures; previously it
  also reported the exponentiated covariate coefficients as causal risk
  ratios. The covariate adjusted model itself is unchanged, and is the
  same model as fitted by Stata’s `ivpoisson gmm, multiplicative` with
  exogenous covariates. The returned object has a new `covariates`
  element, the print methods list the covariates adjusted for and note
  that `E[Y(0)]` is then estimated at covariate values of 0, and
  covariate adjustment is documented with an example in
  [`?msmm`](https://remlapmot.github.io/OneSampleMR/reference/msmm.md).
  Covariates with the `"tsls"` and `"tslsalt"` methods now give a clear
  error message.

## OneSampleMR 0.1.8

CRAN release: 2026-07-08

- Bump roxygen2 to 8.0.0.

- Fixed a bug in
  [`fsw()`](https://remlapmot.github.io/OneSampleMR/reference/fsw.md)
  for models fitted by
  [`ivreg::ivreg()`](https://zeileis.github.io/ivreg/reference/ivreg.html)
  which reported conditional F statistics for the wrong variables when
  exogenous covariates were listed before the exposures in the model
  formula

- Fixed a bug in
  [`tsps()`](https://remlapmot.github.io/OneSampleMR/reference/tsps.md)
  and
  [`tsri()`](https://remlapmot.github.io/OneSampleMR/reference/tsri.md)
  which produced incorrect estimates when exogenous covariates were
  listed before the exposure or the instruments in the model formula

- [`tsps()`](https://remlapmot.github.io/OneSampleMR/reference/tsps.md)
  and
  [`tsri()`](https://remlapmot.github.io/OneSampleMR/reference/tsri.md)
  now give a clear error message when more than one exposure is
  specified, when a variable is named `y` (which is reserved for
  internal use), and their print methods no longer error when a user
  specified unnamed `t0` is used with a log-link or logit-link model

- [`fsw()`](https://remlapmot.github.io/OneSampleMR/reference/fsw.md)
  now detects ordered factor exposures, and its factor exposure error
  message is restored for the
  [`ivreg::ivreg()`](https://zeileis.github.io/ivreg/reference/ivreg.html)
  method and fixed for the
  [`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html)
  method

- [`msmm()`](https://remlapmot.github.io/OneSampleMR/reference/msmm.md)
  now gives its intended error message for non-integer outcomes and a
  corrected message for negative outcomes

- [`tsps()`](https://remlapmot.github.io/OneSampleMR/reference/tsps.md)
  and
  [`tsri()`](https://remlapmot.github.io/OneSampleMR/reference/tsri.md)
  are now roughly 2 times faster because the first stage regression is
  computed once instead of on every evaluation of the GMM moment
  functions, and the moment conditions in all three GMM based estimators
  are now vectorised

- Internal refactoring: the four
  [`fsw()`](https://remlapmot.github.io/OneSampleMR/reference/fsw.md)
  methods now share a common engine, the eight
  [`tsps()`](https://remlapmot.github.io/OneSampleMR/reference/tsps.md)/[`tsri()`](https://remlapmot.github.io/OneSampleMR/reference/tsri.md)
  moment functions are reduced to one per estimator, and dead code has
  been removed

## OneSampleMR 0.1.7

CRAN release: 2026-03-09

- [`fsw()`](https://remlapmot.github.io/OneSampleMR/reference/fsw.md)
  can now calculate Sanderson-Windmeijer conditional F statistics for
  models fitted by
  [`AER::ivreg()`](https://rdrr.io/pkg/AER/man/ivreg.html),
  [`estimatr::iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.html),
  and
  [`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html)
  (thanks [@nvitt](https://github.com/nvitt))

## OneSampleMR 0.1.6

CRAN release: 2025-05-21

- Fixed a bug in
  [`fsw()`](https://remlapmot.github.io/OneSampleMR/reference/fsw.md)
  when an exogenous covariate was a factor (thanks
  [@ZoeReed](https://github.com/ZoeReed))

- The
  [`fsw()`](https://remlapmot.github.io/OneSampleMR/reference/fsw.md)
  function now returns an error if any of the exposures are factors

## OneSampleMR 0.1.5

CRAN release: 2024-06-26

- Replaced dots checking with rlang instead of the ellipsis package
  (thanks [@olivroy](https://github.com/olivroy)).

## OneSampleMR 0.1.4

CRAN release: 2024-06-13

- Add `CITATION` file

- Tweak formatting of code in helpfile examples and vignettes

- Add subsection to README about GNU Fortran requirement for old
  versions of the **gmm** package on macOS

- Bumped the minimum required version of R to be 4.1.0. This is because
  of the same requirement in **pbkrtest**, which is a dependency of
  **car** which is a dependency of **ivreg**

- Bumped version of **roxygen2** used to create the package
  documentation

- Made use of ellipsis `...` in several function definitions safer using
  the **ellipsis** package

- Bumped required version of R to 4.3.0 because the **Matrix** package,
  which is a dependency of **ivreg** now requires R 4.4.0. CRAN retain a
  binary version of **Matrix** 1.6-5 for the previous release of R, and
  so the minimum version of R can be 4.3.0 rather than 4.4.0.

## OneSampleMR 0.1.3

CRAN release: 2023-05-03

- Update **roxygen2** version used to generate documentation

- Reduce the number of functions imported from other packages

## OneSampleMR 0.1.2

CRAN release: 2022-05-11

- Make use of packages in Suggests list conditional on their
  availability

- Remove **ivtools** from Suggests due to the current failure of its
  **ahaz** dependency package to build on 3 of the CRAN Linux
  sub-architectures

## OneSampleMR 0.1.1

CRAN release: 2022-04-28

- Fix an issue causing an `R CMD check` note in R 4.2.0

## OneSampleMR 0.1.0

CRAN release: 2021-11-12

- Initial submission to CRAN
