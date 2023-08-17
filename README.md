# OneSampleMR
<!-- badges: start -->
[![R-CMD-check](https://github.com/remlapmot/OneSampleMR/workflows/R-CMD-check/badge.svg)](https://github.com/remlapmot/OneSampleMR/actions?workflow=R-CMD-check)
[![CRAN status](https://www.r-pkg.org/badges/version/OneSampleMR)](https://cran.r-project.org/package=OneSampleMR)
[![RStudio_CRAN_mirror_downloads_badge](http://cranlogs.r-pkg.org/badges/grand-total/OneSampleMR?color=blue)](https://CRAN.R-project.org/package=OneSampleMR)
[![Codecov test coverage](https://codecov.io/gh/remlapmot/OneSampleMR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/remlapmot/OneSampleMR?branch=main)
[![r-universe](https://remlapmot.r-universe.dev/badges/OneSampleMR)](https://remlapmot.r-universe.dev/OneSampleMR)
<!-- badges: end -->

Useful functions for one sample, individual level data, Mendelian randomization / instrumental variable 
analyses, including implementations of:

* The [Sanderson and Windmeijer (2016)](https://doi.org/10.1016/j.jeconom.2015.06.004) conditional F-statistic for multiple exposure models.
* Various one-sample instrumental variable estimators including the
  * Multiplicative structural mean model (Robins, 1989; [Hernán and Robins, 2006](https://doi.org/10.1097/01.ede.0000222409.00878.37))<!--  * Logistic structural mean model (Robins, 1989; [Vansteelandt and Goetghebeur, 2003](https://doi.org/10.1046/j.1369-7412.2003.00417.x)) -->
  * Two-stage predictor substitution estimators ([Terza, 2008](https://doi.org/10.1016/j.jhealeco.2007.09.009))
  * Two-stage residual inclusion estimators ([Terza, 2008](https://doi.org/10.1016/j.jhealeco.2007.09.009))

**OneSampleMR** was included in the R Views November 2021 "Top 40" New CRAN Packages [here](https://rviews.rstudio.com/2021/12/21/november-2021-top-40-new-cran-packages/)!

## Installation

### Released version

Install the package from CRAN using
``` r
install.packages("OneSampleMR")
```

To check for updates run
``` r
update.packages()
```

### Issue with gmm package versions 1.6-5, 1.6-6, 1.7 on macOS

Using these old versions of the **gmm** package, a GNU Fortran installation was required even if the binary version of the package was installed.

The fix is to update to the current version of **gmm**, which is version 1.8.

If you cannot update your version of **gmm** then

* The current version of GNU Fortran to install is documented [here](https://mac.r-project.org/tools/) and available [here](https://mac.r-project.org/tools/gfortran-12.2-universal.pkg)
* Note that the version of Fortran available from Homebrew will probably not work

### Development version

Install the development version of the package from source from GitHub using
``` r
# install.packages("remotes") # uncomment on first run
remotes::install_github("remlapmot/OneSampleMR")
```

or from r-universe using
``` r
install.packages('OneSampleMR', repos = c('https://remlapmot.r-universe.dev', 'https://cloud.r-project.org'))
```

To check for and install updates to the development version run the previous command again or run
``` r
remotes::update_packages()
```
