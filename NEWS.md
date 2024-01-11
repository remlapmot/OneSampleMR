# OneSampleMR 0.1.3.9000

* Add `CITATION` file

* Tweak formatting of code in helpfile examples and vignettes

* Add subsection to README about GNU Fortran requirement for old versions of the gmm package on macOS

* Bumped the minimum required version of R to be 4.1.0. This is because of the same requirement in pbkrtest, which is a dependency of car which is a dependency of ivreg

* Bumped version of roxygen2 used to create the package documentation

# OneSampleMR 0.1.3

* Update **roxygen2** version used to generate documentation

* Reduce the number of functions imported from other packages

# OneSampleMR 0.1.2

* Make use of packages in Suggests list conditional on their availability

* Remove **ivtools** from Suggests due to the current failure of its **ahaz** dependency package to build on 3 of the CRAN Linux sub-architectures

# OneSampleMR 0.1.1

* Fix an issue causing an `R CMD check` note in R 4.2.0

# OneSampleMR 0.1.0

* Initial submission to CRAN
