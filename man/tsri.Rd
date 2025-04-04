% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tsri.R
\name{tsri}
\alias{tsri}
\title{Two-stage residual inclusion (TSRI) estimators}
\usage{
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
}
\arguments{
\item{formula, instruments}{formula specification(s) of the regression
relationship and the instruments. Either \code{instruments} is missing and
\code{formula} has three parts as in \code{y ~ x1 + x2 | z1 + z2 + z3}
(recommended) or \code{formula} is \code{y ~ x1 + x2} and \code{instruments}
is a one-sided formula \code{~ z1 + z2 + z3} (only for backward
compatibility).}

\item{data}{an optional data frame containing the variables in the model.
By default the variables are taken from the environment of the
\code{formula}.}

\item{subset}{an optional vector specifying a subset of observations to be
used in fitting the model.}

\item{na.action}{a function that indicates what should happen when the data
contain \code{NA}s. The default is set by the \code{na.action} option.}

\item{contrasts}{an optional list. See the \code{contrasts.arg} of
\code{\link[stats:model.matrix]{stats::model.matrix()}}.}

\item{t0}{A vector of starting values for the gmm optimizer. This should
have length equal to the number of exposures plus 1.}

\item{link}{character; one of \code{"identity"} (the default), \code{"logadd"}, \code{"logmult"}, \code{"logit"}.
This is the link function for the second stage model. \code{"identity"} corresponds to linear
regression; \code{"logadd"} is log-additive and corresponds to Poisson / log-binomial regression;
\code{"logmult"} is log-multiplicative and corresponds to gamma regression;
\code{"logit"} corresponds to logistic regression.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
An object of class \code{"tsri"} with the following elements
\describe{
\item{fit}{the fitted object of class \code{"gmm"} from the call to \code{\link[gmm:gmm]{gmm::gmm()}}.}
\item{estci}{a matrix of the estimates with their corresponding confidence interval limits.}
\item{link}{a character vector containing the specified link function.}
}
}
\description{
An excellent description of TSRI estimators is given by Terza et al.
(2008). TSRI estimators proceed by fitting a first stage model of the
exposure regressed upon the instruments (and possibly any measured
confounders). From this the first stage residuals are estimated.
A second stage model is then fitted of the outcome regressed upon
the exposure and first stage residuals (and possibly measured
confounders).
}
\details{
TSRI estimators are sometimes described as a special case of
control function estimators.

\code{tsri()} performs GMM estimation to ensure appropriate standard errors
on its estimates similar to that described that described by
Clarke et al. (2015). Terza (2017) described an alternative approach.
}
\examples{
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
}
\references{
Bowden J, Vansteelandt S.
Mendelian randomization analysis of case-control data using
structural mean models.
Statistics in Medicine, 2011, 30, 6, 678-694.
\doi{10.1002/sim.4138}

Clarke PS, Palmer TM, Windmeijer F. Estimating structural
mean models with multiple instrumental variables using the
Generalised Method of Moments. Statistical Science, 2015, 30, 1,
96-117. \doi{10.1214/14-STS503}

Dukes O, Vansteelandt S.
A note on G-estimation of causal risk ratios.
American Journal of Epidemiology, 2018, 187, 5, 1079-1084.
\doi{10.1093/aje/kwx347}

Palmer T, Thompson JR, Tobin MD, Sheehan NA, Burton PR.
Adjusting for bias and unmeasured confounding in Mendelian randomization
studies with binary responses.
International Journal of Epidemiology, 2008, 37, 5, 1161-1168.
\doi{10.1093/ije/dyn080}

Palmer TM, Sterne JAC, Harbord RM, Lawlor DA, Sheehan NA, Meng S,
Granell R, Davey Smith G, Didelez V.
Instrumental variable estimation of causal risk ratios and causal odds ratios
in Mendelian randomization analyses.
American Journal of Epidemiology, 2011, 173, 12, 1392-1403.
\doi{10.1093/aje/kwr026}

Terza JV, Basu A, Rathouz PJ. Two-stage residual inclusion estimation:
Addressing endogeneity in health econometric modeling.
Journal of Health Economics, 2008, 27, 3, 531-543.
\doi{10.1016/j.jhealeco.2007.09.009}

Terza JV.
Two-stage residual inclusion estimation: A
practitioners guide to Stata implementation.
The Stata Journal, 2017, 17, 4, 916-938.
\doi{10.1177/1536867X1801700409}
}
