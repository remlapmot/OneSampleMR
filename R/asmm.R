#' Additive structural mean model
#'
#' `asmm` is not a function. This helpfile is to note that the additive structural mean model (ASMM)
#' is simply fit with a linear IV estimator, such as available in [ivreg::ivreg()].
#'
#' For a binary outcome the ASMM estimates a causal risk difference.
#'
#' @references
#' Clarke PS, Palmer TM, Windmeijer F. Estimating structural
#' mean models with multiple instrumental variables using the
#' Generalised Method of Moments. Statistical Science, 2015, 30, 1,
#' 96-117. \doi{10.1214/14-STS503}
#'
#' Palmer TM, Sterne JAC, Harbord RM, Lawlor DA, Sheehan NA, Meng S,
#' Granell R, Davey Smith G, Didelez V.
#' Instrumental variable estimation of causal risk ratios and causal odds ratios
#' in Mendelian randomization analyses.
#' American Journal of Epidemiology, 2011, 173, 12, 1392-1403.
#' \doi{10.1093/aje/kwr026}
#'
#' Robins JM. The analysis of randomised and
#' nonrandomised AIDS treatment trials using a new approach to
#' causal inference in longitudinal studies.
#' In Health Service Research Methodology: A Focus on AIDS
#' (L. Sechrest, H. Freeman and A. Mulley, eds.). 1989. 113–159.
#' US Public Health Service, National Center for Health Services Research,
#' Washington, DC.
#'
#' @examples
#' # Single instrument example
#' # Data generation from the example in the ivtools ivglm() helpfile
#' set.seed(9)
#' n <- 1000
#' psi0 <- 0.5
#' Z <- rbinom(n, 1, 0.5)
#' X <- rbinom(n, 1, 0.7*Z + 0.2*(1 - Z))
#' m0 <- plogis(1 + 0.8*X - 0.39*Z)
#' Y <- rbinom(n, 1, plogis(psi0*X + log(m0/(1 - m0))))
#' dat1 <- data.frame(Z, X, Y)
#' fit1 <- ivreg::ivreg(Y ~ X | Z, data = dat1)
#' summary(fit1)
#'
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
#' dat2 <- data.frame(G1, G2, G3, X, Y)
#' fit2 <- ivreg::ivreg(Y ~ X | G1 + G2 + G3, data = dat2)
#' summary(fit2)
#' @name asmm
NULL
