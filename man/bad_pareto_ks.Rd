% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global_mcmc_diagnostics.R
\name{bad_pareto_ks}
\alias{bad_pareto_ks}
\title{Bad pareto-khat estimate values}
\usage{
bad_pareto_ks(x, threshold = 0.7, absolute = FALSE, ...)
}
\arguments{
\item{x}{brmsfit object or psis object}

\item{threshold}{Threshold value to count pareto-khat value as bad.
Default = 0.7.}

\item{...}{Potential future parameters.}
}
\value{
Number of bad pareto-khat estimate values
}
\description{
Bad pareto-khat estimate values
}
\examples{
fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
bad_pareto_ks(fit)
}
