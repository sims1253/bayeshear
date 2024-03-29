% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictive_metrics.R
\name{r2_loo}
\alias{r2_loo}
\title{PSIS-loo R²}
\usage{
r2_loo(fit, psis_object = NULL)
}
\arguments{
\item{fit}{A brmsfit object.}

\item{psis_object}{Optional psis object to prevent recalculation.}
}
\value{
Named list with R² estimate, standard error and pointwise estimate.
}
\description{
PSIS-loo R²
}
\examples{
fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
r2_loo(fit)
}
