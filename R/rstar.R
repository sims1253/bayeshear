#' Calculate R* convergence diagnostic
#'
#' A convenient wrapper around \code{\link[posterior]{rstar}}
#'
#' @param x brmsfit object or posterior draws list from
#'          \code{\link[posterior]{as_draws}}
#' @param ... Additional arguments for \code{\link[posterior]{rstar}}
#'
#' @return A numeric vector of length 1 (by default) or length nsimulations
#'  (if uncertainty = TRUE).
#' @export rstar_w rstar_w.brmsfit rstar_w.list
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' rstar_w(fit)
rstar_w <- function(x, ...) {
  UseMethod("rstar_w")
}

#' @export
rstar_w.brmsfit <- function(fit, ...) {
  posterior::rstar(posterior::as_draws(fit), ...)
}

#' @export
rstar_w.list <- function(posterior_draws, ...) {
  posterior::rstar(posterior_draws, ...)
}
