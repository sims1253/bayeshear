#' Calculate R* convergence diagnostic
#'
#' A convenient wrapper around \code{\link[posterior]{rstar}}
#'
#' @param x brmsfit object or posterior draws object, eg. from
#'          \code{\link[posterior]{as_draws}}
#' @param ... Additional arguments for \code{\link[posterior]{rstar}}
#'
#' @return A numeric vector of length 1 (by default) or length nsimulations
#'  (if uncertainty = TRUE).
#' @export rstar_w rstar_w.brmsfit rstar_w.draws
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
rstar_w.draws <- function(posterior_draws, ...) {
  posterior::rstar(posterior_draws, ...)
}


#' Return number of divergent transitions
#'
#' @param fit brmsfit object
#'
#' @return Number of divergent transitions
#' @export divergent_transitions divergent_transitions.brmsfit
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' divergent_transitions(fit)
divergent_transitions <- function(fit) {
  UseMethod("divergent_transitions")
}

#' @export
divergent_transitions.brmsfit <- function(fit) {
  nuts_diag <- brms::nuts_params(fit)
  return(sum(nuts_diag$Value[which(nuts_diag$Parameter == "divergent__")]))
}


#' Bad pareto-khat estimate values
#'
#' @param x brmsfit object or psis object
#' @param threshold Threshold value to cound pareto-khat value as bad.
#'                  Default = 0.7.
#'
#' @return Number of bad pareto-khat estimate values
#' @export bad_pareto_ks bad_pareto_ks.brmsfit bad_pareto_ks.psis
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' bad_pareto_ks(fit)
bad_pareto_ks <- function(fit, threshold = 0.7) {
  UseMethod("bad_pareto_ks")
}

#' @export
bad_pareto_ks.brmsfit <- function(fit, threshold = 0.7) {
  psis_object <- brms:::.psis(fit, newdata = fit$data, resp = NULL)
  return(length(which(psis_object$diagnostics$pareto_k > threshold)))
}

#' @export
bad_pareto_ks.psis <- function(psis_object, threshold = 0.7) {
  return(length(which(psis_object$diagnostics$pareto_k > threshold)))
}

#' Pareto-khat estimate values
#'
#' @param x brmsfit or psis object
#'
#' @return List of all pareto-khat estimate values
#' @export pareto_ks pareto_ks.brmsfit pareto_ks.psis
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' bad_pareto_ks(fit)
pareto_ks <- function(x) {
  UseMethod("pareto_ks")
}

#' @export
pareto_ks.brmsfit <- function(fit) {
  psis_object <- brms:::.psis(fit, newdata = fit$data, resp = NULL)
  return(psis_object$diagnostics$pareto_k)
}

#' @export
pareto_ks.psis <- function(psis_object) {
  return(psis_object$diagnostics$pareto_k)
}

#' MCMC sampling times
#'
#' @param fit brmsfit object
#'
#' @return List containing warmup and sampling time
#' @export
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' #' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' bad_pareto_ks(fit)(fit)
sampling_time <- function(fit) {
  ltime <- lapply(
    fit$fit@sim$samples,
    function(x) attr(x, "elapsed_time")
  )
  times <- do.call(rbind, ltime)
  return(
    list(
      "warmup_time" = sum(times[, 1]),
      "sampling_time" = sum(times[, 2])
    )
  )
}
