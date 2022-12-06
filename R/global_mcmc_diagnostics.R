#' Return number of divergent transitions
#'
#' @param fit An brmsfit or stanfit object.
#' @param absolute True if the number of divergent transitions should be
#'                 returned. Otherwise the fraction of divergent transitons to
#'                 total post-warmup samples is returned as default.
#' @param ... Potential future arguments.
#'
#' @return Number of divergent transitions
#' @export divergent_transitions divergent_transitions.brmsfit
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' divergent_transitions(fit)
divergent_transitions <- function(fit, absolute = FALSE, ...) {
  UseMethod("divergent_transitions")
}

#' @export
divergent_transitions.brmsfit <- function(fit, absolute = FALSE, ...) {
  # Simply extract the stanfit object and pass it along
    divergent_transitions(fit$fit, absolute, ...)
}

#' @export
divergent_transitions.stanfit <- function(fit, absolute = FALSE, ...) {
  nuts_diag <- bayesplot::nuts_params(fit)
  if (absolute) {
    return(sum(nuts_diag$Value[which(nuts_diag$Parameter == "divergent__")]))
  } else {
    if (is.null(fit@sim$thin) || is.null(fit@sim$chains)) {
      return(NA)
    }
    return(
      sum(nuts_diag$Value[which(nuts_diag$Parameter == "divergent__")]) /
        ceiling(
          (fit@sim$iter - fit@sim$warmup) / fit@sim$thin * fit@sim$chains
        )
    )
  }
}


#' Bad pareto-khat estimate values
#'
#' @param x brmsfit object or psis object
#' @param threshold Threshold value to count pareto-khat value as bad.
#'                  Default = 0.7.
#' @param ... Potential future parameters.
#'
#' @return Number of bad pareto-khat estimate values
#' @export bad_pareto_ks bad_pareto_ks.brmsfit bad_pareto_ks.psis
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' bad_pareto_ks(fit)
bad_pareto_ks <- function(x, threshold = 0.7, absolute = FALSE, ...) {
  # TODO Check the latest paper on pareto k and make the threshold dynamic
  # as described in the paper.
  UseMethod("bad_pareto_ks")
}

#' @export
bad_pareto_ks.brmsfit <- function(x, threshold = 0.7, ...) {
  psis_object <- brms:::.psis(x, newdata = x$data, resp = NULL)
  return(length(which(psis_object$diagnostics$pareto_k > threshold)))
}

#' @export
bad_pareto_ks.psis <- function(x, threshold = 0.7, ...) {
  return(length(which(x$diagnostics$pareto_k > threshold)))
}

#' Pareto-khat estimate values
#'
#' @param x brmsfit or psis object
#'
#' @return List of all pareto-khat estimate values
#' @export pareto_k_values pareto_k_values.brmsfit pareto_k_values.psis
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' bad_pareto_ks(fit)
pareto_k_values <- function(x) {
  UseMethod("pareto_k_values")
}

#' @export
pareto_k_values.brmsfit <- function(x) {
  psis_object <- brms:::.psis(x, newdata = x$data, resp = NULL)
  return(psis_object$diagnostics$pareto_k)
}

#' @export
pareto_k_values.psis <- function(x) {
  return(x$diagnostics$pareto_k)
}

#' MCMC sampling times
#'
#' @param fit An brmsfit or stanfit object.
#' @param absolute TRUE if the absolute times should be returned, FALSE
#'                 (default) if time per sample should be returned.
#' @param ... Potential future parameters.
#'
#' @return List containing warmup and sampling time
#' @export sampling_time sampling_time.brmsfit sampling_time.stanfit
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' #' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' bad_pareto_ks(fit)
sampling_time <- function(fit, absolute = FALSE, ...) {
 UseMethod("sampling_time")
}

#' @export
sampling_time.brmsfit <- function(fit, absolute = FALSE, ...) {
  # Just extract the stanfit object and pass it along
  sampling_time(fit$fit, absolute, ...)
}

#' @export
sampling_time.stanfit <- function(fit, absolute = FALSE, ...) {
  ltime <- lapply(
    fit@sim$samples,
    function(x) attr(x, "elapsed_time")
  )
  times <- do.call(rbind, ltime)
  if (absolute) {
    return(
      list(
        "warmup_time_abs" = sum(times[, 1]),
        "sampling_time_abs" = sum(times[, 2])
      )
    )
  } else {
    return(
      list(
        "warmup_time_rel" = sum(times[, 1]) /
          (fit@sim$warmup * fit@sim$chains),
        "sampling_time_rel" = sum(times[, 2]) /
          ((fit@sim$iter - fit@sim$warmup) * fit@sim$chains)
      )
    )
  }
}
