#' Calculate quantiles based on posterior samples for a given parameter.
#'
#' @param x A fit object supported by \code{\link[posterior]{as_draws}} or a
#'          posterior draws object, eg. from \code{\link[posterior]{as_draws}}.
#' @param variables Vector of variable names of interest.
#' @param probs Vector of quantiles, passed to \code{\link{quantile}}
#'              function.
#' @param ... Additional arguments passed to \code{\link{quantile}}.
#'
#' @return Named list of quantiles.
#' @export posterior_quantiles posterior_quantiles.draws posterior_quantiles.default
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' posterior_quantiles(fit, "b_Intercept", c(0.025, 0.5, 0.975))
#'
posterior_quantiles <- function(x, variables, probs, ...) {
  UseMethod("posterior_quantiles")
}

#' @export
posterior_quantiles.default <- function(x, variables, probs, ...) {
  posterior_quantiles(posterior::as_draws(x), variables, probs, ...)
}

#' @export
posterior_quantiles.draws <- function(x, variables, probs, ...) {
  quantile_list <- lapply(
    variables,
    get_quantiles,
    draws = x,
    probs = probs,
    ... = ...
  )
  names(quantile_list) <- variables
  return(quantile_list)
}

get_quantiles <- function(draws, variable, probs, ...) {
  return(
    do.call(
      quantile,
      c(
        list(posterior::extract_variable_matrix(draws, variable = variable)),
        list(probs),
        list(...)
      )
    )
  )
}
