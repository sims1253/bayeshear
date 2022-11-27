#' Calculate quantiles based on posterior samples for a given parameter.
#'
#' @param x brmsfit object, posterior draws list from
#'          \code{\link[posterior]{as_draws}} or matrix from
#'          \code{\link[posterior]{extract_variable_matrix}}.
#' @param variable Name of the parameter of interest.
#' @param probs Vector of quantiles, passed to \code{\link{quantiles}} function.
#' @param ... Additional arguments passed to \code{\link{quantiles}}.
#'
#' @return Named list of quantiles.
#' @export posterior_quantiles posterior_quantiles.matrix posterior_quantiles.list posterior_quantiles.brmsfit
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = rnorm(1000))
#' posterior_quantiles(fit, "b_Intercept", c(0.025, 0.5, 0.975))
#'
posterior_quantiles <- function(x, variable, probs, ...) {
  UseMethod("posterior_quantiles")
}

#' @export
posterior_quantiles.matrix <- function(draw_matrix,
                                       variable = NULL,
                                       probs,
                                       ...) {
  return(
    do.call(
      quantile,
      c(list(draw_matrix), list(probs), list(...))
    )
  )
}


#' @export
posterior_quantiles.brmsfit <- function(fit, variable, probs, ...) {
  return(
    do.call(
      quantile,
      c(
        list(posterior::extract_variable_matrix(fit, variable = variable)),
        list(probs), list(...)
      )
    )
  )
}

#' @export
posterior_quantiles.list <- function(posterior_draws, variable, probs, ...) {
  return(
    do.call(
      quantile,
      c(list(posterior::extract_variable_matrix(posterior_draws,
        variable = variable
      )), list(probs), list(...))
    )
  )
}
