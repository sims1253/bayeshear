#' Calculate a summary statistic of the posterio for a given parameter.
#'
#' @param x brmsfit object, posterior draws list from
#'          \code{\link[posterior]{as_draws}} or matrix from
#'          \code{\link[posterior]{extract_variable_matrix}}.
#' @param variable Name of the parameter of interest.
#' @param fun Summary function, eg mean, median or sd.
#'
#' @return The calculated summary of the posterior.
#' @export posterior_summary posterior_summary.matrix posterior_summary.list posterior_summary.brmsfit
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = rnorm(1000))
#' posterior_summary(fit, "b_Intercept", mean)
#'
posterior_summary <- function(x, variable, fun) {
  UseMethod("posterior_summary")
}

#' @export
posterior_summary.matrix <- function(draw_matrix, variable = NULL, fun) {
  return(
    do.call(
      fun,
      list(draw_matrix)
    )
  )
}


#' @export
posterior_summary.brmsfit <- function(fit, variable, fun) {
  return(
    do.call(
      fun,
      list(posterior::extract_variable_matrix(fit, variable = variable))
    )
  )
}

#' @export
posterior_summary.list <- function(posterior_draws, variable, fun) {
  return(
    do.call(
      fun,
      list(posterior::extract_variable_matrix(posterior_draws,
        variable = variable
      ))
    )
  )
}
