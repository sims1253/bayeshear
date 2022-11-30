#' Positive probability mass of the posterior samples for a single variable.
#'
#' @param draws Posterior draws for the variable of interest.
#'
#' @return Positive probability mass of the posterior samples for a single
#'         variable.
#' @export
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' pos_prob(posterior::extract_variable(fit, "b_Intercept"))
variable_pos_prob <- function(draws) {
  return(mean(draws > 0))
}

#' Calculate a summary statistic of the posterior for a given variable.
#'
#' @param x brmsfit object or posterior draws object, eg. from
#'          \code{\link[posterior]{as_draws}}
#' @param variables Vector of variable names of interest.
#' @param fun Summary function, eg \code{\link{mean}}.
#'
#' @return The calculated summary of the posterior.
#' @export variable_summary variable_summary.draws variable_summary.brmsfit
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = rnorm(1000))
#' variable_summary(fit, c("b_Intercept", "sigma"), fun = mean)
variable_summary <- function(x, variables, fun) {
  UseMethod("variable_summary")
}

#' @export
variable_summary.brmsfit <- function(fit, variables, fun) {
  out <- lapply(
    variables,
    get_summary,
    draws = posterior::as_draws(fit),
    fun = fun
  )
  names(out) <- variables
  return(out)
}

#' @export
variable_summary.draws <- function(draws, variables, fun) {
  out <- lapply(
    variables,
    get_summary,
    draws = draws,
    fun = fun
  )
  names(out) <- variables
  return(out)
}


# Internal
#------------------------------------------------------------------------------
get_summary <- function(variable, draws, fun) {
  return(
    do.call(
      fun,
      list(posterior::extract_variable(draws, variable)),
    )
  )
}
