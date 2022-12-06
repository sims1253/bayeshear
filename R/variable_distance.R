#' Posterior sample bias for a single variable.
#'
#' @param draws Posterior draws for the variable of interest.
#' @param reference Reference value to calculate the bias against.
#'
#' @return Posterior sample bias for a single variable.
#' @export
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' variable_bias(posterior::extract_variable(fit, "b_Intercept"), 0)
variable_bias <- function(draws, reference) {
  return(mean(draws) - reference)
}

#' Posterior sample rmse for a single variable.
#'
#' @param draws Posterior draws for the variable of interest.
#' @param reference Reference value to calculate the bias against.
#'
#' @return Posterior sample rmse for a single variable.
#' @export
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' variable_rmse(posterior::extract_variable(fit, "b_Intercept"), 0)
variable_rmse <- function(draws, reference) {
  return(sqrt(mean((draws - reference)^2)))
}

#' Posterior sample mae for a single variable.
#'
#' @param draws Posterior draws for the variable of interest.
#' @param reference Reference value to calculate the bias against.
#'
#' @return Posterior sample mae for a single variable.
#' @export
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' variable_mae(posterior::extract_variable(fit, "b_Intercept"), 0)
variable_mae <- function(draws, reference) {
  return(mean(abs(draws - reference)))
}

#' Posterior sample mse for a single variable.
#'
#' @param draws Posterior draws for the variable of interest.
#' @param reference Reference value to calculate the bias against.
#'
#' @return Posterior sample mse for a single variable.
#' @export
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' variable_mse(posterior::extract_variable(fit, "b_Intercept"), 0)
variable_mse <- function(draws, reference) {
  return(mean((draws - reference)^2))
}

#' Percentile of the reference value in the draws.
#'
#' @param draws Posterior draws for the variable of interest.
#' @param reference Reference value to calculate the bias against.
#'
#' @return Percentile of the reference value in the draws.
#' @export
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' variable_mse(posterior::extract_variable(fit, "b_Intercept"), 0)
variable_percentile <- function(draws, reference) {
  return(length(which(draws < reference)) / length(draws))
}

#' Calculate a distance metric of the posterior for a given variable and
#' reference value.
#'
#' @param x brmsfit object or posterior draws object, eg. from
#'          \code{\link[posterior]{as_draws}}.
#' @param variables Vector of variable names of interest.
#' @param references Vector of reference values for the variables of interest.
#' @param fun Distance function, eg \code{\link{variable_rmse}}.
#'
#' @return The calculated summary of the posterior.
#' @export variable_distance variable_distance.draws variable_distance.brmsfit
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = rnorm(1000))
#' variable_distance(
#'   fit,
#'   c("b_Intercept", "sigma"),
#'   c(0, 1),
#'   fun = variable_bias
#' )
variable_distance <- function(x, variables, references, fun) {
  UseMethod("variable_distance")
}

#' @export
variable_distance.brmsfit <- function(x, variables, references, fun) {
  out <- as.list(purrr::map2_dbl(
    variables,
    references,
    get_distance,
    draws = posterior::as_draws(x),
    fun = fun
  ))
  names(out) <- variables
  return(out)
}

#' @export
variable_distance.draws <- function(x, variables, references, fun) {
  out <- as.list(purrr::map2_dbl(
    variables,
    references,
    get_distance,
    draws = x,
    fun = fun
  ))
  names(out) <- variables
  return(out)
}


# Internal
#------------------------------------------------------------------------------
get_distance <- function(variable, references, draws, fun) {
  return(
    do.call(
      fun,
      list(
        posterior::extract_variable(draws, variable),
        references
      ),
    )
  )
}
