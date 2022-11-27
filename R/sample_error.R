#' Calculate an error based on posterior samples for a given parameter.
#'
#' @param x brmsfit object, posterior draws list from
#'          \code{\link[posterior]{as_draws}} or matrix from
#'          \code{\link[posterior]{extract_variable_matrix}}.
#' @param variable Name of the parameter of interest.
#' @param error Identifier of the error measure to calculate.
#' @param reference Reference value to calculate the error against.
#'
#' @return The calculated error of the posterior.
#' @export sample_error sample_error.matrix sample_error.list sample_error.brmsfit
#'
#' @examples
#' vfit <- brms::brm(y ~ 1, data = rnorm(1000))
#' sample_error(fit, "b_Intercept", "bias", 0)
#'
sample_error <- function(x, variable, error, reference) {
  UseMethod("sample_error")
}

#' @export
sample_error.matrix <- function(draw_matrix,
                                variable = NULL,
                                error,
                                reference) {
  return(
    do.call(
      error_lookup(error),
      list(draw_matrix, reference)
    )
  )
}


#' @export
sample_error.brmsfit <- function(fit, variable, error, reference) {
  return(
    do.call(
      error_lookup(error),
      list(
        posterior::extract_variable_matrix(fit, variable = variable),
        reference
      )
    )
  )
}

#' @export
sample_error.list <- function(posterior_draws, variable, error, reference) {
  return(
    do.call(
      error_lookup(error),
      list(posterior::extract_variable_matrix(posterior_draws,
        variable = variable
      ), reference)
    )
  )
}



# Internal functions
#------------------------------------------------------------------------------
error_lookup <- function(error) {
  switch(error,
    "bias" = sample_bias,
    "rmse" = sample_rmse,
    "mae" = sample_mae,
    "mse" = sample_mse
  )
}

sample_bias <- function(draws, reference) {
  return(mean(draws) - reference)
}

sample_rmse <- function(draws, reference) {
  return(sqrt(mean((draws - reference)^2)))
}

sample_mae <- function(draws, reference) {
  return(mean(abs(draws - reference)))
}

sample_mse <- function(draws, reference) {
  return(mean((draws - reference)^2))
}
