#' Title
#'
#' @param posterior_draws posterior::extract_variable_matrix for relevant parameter
#' @param true_value The true parameter value to calculate the bias against.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
posterior_sample_rmse <- function(posterior_draws, true_value, ...) {
  return(sqrt(mean((posterior_draws - true_value)^2)))
}
