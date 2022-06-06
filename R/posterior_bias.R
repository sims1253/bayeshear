#' Title
#'
#' @param posterior_draws posterior::extract_variable_matrix for relevant parameter
#' @param true_value The true parameter value to calculate the bias against.
#'
#' @return
#' @export
#'
#' @examples
posterior_bias <- function(posterior_draws, true_value, ...) {
  return(mean(posterior_draws) - true_value)
}
