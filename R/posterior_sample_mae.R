#' Title
#'
#' @param posterior_draws posterior::extract_variable_matrix for relevant parameter
#' @param true_value The true parameter value to calculate the mae against.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
posterior_sample_mae <- function(posterior_draws, true_value, ...) {
  return(mean(abs(posterior_draws - true_value)))
}
