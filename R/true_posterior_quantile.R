#' Title
#'
#' @param posterior_draws posterior::extract_variable_matrix for relevant parameter
#' @param true_value
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
true_posterior_quantile <- function(posterior_draws, true_value, ...) {
  return(length(which(posterior_draws < true_value)) / length(posterior_draws))
}
