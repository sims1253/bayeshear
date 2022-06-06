#' Title
#'
#' @param posterior_draws posterior::extract_variable_matrix for relevant parameter
#'
#' @return
#' @export
#'
#' @examples
posterior_sd <- function(posterior_draws) {
  return(sd(posterior_draws))
}
