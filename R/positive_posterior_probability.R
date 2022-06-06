#' Title
#'
#' @param posterior_draws posterior::extract_variable_matrix for relevant parameter
#'
#' @return
#' @export
#'
#' @examples
pos_prob <- function(posterior_draws) {
  return(mean(posterior_draws > 0))
}
