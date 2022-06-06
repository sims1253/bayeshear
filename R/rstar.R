#' Title
#'
#' @param posterior_draws posterior::as_draws_array of a fit object
#'
#' @return
#' @export
#'
#' @examples
p_rstar <- function(posterior_draws) {
  posterior::rstar(posterior_draws)
}
