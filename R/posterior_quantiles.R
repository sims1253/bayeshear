#' Title
#'
#' @param prob Quantile
#' @param posterior_draws posterior::extract_variable_matrix for relevant parameter
#'
#' @return
#' @export
#'
#' @examples
posterior_quantiles <- function(posterior_draws, prob) {
  return(unname(quantile(posterior_draws, probs = prob)))
}
