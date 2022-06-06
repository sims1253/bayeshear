#' Title
#'
#' @param fit
#'
#' @return
#' @export
#'
#' @examples
bad_pareto_ks <- function(fit, psis_object = NULL, ...) {
  if (is.null(psis_object)) {
    psis_object <- brms:::.psis(fit, newdata = fit$data, resp = NULL)
  }
  return(length(which(psis_object$diagnostics$pareto_k > 0.7)))
}
