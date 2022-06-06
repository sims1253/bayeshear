#' Title
#'
#' @param fit
#'
#' @return
#' @export
#'
#' @examples
elpd_loo_handler <- function(fit) {
  loo_object <- brms::loo(fit, save_psis = TRUE)
  return(
    list(
      "p_loo" = loo_object$estimates[2, 1],
      "se_p_loo" = loo_object$estimates[2, 2],
      "elpd_loo" = loo_object$estimates[1, 1],
      "se_elpd_loo" = loo_object$estimates[1, 2],
      "looic" = loo_object$estimates[3, 1],
      "se_looic" = loo_object$estimates[3, 2],
      "object" = loo_object
    )
  )
}

#' Title
#'
#' @param fit
#' @param newdata
#'
#' @return
#' @export
#'
#' @examples
elpd_newdata <- function(fit, newdata) {
  ll <- brms::log_lik(fit, newdata = newdata)
  elpd <- matrixStats::colLogSumExps(ll) - log(nrow(ll))
  loo_object <- custom_loo_object(
    pointwise_criterion = elpd
  )
  return(
    list(
      "elpd_newdata" = loo_object$estimates[1, 1],
      "se_elpd_newdata" = loo_object$estimates[1, 2],
      "object" = loo_object
    )
  )
}
