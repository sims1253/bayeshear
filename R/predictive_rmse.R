#' Calculate the root-mean-squared-error for given y and yrep.
#'
#' If psis-weights are supplied, the corresponding psis-rmse is returned.
#'
#' @param y Vector of observed values
#' @param yrep Vector of predicted Values
#' @param weights PSIS weights
#'
#' @return rmse for the given y and yrep vectors
#' @export
#'
#' @examples
rmse <- function(y, yrep, weights = NULL) {
  y_matrix <- matrix(
    y,
    nrow <- nrow(yrep),
    ncol <- ncol(yrep),
    byrow <- TRUE
  )
  if (is.null(weights)) {
    return(sqrt(colMeans(((y_matrix - yrep)^2
    ))))
  } else {
    return(sqrt(colSums(weights * ((y_matrix - yrep)^2
    )) /
      colSums(weights)))
  }
}

#' Title
#'
#' @param y
#' @param yrep
#'
#' @return
#' @export
#'
#' @examples
rmse_newdata <- function(fit, newdata) {
  pointwise_rmse <- rmse(
    y = y <- newdata$y,
    yrep = brms::posterior_predict(fit, newdata = newdata)
  )

  loo_object <- custom_loo_object(
    pointwise_criterion = -pointwise_rmse
  )
  return(
    list(
      "rmse_newdata" = loo_object$estimates[1, 1],
      "se_rmse_newdata" = loo_object$estimates[1, 2],
      "object" = loo_object
    )
  )
}

#' Calculate PSIS-loo rmse for a given brms fit
#'
#' @param ... Additional arguments to be passed to update() in case of reloo
#' @param fit
#' @param psis_object
#'
#' @return \code{custom_loo_object} object with rmse acting as elpd.
#' @export
#'
#' @examples
rmse_loo <- function(fit,
                     psis_object = NULL,
                     ...) {
  if (is.null(psis_object)) {
    psis_object <- brms:::.psis(fit, newdata = fit$data, resp = NULL)
  }
  pointwise_rmse <- rmse(
    y = brms::get_y(fit),
    yrep = posterior_predict(fit, fit$data),
    weights = exp(psis_object$log_weights)
  )

  loo_object <- custom_loo_object(
    pointwise_criterion = -pointwise_rmse,
    psis_object = psis_object
  )
  return(
    list(
      "rmse_loo" = loo_object$estimates[1, 1],
      "se_rmse_loo" = loo_object$estimates[1, 2],
      "object" = loo_object
    )
  )
}
