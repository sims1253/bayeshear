#' Title
#'
#' @return
#' @export
#'
#' @examples
r2 <- function(y, yrep, weights = NULL) {
  ss_y <- sum((y - mean(y))^2)
  pointwise_loo_r2 <- vector(mode = "numeric", length = length(y))

  if (is.null(weights)) {
    for (n in seq_along(pointwise_loo_r2)) {
      ss_e <- sum((y[n] - yrep[, n])^2)
      pointwise_loo_r2[[n]] <- 1 / length(y) - ss_e / ss_y
    }
  } else {
    for (n in seq_along(pointwise_loo_r2)) {
      ss_e <- sum((weights[, n] * (y[n] - yrep[, n])^2) / sum(weights[, n]))
      pointwise_loo_r2[[n]] <- 1 / length(y) - ss_e / ss_y
    }
  }
  return(pointwise_loo_r2)
}

#' Calculate PSIS-loo R² for a given brms fit
#'
#' @param fit brms fit to calculate rmse for
#' @param psis_object
#' @param ...
#'
#' @return \code{custom_loo_object} object with R² acting as elpd.
#'
#' @examples
r2_loo <- function(fit, psis_object = NULL, ...) {
  if (is.null(psis_object)) {
    psis_object <- brms:::.psis(fit, newdata = fit$data, resp = NULL)
  }
  pointwise_loo_r2 <- r2(
    y = get_y(fit),
    yrep = brms::posterior_predict(fit, fit$data),
    weights = exp(psis_object$log_weights)
  )

  loo_object <- custom_loo_object(
    pointwise_criterion = pointwise_loo_r2,
    psis_object = psis_object
  )
  return(
    list(
      "r2_loo" = loo_object$estimates[1, 1],
      "se_r2_loo" = loo_object$estimates[1, 2],
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
r2_newdata <- function(fit, newdata) {
  pointwise_r2 <- r2(
    y = y <- newdata$y,
    yrep = brms::posterior_predict(fit, newdata = newdata)
  )

  loo_object <- custom_loo_object(
    pointwise_criterion = pointwise_r2
  )
  return(
    list(
      "r2_newdata" = loo_object$estimates[1, 1],
      "se_r2_newdata" = loo_object$estimates[1, 2],
      "object" = loo_object
    )
  )
}
