#' Loo expected log pointwise predictive density and effective number of
#' parameters
#'
#' Mainly a convenient wrapper for \code{\link[loo]{loo}} to get the quantities
#' bayesim is interested in.
#'
#' @param fit A brmsfit object
#' @param ... Additional arguments for \code{\link[loo]{loo}}
#'
#' @return A named list containing elpd_loo and p_loo estimates, standard errors
#'         and pointwise estimates
#' @export elpd_loo elpd_loo.brmsfit
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' elpd_loo(fit)
elpd_loo <- function(fit, ...) {
  UseMethod("elpd_loo")
}

#' @export
elpd_loo.brmsfit <- function(fit, ...) {
  loo_object <- brms::loo(fit, ...)
  return(
    list(
      "p_loo" = loo_object$estimates[2, 1],
      "pointwise_p_loo" = loo_object$pointwise[, 3],
      "se_p_loo" = loo_object$estimates[2, 2],
      "elpd_loo" = loo_object$estimates[1, 1],
      "pointwise_elpd_loo" = loo_object$pointwise[, 1],
      "se_elpd_loo" = loo_object$estimates[1, 2],
      "pointwise_mcse_elpd_loo" = loo_object$pointwise[, 2]
    )
  )
}

#' Expected log pointwise predictive density based on new data
#'
#' @param fit a brmsfit object
#' @param newdata A data.frame containing the new dataset ob which to evaluate
#'                elpd on
#'
#' @return A named list containing the elpd estimate, standard error and
#'         pointwise estimate.
#' @export elpd_newdata elpd_newdata.brmsfit
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' elpd_newdata(fit, newdata = list(y = rnorm(1000)))
elpd_newdata <- function(fit, newdata) {
  UseMethod("elpd_newdata")
}

#' @export
elpd_newdata.brmsfit <- function(fit, newdata) {
  ll <- brms::log_lik(fit, newdata = newdata)
  pointwise_elpd <- matrixStats::colLogSumExps(ll) - log(nrow(ll))
  return(
    list(
      "elpd_newdata" = sum(pointwise_elpd),
      "pointwise_elpd_newdata" = pointwise_elpd,
      "se_elpd_newdata" = sqrt(length(pointwise_elpd) * var(pointwise_elpd))
    )
  )
}

#' PSIS-LOO rmse
#'
#' @param fit A brmsfit object
#' @param psis_object An optional psis object to prevent recalculation.
#'
#' @return Named list with rmse estimate, standard error and pointwise estimate.
#' @export rmse_loo rmse_loo.brmsfit
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' rmse_loo(fit)
rmse_loo <- function(fit,
                     psis_object = NULL) {
  UseMethod("rmse_loo")
}

#' @export
rmse_loo.brmsfit <- function(fit, psis_object = NULL) {
  if (is.null(psis_object)) {
    psis_object <- brms:::.psis(fit, newdata = fit$data, resp = NULL)
  }
  pointwise_rmse <- rmse(
    y = brms::get_y(fit),
    yrep = brms::posterior_predict(fit, fit$data),
    weights = exp(psis_object$log_weights)
  )
  return(
    list(
      "rmse_loo" = sum(pointwise_rmse),
      "pointwise_rmse_loo" = pointwise_rmse,
      "se_rmse_loo" = sqrt(length(pointwise_rmse) * var(pointwise_rmse))
    )
  )
}

#' rmse based on new data
#'
#' @param fit A brmsfit object
#' @param newdata A data.frame containing the new dataset ob which to evaluate
#'                elpd on
#'
#' @return Named list with rmse estimate, standard error and pointwise estimate.
#' @export rmse_newdata rmse_newdata.brmsfit
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' rmse_newdata(fit, newdata = list(y = rnorm(1000)))
rmse_newdata <- function(fit, newdata) {
  UseMethod("rmse_newdata")
}

#' @export
rmse_newdata.brmsfit <- function(fit, newdata) {
  pointwise_rmse <- rmse(
    y = y <- newdata$y,
    yrep = brms::posterior_predict(fit, newdata = newdata)
  )
  return(
    list(
      "rmse_newdata" = sum(pointwise_rmse),
      "pointwise_rmse_newdata" = pointwise_rmse,
      "se_rmse_newdata" = sqrt(length(pointwise_rmse) * var(pointwise_rmse))
    )
  )
}

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
#' rmse(c(1:10), c(5:15))
rmse <- function(y, yrep, weights = NULL) {
  y_matrix <- matrix(
    y,
    nrow <- nrow(yrep),
    ncol <- ncol(yrep),
    byrow <- TRUE
  )
  if (is.null(weights)) {
    return(sqrt(colMeans(((y_matrix - yrep)^2))))
  } else {
    return(sqrt(colSums(weights * ((y_matrix - yrep)^2)) / colSums(weights)))
  }
}

#' PSIS-loo R²
#'
#' @param fit A brmsfit object.
#' @param psis_object Optional psis object to prevent recalculation.
#'
#' @return Named list with R² estimate, standard error and pointwise estimate.
#' @export r2_loo r2_loo.brmsfit
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' r2_loo(fit)
r2_loo <- function(fit, psis_object = NULL) {
  UseMethod("r2_loo")
}

#' @export
r2_loo.brmsfit <- function(fit, psis_object = NULL) {
  if (is.null(psis_object)) {
    psis_object <- brms:::.psis(fit, newdata = fit$data, resp = NULL)
  }
  pointwise_r2 <- r2(
    y = get_y(fit),
    yrep = brms::posterior_predict(fit, fit$data),
    weights = exp(psis_object$log_weights)
  )

  return(
    list(
      "r2_loo" = sum(pointwise_r2),
      "pointwise_r2_loo" = pointwise_r2,
      "se_r2_loo" = sqrt(length(pointwise_r2) * var(pointwise_r2))
    )
  )
}

#' R² for new data
#'
#' @param fit A brmsfit object.
#' @param newdata A data.frame containing the new dataset ob which to evaluate
#'                elpd on
#'
#' @return Named list with R² estimate, standard error and pointwise estimate.
#' @export r2_newdata r2_newdata.brmsfit
#' @examples
#' fit <- brms::brm(y ~ 1, data = list(y = rnorm(1000)))
#' r2_newdata(fit, newdata = list(y = rnorm(1000)))
r2_newdata <- function(fit, newdata) {
  UseMethod("r2_newdata")
}

#' @export
r2_newdata.brmsfit <- function(fit, newdata) {
  pointwise_r2 <- r2(
    y = y <- newdata$y,
    yrep = brms::posterior_predict(fit, newdata = newdata)
  )

  return(
    list(
      "r2_newdata" = sum(pointwise_r2),
      "pointwise_r2_newdata" = pointwise_r2,
      "se_r2_newdata" = sqrt(length(pointwise_r2) * var(pointwise_r2))
    )
  )
}

#' Calculate R^2 for given y and yrep.
#'
#' If psis-weights are supplied, the corresponding psis-R^2 is returned.
#'
#' @param y Vector of observed values
#' @param yrep Vector of predicted Values
#' @param weights PSIS weights
#'
#' @return R^2 for the given y and yrep vectors
#' @export
#'
#' @examples
#' r2(c(1:10), c(5:14))
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
