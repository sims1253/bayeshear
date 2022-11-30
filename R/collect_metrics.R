#' Collect multiple metrics at once
#'
#' A convenience function that collects all given metrics at once. This
#' can save time compared to manually calling all metric functions
#' individually, as some variables can be reused instead of being calculated
#' multiple times.
#'
#' @param x A brmsfit object, or posterior draws (not currently supported)
#' @param metrics A vector of metric identifiers. See details for supported
#' identifiers.
#'
#' @return A named list containing all requested metrics.
#'
#' @details Currently, the following identifiers are supported. See linked
#'  functions for required additional arguments:
#' \itemize{
#' \item "bias": \code{\link{posterior_bias}}
#' \item "divergents": \code{\link{divergents}}
#' \item "ess": \code{\link{ess}}
#' \item "elpd_loo": \code{\link{elpd_loo}}
#' \item "elpd_newdata": \code{\link{elpd_newdata}}
#' \item "epred": \code{\link{epred}}
#' \item "mae_s": \code{\link{}}
#' \item "p_mean": \code{\link{p_mean}}
#' \item "p_sd": \code{\link{p_sd}}
#' \item "pareto_k": \code{\link{}}
#' \item "pos_prob": \code{\link{}}
#' \item "ppred": \code{\link{}}
#' \item "pq": \code{\link{}}
#' \item "q_true": \code{\link{}}
#' \item "r2_loo": \code{\link{}}
#' \item "r2_newdata": \code{\link{}}
#' \item "residuals": \code{\link{}}
#' \item "rhat": \code{\link{}}
#' \item "rmse_loo": \code{\link{}}
#' \item "rmse_newdata": \code{\link{}}
#' \item "rmse_s": \code{\link{}}
#' \item "rstar": \code{\link{}}
#' \item "time_sampling": \code{\link{}}
#' \item "time_total": \code{\link{}}
#' \item "time_warmup": \code{\link{}}
#' \item "y": \code{\link{}}
#' }
#'
#' Note,that not all identifiers are supported for each input class.
#'
#' @export collect_metrics collect_metrics.brmsfit
#'
#' @examples
collect_metrics <- function(x, metrics, ...) {
  UseMethod("collect_metrics")
}

#' @export
#'
collect_metrics.brmsfit <- function(fit,
                                    numeric_metrics,
                                    predictive_metrics,
                                    testing_data,
                                    ...) {
  posterior_draws <- posterior::extract_variable_matrix(fit, variable = "b_x")

  loo_objects <- vector(mode = "list", length = length(predictive_metrics))
  predictive_results <- vector(
    mode = "list",
    length = length(predictive_metrics)
  )

  psis_object <- NULL
  for (i in seq_along(predictive_metrics)) {
    identifier <- predictive_metrics[[i]]
    result <- metric_lookup(
      identifier = identifier,
      fit = fit,
      posterior_draws = posterior_draws,
      testing_data = testing_data,
      psis_object,
      ...
    )
    if (is.null(psis_object) & "psis_object" %in% names(result$object)) {
      psis_object <- result$object$psis_object
    }
    loo_objects[[i]] <- result$object
    predictive_results[[i]] <- result[names(result) != "object"]
  }

  numeric_result_list <- vector(mode = "list", length = length(numeric_metrics))
  for (i in seq_along(numeric_metrics)) {
    identifier <- numeric_metrics[[i]]
    result <- metric_lookup(
      identifier = identifier,
      fit = fit,
      posterior_draws = posterior_draws,
      psis_object = psis_object,
      ...
    )
    if (length(result) == 1) {
      result <- list(result)
      names(result) <- identifier
      numeric_result_list[[i]] <- result
    } else {
      numeric_result_list[[i]] <- result
    }
  }

  return(
    list(
      numeric_results = unlist(
        c(numeric_result_list, predictive_results),
        recursive = FALSE
      ),
      loo_objects = loo_objects
    )
  )
}
