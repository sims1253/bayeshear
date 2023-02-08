#' Builds a loo object that contains any pointwise criterion, acting as elpd
#' for compatibility.
#'
#' LOO currently has hardcoded expectations of elpd as part of loo objects
#' so to use loo objects, we have to disguise other criteria as elpd.
#'
#' @param pointwise_criterion vector of criterion values for each observation
#' @param psis_object \code{brms:::.psis} object for psis diagnostics
#'
#' @return a loo object, containing a criterion, disguised as elpd
#' @export
#'
#' @examples
custom_loo_object <- function(pointwise_criterion,
                              psis_object = NULL) {
  loo_object <- list()
  criterion <- sum(pointwise_criterion)
  se_criterion <- sqrt(length(pointwise_criterion) * var(pointwise_criterion))
  loo_object$estimates <- matrix(c(criterion, se_criterion), nrow = 1)
  loo_object$pointwise <- as.matrix(pointwise_criterion)
  loo_object$elpd_loo <- criterion

  row.names(loo_object$estimates) <- c("elpd_loo")
  colnames(loo_object$estimates) <- c("Estimate", "SE")
  colnames(loo_object$pointwise) <- c("elpd_loo")

  if (!is.null(psis_object)) {
    loo_object$diagnostics <- psis_object$diagnostics
  }

  attr(loo_object, "model_name") <- NULL
  attr(loo_object, "dims") <- dim(psis_object)
  attr(loo_object, "class") <-
    c("psis_loo", "importance_sampling_loo", "loo")

  return(loo_object)
}

#' Title
#'
#' @param loo_object_matrix
#' @param predictive_metrics
#'
#' @return
#' @export
#'
#' @examples
custom_loo_compare <- function(loo_object_matrix, predictive_metrics) {
  final_result <- data.frame(
    matrix(
      nrow = length(loo_object_matrix),
      ncol = 2 * length(predictive_metrics)
    )
  )
  colnames(final_result) <- unlist(lapply(predictive_metrics, function(x) {
    c(paste0(x, "_delta"), paste0(x, "_se_delta"))
  }))
  index <- names(loo_object_matrix)
  rownames(final_result) <- index

  for (i in seq_along(predictive_metrics)) {
    metric <- predictive_metrics[[i]]

    loo_result <- loo::loo_compare(lapply(loo_object_matrix, function(x) {
      x[[i]]
    }))
    deltas <- numeric(length = length(index))
    errors <- numeric(length = length(index))
    for (i in seq_along(index)) {
      deltas[[i]] <- loo_result[index[[i]], "elpd_diff"]
      errors[[i]] <- loo_result[index[[i]], "se_diff"]
    }
    final_result[paste0(metric, "_delta")] <- deltas
    final_result[paste0(metric, "_se_delta")] <- errors
  }
  return(final_result)
}
