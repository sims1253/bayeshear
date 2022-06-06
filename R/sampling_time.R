#' Title
#'
#' @param fit brmsfit object
#'
#' @return
#' @export
#'
#' @examples
sampling_time <- function(fit) {
  times <- rstan::get_elapsed_time(fit$fit)
  return(
    list(
      "warmup_time" = sum(times[, 1]),
      "sampling_time" = sum(times[, 2])
    )
  )
}
