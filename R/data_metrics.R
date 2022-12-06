#' Rhat convergence diagnostic
#'
#' @param x Any non-supported fit object.
#' @param ... Further arguments passed to standata.
#'
#' @return Warning that the fit model type is not supported.
#' @method get_y default
#' @importFrom brms get_y
#' @export get_y
#' @export
#'
#' @examples
#' testthat::expect_error(get_y(c(1, 2, 3)))
get_y.default <- function(x, ...) {
  stop(paste("get_y is not supported for class", class(x)))
}
