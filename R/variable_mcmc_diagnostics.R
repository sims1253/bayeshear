#' Rhat convergence diagnostic
#'
#' @param draws posterior draws object, eg. from
#'              \code{\link[posterior]{as_draws}}
#' @param variables Vector of variable names of interest.
#'
#' @return Named list of rhat values for each variable.
#' @method rhat draws
#' @importFrom posterior rhat
#' @export rhat
#' @export
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = rnorm(1000))
#' rhat(posterior::as_draws(fit), c("b_Intercept", "sigma"))
#'
rhat.draws <- function(draws, variables) {
  out <- lapply(variables, get_rhat, draws)
  names(out) <- variables
  return(out)
}


#' Bulk effective sample size (bulk-ESS)
#'
#' @param draws posterior draws object, eg. from
#'              \code{\link[posterior]{as_draws}}
#' @param variables Vector of variable names of interest.
#'
#' @return Named list of bulk effective sample size for each variable
#' @method ess_bulk draws
#' @importFrom posterior ess_bulk
#' @export ess_bulk
#' @export
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = rnorm(1000))
#' ess_bulk(posterior::as_draws(fit), c("b_Intercept", "sigma"))
ess_bulk.draws <- function(draws, variables) {
  out <- lapply(variables, get_ess_bulk, draws)
  names(out) <- variables
  return(out)
}

#' Bulk effective sample size (bulk-ESS)
#'
#' @param fit brmsfit object.
#' @param variables Vector of variable names of interest.
#'
#' @return Named list of bulk effective sample size for each variable
#' @method ess_bulk brmsfit
#' @importFrom posterior ess_bulk
#' @export ess_bulk
#' @export
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = rnorm(1000))
#' ess_bulk(fit, c("b_Intercept", "sigma"))
ess_bulk.brmsfit <- function(fit, variables) {
  out <- lapply(variables, get_ess_bulk, posterior::as_draws(fit))
  names(out) <- variables
  return(out)
}

#' Tail effective sample size (tail-ESS)
#'
#' @param draws posterior draws object, eg. from
#'              \code{\link[posterior]{as_draws}}
#' @param variables Vector of variable names of interest.
#'
#' @return Named list of tail effective sample size for each variable
#' @method ess_tail draws
#' @importFrom posterior ess_tail
#' @export ess_tail
#' @export
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = rnorm(1000))
#' ess_tail(posterior::as_draws(fit), c("b_Intercept", "sigma"))
ess_tail.draws <- function(draws, variables) {
  out <- lapply(variables, get_ess_tail, draws)
  names(out) <- variables
  return(out)
}

#' Tail effective sample size (tail-ESS)
#'
#' @param fit brmsfit object.
#' @param variables Vector of variable names of interest.
#'
#' @return Named list of tail effective sample size for each variable
#' @method ess_tail brmsfit
#' @importFrom posterior ess_tail
#' @export ess_tail
#' @export
#'
#' @examples
#' fit <- brms::brm(y ~ 1, data = rnorm(1000))
#' ess_tail(fit, c("b_Intercept", "sigma"))
ess_tail.brmsfit <- function(draws, variables) {
  out <- lapply(variables, get_ess_tail, posterior::as_draws(fit))
  names(out) <- variables
  return(out)
}

# Internal
#-------------------------------------------------------------------------------

get_rhat <- function(variable, draws) {
  return(posterior::rhat(posterior::extract_variable_matrix(draws, variable)))
}

get_ess_bulk <- function(variable, draws) {
  return(
    posterior::ess_bulk(posterior::extract_variable_matrix(draws, variable)))
}

get_ess_tail <- function(variable, draws) {
  return(
    posterior::ess_tail(posterior::extract_variable_matrix(draws, variable)))
}
