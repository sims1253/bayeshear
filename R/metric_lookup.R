metric_lookup <- function(identifier, ...) {
  UseMethod("metric_lookup")
}

#' Access metrics with string identifiers
#'
#' This function is mostly a helper function that maps identifier strings to
#' metrics for convenient use in some contexts.
#'
#' @param identifier A string that identifies a supported function.
#'
#' @return The function corresponding to the identifier string.
#' @export
#'
metric_lookup.brmsfit <- function(identifier,
                                  fit,
                                  posterior_draws = NULL,
                                  testing_data = NULL,
                                  ...) {
  if (is.null(posterior_draws)) {
    posterior_draws <- posterior::extract_variable_matrix(fit, variable = "b_x")
  }

  if (grepl("pq_", identifier)) {
    prob <- as.numeric(substring(identifier, 4))
    return(unname(posterior_quantiles(posterior_draws, prob)))
  } else {
    switch(identifier,
      "divergents" = return(divergents(fit)),
      "rstar_w" = return(p_rstar(fit)),
      "pareto_k" = return(bad_pareto_ks(fit, ...)),
      "time" = return(sampling_time(fit)),
      "rhat" = return(posterior::rhat(posterior_draws)),
      "ess_bulk" = return(posterior::ess_bulk(posterior_draws)),
      "ess_tail" = return(posterior::ess_tail(posterior_draws)),
      "q_true" = return(q_true(posterior_draws, ...)),
      "bias" = return(p_bias(posterior_draws, ...)),
      "rmse_s" = return(rmse_s(posterior_draws, ...)),
      "mae_s" = return(mae_s(posterior_draws, ...)),
      "p_mean" = return(p_mean(posterior_draws)),
      "p_sd" = return(p_sd(posterior_draws)),
      "pos_prob" = return(pos_prob(posterior_draws)),
      "elpd_loo" = return(elpd_loo_handler(fit)),
      "elpd_newdata" = return(elpd_newdata(fit, testing_data)),
      "rmse_loo" = return(rmse_loo(fit, ...)),
      "rmse_newdata" = return(rmse_newdata(fit, testing_data)),
      "r2_loo" = return(r2_loo(fit, ...)),
      "r2_newdata" = return(r2_newdata(fit, testing_data)),
      "residuals" = return(
        list(residuals = residuals(fit, method = "posterior_predict")[, 1])
      ),
      "y" = return(list(brms::get_y(fit))),
      "ppred" = return(list(colMeans(brms::posterior_predict(fit))))
    )
  }
}
