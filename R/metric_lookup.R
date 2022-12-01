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
                                  posterior_variable_matrix = NULL,
                                  testing_data = NULL,
                                  variables = NULL,
                                  references = NULL,
                                  threshold = 0.7,
                                  ...) {
  if (is.null(posterior_draws)) {
    posterior_draws <- posterior::as_draws(fit)
  }

  if (grepl("pq_", identifier)) {
    prob <- as.numeric(substring(identifier, 4))
    return(unname(posterior_quantiles(posterior_draws, prob)))
  } else {
    switch(identifier,
      # Variable summaries
      "v_mean" = return(variable_summary(posterior_draws, variables, mean)),
      "v_sd" = return(variable_summary(posterior_draws, variables, sd)),
      "v_median" = return(variable_summary(posterior_draws, variables, median)),
      "v_mad" = return(variable_summary(posterior_draws, variables, mad)),
      "v_pos_prob" = return(
        variable_summary(posterior_draws, variables, variable_pos_prob)
      ),
      # Variable distance measures
      "v_bias" = return(
        variable_distance(
          posterior_draws,
          variables,
          references,
          variable_bias
        )
      ),
      "v_rmse" = return(
        variable_distance(posterior_draws, variables, references, variable_rmse)
      ),
      "v_mae" = return(
        variable_distance(posterior_draws, variables, references, variable_mae)
      ),
      "v_mse" = return(
        variable_distance(posterior_draws, variables, references, variable_mse)
      ),
      "v_percentile" = return(
        variable_distance(
          posterior_draws,
          variables, references,
          variable_percentile
        )
      ),
      # Global MCMC Diagnostics
      "divergent_transitions" = return(divergent_transitions(fit)),
      "rstar_w" = return(rstar_w(posterior_draws)),
      "pareto_k" = return(bad_pareto_ks(fit, threshold)),
      "time" = return(sampling_time(fit)),
      # Variable MCMC Diagnostics
      "rhat" = return(rhat(posterior_draws, variables)),
      "ess_bulk" = return(ess_bulk(posterior_draws, variables)),
      "ess_tail" = return(posterior::ess_tail(posterior_draws, variables)),

      # Predictive Metrics
      "elpd_loo" = return(elpd_loo_handler(fit)),
      "elpd_newdata" = return(elpd_newdata(fit, testing_data)),
      "rmse_loo" = return(rmse_loo(fit, ...)),
      "rmse_newdata" = return(rmse_newdata(fit, testing_data)),
      "r2_loo" = return(r2_loo(fit, ...)),
      "r2_newdata" = return(r2_newdata(fit, testing_data)),

      "residuals" = return(
        list(residuals = residuals(fit, method = "posterior_predict")[, 1])
      ),
      "ppred" = return(list(colMeans(brms::posterior_predict(fit)))),
      # Other
      "y" = return(list(get_y(fit)))
    )
  }
}
