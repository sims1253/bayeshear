# bayeshear

bayeshear is an R package of metrics for models fitted with brms and evaluated with loo. It was written in 2023 for the simulation package [bayesim](https://github.com/sims1253/bayesim).

## Status

bayeshear is retired. Nobody maintains it, and it will get no fixes. The repository stays online so that people can reproduce old results.

## Wrong results

In every version up to and including 0.3.0.9000, `rmse_loo()`, `rmse_newdata()`, `r2_loo()`, `r2_newdata()` and `r2()` return wrong values. If you published numbers from these functions, recompute them with the replacements below.

The numbers in this section come from one reproduction: 1,000 observations from a standard normal distribution, an intercept-only normal model (`brm(y ~ 1)`, 4 chains, 4,000 draws), and 1,000 new observations from the same distribution. The model is correct for these data. It used brms 2.23.0, loo 2.10.1 and posterior 1.7.0.

`rmse_loo()` and `rmse_newdata()` return a sum, not a root mean squared error. For each observation they compute the RMSE of the posterior predictive draws, and those draws include the observation noise. Then they add these values over all observations. The result grows with the number of observations and is never close to the RMSE of the predictions.

| Function | bayeshear | Correct value | Source of the correct value |
|---|---|---|---|
| `rmse_loo()` | 1,402 | 1.04 | `loo::loo_predictive_metric()` with `posterior_epred()` |
| `rmse_newdata()` | 1,405 | 1.04 | RMSE of the posterior mean prediction |

`rmse_newdata()` also reads the response from `newdata$y`, so it stops with an error, or uses the wrong column, when the response has a different name. `r2_newdata()` also reads `newdata$y`.

The R² functions use a wrong formula. Each observation contributes `1/N - ss_e / ss_y`, and `r2_loo()` and `r2_newdata()` add these contributions. Without weights, `ss_e` is the squared error summed over all posterior draws, so the result grows with the number of draws. With weights, `ss_e` is a weighted mean of squared errors of predictive draws that include the noise, so a correct model gets a value close to -1.

| Function | bayeshear | Correct value | Source of the correct value |
|---|---|---|---|
| `r2_loo()` | -1.00 | -0.002 | `brms::loo_R2()` |
| `r2_newdata()` | -7,975 | 0 | `brms::bayes_R2(newdata = ...)` |
| `sum(r2(y, yrep))` | -8,009 | -0.002 | `brms::loo_R2()` |
| `sum(r2(y, yrep, weights))`, uniform weights | -1.00 | -0.002 | `brms::loo_R2()` |

An intercept-only model explains none of the variance, so its correct R² is about 0.

The tests in `tests/testthat/test-predictive_metrics.R` expect these wrong values. For example, they expect `rmse_loo()` to return 1362.19 and `r2_newdata()` to return -1949.87.

## Other known defects

- `bad_pareto_ks()` uses a fixed threshold of 0.7. loo uses `min(1 - 1/log10(S), 0.7)`, where S is the number of draws. With fewer than about 2,150 draws the loo threshold is lower (0.667 at 1,000 draws), so `bad_pareto_ks()` counts too few bad values.
- `custom_loo_object()` labels any criterion as `elpd_loo`, so that `loo::loo_compare()` accepts it. `loo_compare()` ranks a larger elpd as better. For an error metric such as RMSE, `custom_loo_compare()` therefore ranks the worst model first.
- The test suite fails: `[ FAIL 8 | WARN 0 | SKIP 0 | PASS 37 ]`. Two failures call `variable_percentile()`, which no longer exists; the function is now `variable_true_percentile()`. The other six compare against stored numbers that current brms and loo no longer produce.
- `R CMD check` stops at the dependency step, because it requires the suggested packages caret and randomForest. The package code does not use them.
- `divergent_transitions()` calls `bayesplot::nuts_params()`, but `DESCRIPTION` does not declare bayesplot.
- `rmse_loo()`, `r2_loo()`, `bad_pareto_ks()` and `pareto_k_values()` call the internal function `brms:::.psis()`. A brms release can change or remove it without notice.

## What to use instead

| bayeshear function | Replacement |
|---|---|
| `elpd_loo()` | `loo::loo()` or `brms::loo()` |
| `elpd_newdata()` | `loo::elpd(brms::log_lik(fit, newdata = newdata))` |
| `rmse_loo()` | `loo::loo_predictive_metric(brms::posterior_epred(fit), y, brms::log_lik(fit), metric = "rmse")` |
| `rmse_newdata()` | `sqrt(mean((y_new - colMeans(brms::posterior_epred(fit, newdata = newdata)))^2))` |
| `rmse()` | `loo::loo_predictive_metric()` with `metric = "rmse"`, or base R |
| `r2_loo()` | `brms::loo_R2()` |
| `r2_newdata()` | `brms::bayes_R2(fit, newdata = newdata)` |
| `r2()` | `brms::loo_R2()` or `brms::bayes_R2()` |
| `custom_loo_object()` | none |
| `custom_loo_compare()` | `loo::loo_compare()` for elpd; none for other criteria |
| `bad_pareto_ks()` | `length(loo::pareto_k_ids(loo::loo(fit)))`, or `loo::pareto_k_table()` |
| `pareto_k_values()` | `loo::pareto_k_values(loo::loo(fit))` |
| `divergent_transitions()` | `brms::nuts_params(fit)`, then sum the `divergent__` rows |
| `sampling_time()` | `rstan::get_elapsed_time(fit$fit)` for rstan fits |
| `post_warmup_samples()` | `posterior::ndraws()` or `brms::ndraws()` |
| `get_y()` | `brms::get_y()` |
| `posterior_quantiles()` | `posterior::summarise_draws(x, ~quantile(.x, probs = probs))` |
| `variable_summary()` | `posterior::summarise_draws()` |
| `variable_distance()` | `posterior::extract_variable()` and base R |
| `variable_bias()` | `mean(draws) - reference` |
| `variable_rmse()` | `sqrt(mean((draws - reference)^2))` |
| `variable_mae()` | `mean(abs(draws - reference))` |
| `variable_mse()` | `mean((draws - reference)^2)` |
| `variable_true_percentile()` | `mean(draws < reference)` |
| `variable_pos_prob()` | `mean(draws > 0)` |

`loo::loo_predictive_metric()` also computes MAE and MSE (`metric = "mae"` or `"mse"`). For convergence diagnostics, use `brms::rhat()` or `posterior::summarise_draws()`.

`loo::pareto_k_ids()` and `loo::pareto_k_table()` use the sample-size-dependent threshold. `posterior::pareto_khat_threshold()` returns `1 - 1/log10(S)` without the cap at 0.7, so it gives a higher threshold than loo when S is above about 2,150.

## Installing the last version

To reproduce old work, install the last commit that changes package code:

```r
remotes::install_github("sims1253/bayeshear@ae5cae537c154a8ccc0ff50a7299a331b4e2fac2")
```

The package also needs bayesplot for `divergent_transitions()`. Install it separately.

## License

`DESCRIPTION` gives the license as `GPL-3 + file LICENSE`. The `LICENSE` file contains the text of the GNU General Public License, version 3.
