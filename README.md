# bayeshear

bayeshear is an R package of metrics for models fitted with brms and evaluated with loo. It was written in 2022 and 2023 for the simulation package [bayesim](https://github.com/sims1253/bayesim).

## Status

bayeshear is retired. Nobody maintains it, and it will get no fixes. The repository stays online so that people can reproduce old results.

## Wrong results

`rmse_loo()`, `rmse_newdata()`, `r2_loo()`, `r2_newdata()` and `r2()` return wrong values in every version that has them, from the first commit in June 2022 to 0.3.0.9000. If you published numbers from these functions, recompute them with the replacements below.

The numbers in this section come from one reproduction: 1,000 observations from a standard normal distribution, an intercept-only normal model (`brm(y ~ 1)`, 4 chains, 4,000 draws), and 1,000 new observations from the same distribution. The model is correct for these data. It used brms 2.23.0, loo 2.10.1 and posterior 1.7.0.

`rmse_loo()` and `rmse_newdata()` do not return a root mean squared error. For each observation they compute the RMSE of the posterior predictive draws, which include the observation noise, and then add these values over all observations. The result grows with the number of observations. Since commit 46d544a (1 December 2022), including version 0.3.0.9000, the functions return this sum. Before that commit, in version 0.1.0.9000 and the first commits of 0.2.0.9000, they returned the negative of the sum.

| Function | bayeshear | Correct value | Source of the correct value |
|---|---|---|---|
| `rmse_loo()` | 1,402 | 1.04 | `loo::loo_predictive_metric()`, see the code below the replacement table |
| `rmse_newdata()` | 1,405 | 1.04 | RMSE of the posterior mean prediction |

`rmse_newdata()` and `r2_newdata()` take the observed values from `newdata$y`, whatever the response is called. R matches `$y` partially, so without a column `y` they use the one column whose name starts with `y`, such as `yield`. If no column name starts with `y`, or more than one does, `rmse_newdata()` stops with an error and `r2_newdata()` returns 0 with a warning.

The R² functions use a wrong formula. Each observation contributes `1/N - ss_e / ss_y`, and `r2_loo()` and `r2_newdata()` add these contributions. Without weights, `ss_e` is the squared error summed over all posterior draws, so the result grows with the number of draws. With weights, `ss_e` is a weighted mean of squared errors of predictive draws that include the noise. In this example that puts the result near -1.

| Function | bayeshear | Correct value | Source of the correct value |
|---|---|---|---|
| `r2_loo()` | -1.00 | -0.002 | `brms::loo_R2()` |
| `r2_newdata()` | -7,975 | 0 | `brms::bayes_R2(newdata = ...)` |
| `sum(r2(y, yrep))` | -8,009 | | |
| `sum(r2(y, yrep, weights))`, uniform weights | -1.00 | | |

An intercept-only model explains none of the variance, so its correct R² is about 0.

The tests in `tests/testthat/test-predictive_metrics.R` expect these wrong values. For example, they expect `rmse_loo()` to return 1362.19 and `r2_newdata()` to return -1949.87.

## Other known defects

- `bad_pareto_ks()` uses a default threshold of 0.7. loo uses `min(1 - 1/log10(S), 0.7)`, where S is the number of draws. With fewer than about 2,150 draws the loo threshold is lower (0.667 at 1,000 draws). `bad_pareto_ks()` then can count too few bad values, because it misses every k between the loo threshold and 0.7.
- `custom_loo_object()` labels any criterion as `elpd_loo`, so that `loo::loo_compare()` accepts it. `loo_compare()` treats the largest value as best. For an error such as RMSE it therefore puts the model with the largest error first. With loo 2.10.1, `custom_loo_compare()` can keep the input order and return `NA` for every difference and standard error, or assign the differences to the wrong models, as it does for models named `"1"` and `"2"`.
- The test suite fails with brms 2.23.0, loo 2.10.1 and posterior 1.7.0: `[ FAIL 8 | WARN 0 | SKIP 0 | PASS 37 ]`. Two failures call `variable_percentile()`, which no longer exists; the function is now `variable_true_percentile()`. The other six compare against stored numbers that these versions no longer produce.
- `R CMD check` stops at the dependency step when caret and randomForest are not installed. They are suggested packages, and the package code does not use them.
- `divergent_transitions()` calls `bayesplot::nuts_params()`, but `DESCRIPTION` does not declare bayesplot. It works only because brms imports bayesplot.
- `rmse_loo()`, `r2_loo()`, `bad_pareto_ks()` and `pareto_k_values()` call the internal function `brms:::.psis()`. A brms release can change or remove it without notice.

## What to use instead

| bayeshear function | Replacement |
|---|---|
| `elpd_loo()` | `brms::loo(fit)` |
| `elpd_newdata()` | `loo::elpd(brms::log_lik(fit, newdata = newdata))` |
| `rmse_loo()` | `loo::loo_predictive_metric()`, see the code below |
| `rmse_newdata()` | `sqrt(mean((brms::get_y(fit, newdata = newdata) - colMeans(brms::posterior_epred(fit, newdata = newdata)))^2))` |
| `rmse()` | `sqrt(colMeans(sweep(yrep, 2, y)^2))`, or with weights `sqrt(colSums(weights * sweep(yrep, 2, y)^2) / colSums(weights))` |
| `r2_loo()` | `brms::loo_R2(fit)` |
| `r2_newdata()` | `brms::bayes_R2(fit, newdata = newdata)` |
| `r2()` | `brms::loo_R2(fit)`, or `brms::bayes_R2(mu, y = y)` for a matrix `mu` of expected-value draws |
| `custom_loo_object()` | none |
| `custom_loo_compare()` | `loo::loo_compare(brms::loo(fit), brms::loo(fit2))` for elpd; none for other criteria |
| `bad_pareto_ks()` | `length(loo::pareto_k_ids(brms::loo(fit)))` |
| `pareto_k_values()` | `loo::pareto_k_values(brms::loo(fit))` |
| `divergent_transitions()` | `mean(brms::nuts_params(fit, pars = "divergent__")$Value)`; for `absolute = TRUE`, `sum(brms::nuts_params(fit, pars = "divergent__")$Value)` |
| `sampling_time()` | see the code below |
| `post_warmup_samples()` | `brms::ndraws(fit)` |
| `get_y()` | `brms::get_y(fit)` |
| `posterior_quantiles()` | `posterior::summarise_draws(fit, ~quantile(.x, probs = probs))` |
| `variable_summary()` | `posterior::summarise_draws(fit, fun)` |
| `variable_distance()` | `fun(posterior::extract_variable(fit, variable), reference)`, once for each variable and its reference |
| `variable_bias()` | `mean(draws) - reference` |
| `variable_rmse()` | `sqrt(mean((draws - reference)^2))` |
| `variable_mae()` | `mean(abs(draws - reference))` |
| `variable_mse()` | `mean((draws - reference)^2)` |
| `variable_true_percentile()` | `mean(draws < reference)` |
| `variable_pos_prob()` | `mean(draws > 0)` |

In the table, `y`, `yrep`, `weights`, `draws`, `reference`, `fun`, `probs` and `variable` are the arguments of the bayeshear function, and `newdata` is a data frame of new observations. `NAMESPACE` also exports the S3 methods by their full names (the `.brmsfit`, `.stanfit`, `.psis`, `.draws` and `.default` methods). The rows are written for a `brmsfit`. Not all of them work on a raw `stanfit`; `brms::ndraws()` does not, for example. The two replacement Pareto functions, `loo::pareto_k_ids()` and `loo::pareto_k_values()`, also accept an existing PSIS or PSIS-LOO object in place of `brms::loo(fit)`.

The LOO RMSE in the first table came from this code:

```r
ll <- brms::log_lik(fit)
r_eff <- loo::relative_eff(
  exp(ll),
  chain_id = rep(seq_len(brms::nchains(fit)), each = brms::ndraws(fit) / brms::nchains(fit))
)
loo::loo_predictive_metric(brms::posterior_epred(fit), brms::get_y(fit), ll,
                           metric = "rmse", r_eff = r_eff)
```

Both RMSE replacements need a fit with one numeric response, for which `brms::posterior_epred()` returns a matrix of draws by observations. They do not work for ordinal or categorical fits. `rmse()` computed one RMSE per observation over the draws. The aggregate LOO RMSE is the code above.

`r2()` returned one term per observation, and these terms do not add up to an R². `brms::bayes_R2(mu, y = y)` returns one R² per draw.

`sampling_time()` summed the elapsed times of all chains. By default it divided them by the number of warmup and sampling iterations of all chains. For a `brmsfit` fitted with rstan:

```r
# sampling_time(fit, absolute = TRUE)
colSums(rstan::get_elapsed_time(fit$fit))
# sampling_time(fit)
colSums(rstan::get_elapsed_time(fit$fit)) /
  (c(fit$fit@sim$warmup, fit$fit@sim$iter - fit$fit@sim$warmup) * fit$fit@sim$chains)
```

`loo::pareto_k_ids()` and `loo::pareto_k_table()` use the sample-size-dependent threshold. `posterior::pareto_khat_threshold()` returns `1 - 1/log10(S)` without the cap at 0.7, so it gives a higher threshold than loo when S is above about 2,150.

## Installing the last version

To reproduce old work, install the last commit that changes package code. It installs version 0.3.0.9000:

```r
remotes::install_github("sims1253/bayeshear@ae5cae537c154a8ccc0ff50a7299a331b4e2fac2")
```

## License

`DESCRIPTION` gives the license as `GPL-3 + file LICENSE`. The `LICENSE` file contains the text of the GNU General Public License, version 3.
