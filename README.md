# Bayeshear

A collection of Bayesian model metrics initially built for [Bayesim](https://github.com/sims1253/bayesim) and exported to their own package for convenience.

While most of the containing functions are essentially just convenient wrappers, Bayeshear does have experimental support of rmse and R² for the `{loo}` package and can compare models via `loo_compare` using those metrics.
