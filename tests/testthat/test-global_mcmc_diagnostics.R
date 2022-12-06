test_that("divergent_transitions", {
  # Test that the function runs without problem
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  expect_equal(divergent_transitions(fit), 0)
  expect_equal(divergent_transitions(fit), divergent_transitions(fit$fit))
})

test_that("bad_pareto_ks", {
  # Test that the function runs without problem
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  expect_equal(bad_pareto_ks(fit), 0)
  expect_equal(bad_pareto_ks(brms:::.psis(
    fit,
    newdata = fit$data,
    resp = NULL
  )), 0)
})

test_that("pareto_k_values", {
  # Test that the function runs without problem
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  expect_equal(
    pareto_k_values(fit),
    pareto_k_values(brms:::.psis(fit, newdata = fit$data, resp = NULL))
  )
})

test_that("sampling_time", {
  # Test that the function runs without problem
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  expect_equal(
    sampling_time(fit, absolute = TRUE),
    list(warmup_time_abs = 0.024, sampling_time_abs = 0.062)
  )
})
