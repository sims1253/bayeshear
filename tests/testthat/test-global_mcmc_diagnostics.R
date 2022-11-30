test_that("rstar_w", {
  # Test that the function runs without problem
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  set.seed(1337)
  expect_equal(rstar_w(fit), 1.4933333333333)
})

test_that("divergent_transitions", {
  # Test that the function runs without problem
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  expect_equal(divergent_transitions(fit), 0)
})

test_that("bad_pareto_ks", {
  # Test that the function runs without problem
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  expect_equal(bad_pareto_ks(fit), 0)
  expect_equal(bad_pareto_ks(brms:::.psis(fit,
                                          newdata = fit$data, resp = NULL)), 0)
})

test_that("pareto_ks", {
  # Test that the function runs without problem
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  expect_equal(pareto_ks(fit),
               pareto_ks(brms:::.psis(fit, newdata = fit$data, resp = NULL)))
})

test_that("sampling_time", {
  # Test that the function runs without problem
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  expect_equal(sampling_time(fit),
               list(warmup_time = 0.024, sampling_time = 0.062))
})
