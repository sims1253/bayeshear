test_that("posterior_summary", {
  # Test that the function runs without problem
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  expect_no_error(posterior_summary(fit, "b_Intercept", mean))
  expect_no_error(posterior_summary(posterior::as_draws(fit), "b_Intercept", mean))
  expect_no_error(posterior_summary(posterior::extract_variable_matrix(fit, "b_Intercept"), fun = mean))

  # Test for value
  a <- matrix(nrow = 4, ncol = 4)
  a[, ] <- 1
  expect_equal(posterior_summary(a, fun = mean), 1)
  expect_equal(posterior_summary(a, fun = median), 1)
  expect_equal(posterior_summary(a, fun = sd), 0)
  a[, ] <- 1:2
  a[1, 1] <- 100
  expect_equal(posterior_summary(a, fun = mean), 7.6875)
  expect_equal(posterior_summary(a, fun = median), 2)
})
