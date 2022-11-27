test_that("sample_error", {
  # Test that the function runs without problem
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  expect_no_error(sample_error(fit, "b_Intercept", "bias", 0))
  expect_no_error(sample_error(posterior::as_draws(fit), "b_Intercept", "bias", 0))
  expect_no_error(sample_error(posterior::extract_variable_matrix(fit, "b_Intercept"), error = "bias", reference = 0))

  # Test for value
  a <- matrix(nrow = 4, ncol = 4)
  a[, ] <- 1
  expect_equal(sample_error(a, error = "bias", reference = 1), 0)
  expect_equal(sample_error(a, error = "rmse", reference = 1), 0)
  expect_equal(sample_error(a, error = "mae", reference = 1), 0)
  expect_equal(sample_error(a, error = "mse", reference = 1), 0)

  expect_equal(sample_error(a, error = "bias", reference = 4), -3)
  expect_equal(sample_error(a, error = "rmse", reference = 4), 3)
  expect_equal(sample_error(a, error = "mae", reference = 4), 3)
  expect_equal(sample_error(a, error = "mse", reference = 4), 9)

  expect_equal(sample_error(a, error = "bias", reference = -4), 5)
  expect_equal(sample_error(a, error = "rmse", reference = -4), 5)
  expect_equal(sample_error(a, error = "mae", reference = -4), 5)
  expect_equal(sample_error(a, error = "mse", reference = -4), 25)
})
