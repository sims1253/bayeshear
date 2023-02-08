test_that("variable_distance", {
  # Test that the function runs without problem
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  expect_no_error(variable_distance(fit, "b_Intercept", 0, variable_rmse))
  expect_no_error(
    variable_distance(fit, c("b_Intercept", "sigma"), c(0, 1), variable_bias)
  )
  expect_no_error(
    variable_distance(posterior::as_draws(fit), "b_Intercept", 0, variable_mse)
  )
  expect_no_error(
    variable_distance(
      posterior::as_draws(fit),
      c("b_Intercept", "sigma"),
      c(0, 1),
      variable_mae
    )
  )
  expect_no_error(
    variable_distance(
      posterior::as_draws(fit),
      "b_Intercept",
      0,
      variable_percentile
    )
  )

  # Test for value
  a <- list("1" = list(a = rep(c(-1, 0, 1), 4)))
  class(a) <- c("draws_list", "draws", "list")
  expect_equal(variable_distance(a, "a", 0, variable_bias)$a, 0)
  expect_equal(variable_distance(a, "a", 0, variable_rmse)$a, sqrt(2 / 3))
  expect_equal(variable_distance(a, "a", 0, variable_mse)$a, 2 / 3)
  expect_equal(variable_distance(a, "a", 0, variable_mae)$a, 2 / 3)
  a$`1`$a <- 1:100
  expect_equal(variable_distance(a, "a", 50, variable_percentile)$a, 0.49)
})
