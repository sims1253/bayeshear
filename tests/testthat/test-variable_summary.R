test_that("variable_summary", {
  # Test that the function runs without problem
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  expect_no_error(variable_summary(fit, "b_Intercept", mean))
  expect_no_error(variable_summary(fit, c("b_Intercept", "sigma"), median))
  expect_no_error(variable_summary(posterior::as_draws(fit), "b_Intercept", sd))
  expect_no_error(variable_summary(
    posterior::as_draws(fit),
    c("b_Intercept", "sigma"),
    variable_pos_prob
  ))

  # Test for value
  a <- list("1" = list(a = rep(1, 10)))
  class(a) <- c("draws_list", "draws", "list")
  expect_equal(variable_summary(a, "a", mean)$a, 1)
  expect_equal(variable_summary(a, "a", median)$a, 1)
  expect_equal(variable_summary(a, "a", sd)$a, 0)
  expect_equal(variable_summary(a, "a", variable_pos_prob)$a, 1)
  a <- list("1" = list(a = rep(c(1, 2), 8)))
  class(a) <- c("draws_list", "draws", "list")
  a$`1`$a[1] <- 100
  expect_equal(variable_summary(a, "a", mean)$a, 7.6875)
  expect_equal(variable_summary(a, "a", median)$a, 2)
  a$`1`$a[1] <- -1
  expect_equal(variable_summary(a, "a", variable_pos_prob)$a, 15 / 16)
})
