test_that("posterior_quantiles", {
  # Test that the function runs without problem
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  expect_no_error(posterior_quantiles(fit, "b_Intercept", c(0.025, 0.5, 0.975)))
  expect_no_error(
    posterior_quantiles(
      posterior::as_draws(fit),
      "b_Intercept",
      c(0.025, 0.5, 0.975)
    )
  )

  # Test for value
  a <- list("1" = list(a = 1:100))
  class(a) <- c("draws_list", "draws", "list")
  expect_equal(unname(posterior_quantiles(a, "a", probs = c(0.5))), 50.5)
  expect_equal(
    unname(posterior_quantiles(a, "a", probs = c(0.1, 0.5, 0.9))),
    c(10.9, 50.5, 90.1)
  )
})
