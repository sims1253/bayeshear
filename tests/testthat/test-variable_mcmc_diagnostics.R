test_that("rhat", {
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  expect_equal(
    rhat(as_draws(fit), c("b_Intercept", "sigma")),
    list(
      b_Intercept = posterior::rhat(
        posterior::extract_variable_matrix(fit, "b_Intercept")
      ),
      sigma = posterior::rhat(
        posterior::extract_variable_matrix(fit, "sigma")
      )
    )
  )
})

test_that("ess_bulk", {
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  expect_equal(
    ess_bulk(as_draws(fit), c("b_Intercept", "sigma")),
    list(
      b_Intercept = posterior::ess_bulk(
        posterior::extract_variable_matrix(as_draws(fit), "b_Intercept")
      ),
      sigma = posterior::ess_bulk(
        posterior::extract_variable_matrix(as_draws(fit), "sigma")
      )
    )
  )

  expect_equal(
    ess_bulk(as_draws(fit), c("b_Intercept", "sigma")),
    list(
      b_Intercept = posterior::ess_bulk(
        posterior::extract_variable_matrix(fit, "b_Intercept")
      ),
      sigma = posterior::ess_bulk(
        posterior::extract_variable_matrix(fit, "sigma")
      )
    )
  )
})

test_that("ess_tail", {
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  expect_equal(
    ess_tail(as_draws(fit), c("b_Intercept", "sigma")),
    list(
      b_Intercept = posterior::ess_tail(
        posterior::extract_variable_matrix(as_draws(fit), "b_Intercept")
      ),
      sigma = posterior::ess_tail(
        posterior::extract_variable_matrix(as_draws(fit), "sigma")
      )
    )
  )

  expect_equal(
    ess_tail(as_draws(fit), c("b_Intercept", "sigma")),
    list(
      b_Intercept = posterior::ess_tail(
        posterior::extract_variable_matrix(fit, "b_Intercept")
      ),
      sigma = posterior::ess_tail(
        posterior::extract_variable_matrix(fit, "sigma")
      )
    )
  )
})
