test_that("elpd_loo", {
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  set.seed(1337)
  res <- elpd_loo(fit)
  expect_equal(res$p_loo, 2.0081953)
  expect_equal(res$se_p_loo, 0.15917164)
  expect_equal(res$elpd_loo, -1422.051824)
  expect_equal(res$se_elpd_loo, 21.36699922)
})

test_that("elpd_newdata", {
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  set.seed(1337)
  res <- elpd_newdata(fit, newdata = list(y = rnorm(1000)))
  expect_equal(res$elpd_newdata, -1448.882)
  expect_equal(res$se_elpd_newdata, 24.751864)
})

test_that("rmse_loo", {
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  set.seed(1337)
  res <- rmse_loo(fit)
  expect_equal(res$rmse_loo, 1362.19085)
  expect_equal(res$se_rmse_loo, 12.4227183)
})

test_that("rmse_newdata", {
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  set.seed(1337)
  res <- rmse_newdata(fit, newdata = list(y = rnorm(1000)))
  expect_equal(res$rmse_newdata, 1371.028)
  expect_equal(res$se_rmse_newdata, 13.6797038)
})

test_that("r2_loo", {
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  set.seed(1337)
  res <- r2_loo(fit)
  expect_equal(res$r2_loo, -1.00552276)
  expect_equal(res$se_r2_loo, 0.042770102)
})

test_that("r2_newdata", {
  fit <- readRDS("model_files/normal_identity_intercept.RDS")
  set.seed(1337)
  res <- r2_newdata(fit, newdata = list(y = rnorm(1000)))
  expect_equal(res$r2_newdata, -1949.8731)
  expect_equal(res$se_r2_newdata, 47.092241)
})
