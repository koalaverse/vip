# Test if SAEforest_model and most important errors work as expected

test_that("SAEforest_model for meanOnly and prediction generics work?", {
  data("eusilcA_pop")
  data("eusilcA_smp")

  income <- eusilcA_smp$eqIncome
  X_covar <- eusilcA_smp[, -c(1, 16, 17, 18)]

  model1 <- SAEforest_model(
    Y = income, X = X_covar, dName = "district",
    smp_data = eusilcA_smp, pop_data = eusilcA_pop,
    num.trees = 50
  )

  expect_s3_class(model1, "SAEforest_mean")
  expect_warning(summarize_indicators(model1))
})


test_that("SAEforest_model produces informative error messages?", {
  data("eusilcA_pop")
  data("eusilcA_smp")

  income <- 0
  X_covar <- eusilcA_smp[, -c(1, 16, 17, 18)]

  expect_error(SAEforest_model(
    Y = income, X = X_covar, dName = "district",
    smp_data = eusilcA_smp, pop_data = eusilcA_pop,
    num.trees = 50
  ))

  income <- eusilcA_smp$eqIncome[1:100]
  X_covar <- eusilcA_smp[, -c(1, 16, 17, 18)]

  expect_error(SAEforest_model(
    Y = income, X = X_covar, dName = "district",
    smp_data = eusilcA_smp, pop_data = eusilcA_pop,
    num.trees = 50
  ))

  income <- eusilcA_smp$eqIncome
  X_covar <- eusilcA_smp[, -c(1, 16, 18)]

  expect_error(SAEforest_model(
    Y = income, X = X_covar, dName = "district",
    smp_data = eusilcA_smp, pop_data = eusilcA_pop,
    num.trees = 50
  ))
})


test_that("SAEforest_model for nonlinear indicators works correctly?", {
  data("eusilcA_pop")
  data("eusilcA_smp")

  income <- eusilcA_smp$eqIncome
  X_covar <- eusilcA_smp[, -c(1, 16, 17, 18)]

  model3 <- SAEforest_model(
    Y = income, X = X_covar, dName = "district", smp_data = eusilcA_smp,
    pop_data = eusilcA_pop, meanOnly = FALSE, MSE = "none",
    B = 3, mtry = 5, num.trees = 50, threshold = function(Y) {
      0.5 *
        median(Y)
    }, smearing = FALSE
  )

  expect_s3_class(model3, "SAEforest")
  expect_error(summarize_indicators(model3, CV = TRUE))
})
