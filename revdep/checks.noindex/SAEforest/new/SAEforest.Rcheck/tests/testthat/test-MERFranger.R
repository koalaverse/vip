# Test if MERFranger and most important errors work as expected

test_that("MERFranger and prediction generics work?", {
  data("eusilcA_pop")
  data("eusilcA_smp")

  income <- eusilcA_smp$eqIncome
  X_covar <- eusilcA_smp[, -c(1, 16, 17, 18)]

  model1 <- MERFranger(
    Y = income, X = X_covar, random = "(1|district)",
    data = eusilcA_smp, num.trees = 50
  )

  # get individual predictions:
  ind_pred <- predict(model1, eusilcA_pop)
  ind_pred_no <- predict(model1$Forest, eusilcA_pop)$predictions + predict(model1$EffectModel,
    eusilcA_pop,
    allow.new.levels = TRUE
  )

  expect_s3_class(model1, "MERFranger")
  expect_equal(ind_pred, ind_pred_no)
})


test_that("MERFranger produces informative error messages?", {
  data("eusilcA_pop")
  data("eusilcA_smp")

  income <- 0
  X_covar <- eusilcA_smp[, -c(1, 16, 17, 18)]

  expect_error(MERFranger(
    Y = income, X = X_covar, random = "(1|district)",
    data = eusilcA_smp, num.trees = 50
  ))

  income <- eusilcA_smp$eqIncome[1:100]
  X_covar <- eusilcA_smp[, -c(1, 16, 17, 18)]

  expect_error(MERFranger(
    Y = income, X = X_covar, random = "(1|district)",
    data = eusilcA_smp, num.trees = 50
  ))
})
