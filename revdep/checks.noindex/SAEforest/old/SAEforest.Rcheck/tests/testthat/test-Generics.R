# Test if generic functions work as expected

test_that("Do generics for SAEforest_model work correctly?", {
  data("eusilcA_pop")
  data("eusilcA_smp")

  income <- eusilcA_smp$eqIncome
  X_covar <- eusilcA_smp[, -c(1, 16, 17, 18)]

  model1 <- SAEforest_model(
    Y = income, X = X_covar, dName = "district",
    smp_data = eusilcA_smp, pop_data = eusilcA_pop,
    num.trees = 50, mtry = 3
  )

  expect_equal(sigma(model1), model1$MERFmodel$ErrorSD)
  expect_equal(residuals(model1), model1$MERFmodel$OOBresiduals)
  expect_equal(ranef(model1), model1$MERFmodel$RandomEffects)
  expect_equal(fixef(model1), model1$MERFmodel$Forest)
  expect_equal(getData(model1), eusilcA_smp)
  expect_equal(VarCorr(model1), model1$MERFmodel$VarianceCovariance)
})
