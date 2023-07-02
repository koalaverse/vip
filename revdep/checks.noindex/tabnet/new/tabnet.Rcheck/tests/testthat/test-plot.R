
test_that("Autoplot with unsupervised training, w and wo valid_split", {

  expect_error(
    print(autoplot(attr_pretrained)),
    regexp = NA
  )

  expect_error(
    print(autoplot(attr_pretrained_vsplit)),
    regexp = NA
  )

})

test_that("Autoplot with supervised training, w and wo valid_split", {

  expect_error(
    print(autoplot(attr_fitted)),
    regexp = NA
  )

  expect_error(
    print(autoplot(attr_fitted_vsplit)),
    regexp = NA
  )

})

test_that("Autoplot a model without checkpoint", {

  tabnet_pretrain <- tabnet_pretrain(attrix, attriy, epochs = 3)
  expect_error(
    print(autoplot(tabnet_pretrain)),
    regexp = NA
  )

  tabnet_pretrain <- tabnet_pretrain(attrix, attriy, epochs = 3, valid_split=0.3)
  expect_error(
    print(autoplot(tabnet_pretrain)),
    regexp = NA
  )

  tabnet_fit <- tabnet_fit(attrix, attriy, epochs = 3)
  expect_error(
    print(autoplot(tabnet_fit)),
    regexp = NA
  )

  tabnet_fit <- tabnet_fit(attrix, attriy, epochs = 3, valid_split=0.3)
  expect_error(
    print(autoplot(tabnet_fit)),
    regexp = NA
  )

})

test_that("Autoplot of pretrain then fit scenario", {

  tabnet_fit <- tabnet_fit(attrix, attriy, tabnet_model=attr_pretrained_vsplit, epochs = 12)

  expect_error(
    print(autoplot(tabnet_fit)),
    regexp = NA
  )

})

test_that("Autoplot of tabnet_explain works for pretrain and fitted model", {

  explain_pretrain <- tabnet_explain(attr_pretrained_vsplit, attrix)
  explain_fit <- tabnet_explain(attr_fitted_vsplit, attrix)

  expect_error(
    print(autoplot(explain_pretrain)),
    regexp = NA
  )

  expect_error(
    print(autoplot(explain_pretrain, type="steps")),
    regexp = NA
  )

  expect_error(
    print(autoplot(explain_pretrain, type="steps", quantile = 0.99)),
    regexp = NA
  )

  expect_error(
    print(autoplot(explain_fit)),
    regexp = NA
  )

  expect_error(
    print(autoplot(explain_fit, type="steps")),
    regexp = NA
  )

  expect_error(
    print(autoplot(explain_fit, type="steps", quantile = 0.99)),
    regexp = NA
  )

})
