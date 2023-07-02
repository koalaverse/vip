test_that("Supervised training can continue with a additional fit, with or wo from_epoch=", {

  fit_2 <- tabnet_fit(x, y, tabnet_model = ames_fit, epochs = 1)

  expect_equal(fit_2$fit$config$epoch, 1)
  expect_length(fit_2$fit$metrics, 6)
  expect_identical(ames_fit$fit$metrics[[1]]$train$loss, fit_2$fit$metrics[[1]]$train$loss)
  expect_identical(ames_fit$fit$metrics[[5]]$train$loss, fit_2$fit$metrics[[5]]$train$loss)

  expect_no_error(
    fit_3 <- tabnet_fit(x, y, tabnet_model = ames_fit, from_epoch = 2, epoch = 1 )
  )
  expect_equal(fit_3$fit$config$epoch, 1)
  expect_length(fit_3$fit$metrics, 3)
  expect_identical(ames_fit$fit$metrics[[1]]$train$loss, fit_2$fit$metrics[[1]]$train$loss)
  expect_identical(ames_fit$fit$metrics[[2]]$train$loss, fit_2$fit$metrics[[2]]$train$loss)

})

test_that("we can change the tabnet_options between training epoch", {

  fit_2 <- tabnet_fit(x, y, ames_fit, epochs = 1, penalty = 0.003, learn_rate = 0.002)

  expect_equal(fit_2$fit$config$epoch, 1)
  expect_length(fit_2$fit$metrics, 6)
  expect_equal(fit_2$fit$config$learn_rate, 0.002)

})

test_that("epoch counter is valid for retraining from a checkpoint", {

  tmp <- tempfile("model", fileext = "rds")
  withr::local_file(saveRDS(ames_fit, tmp))

  fit1 <- readRDS(tmp)
  fit_2 <- tabnet_fit(x, y, ames_fit, epochs = 12, verbose=T)

  expect_equal(fit_2$fit$config$epoch, 12)
  expect_length(fit_2$fit$metrics, 17)
  expect_lte(mean(fit_2$fit$metrics[[17]]$train$loss), mean(fit_2$fit$metrics[[1]]$train$loss))

})

test_that("trying to continue training with different dataset raise error", {

  pretrain_1 <- tabnet_pretrain(x, y, epochs = 1)

  expect_error(
    pretrain_2 <- tabnet_fit(attrix, y, tabnet_model=pretrain_1, epochs = 1),
    regexp = "Model dimensions"
  )

  fit_1 <- tabnet_fit(x, y, epochs = 1)

  expect_error(
    fit_2 <- tabnet_fit(attrix, y, tabnet_model=fit_1, epochs = 1),
    regexp = "Model dimensions"
  )

  expect_error(
    fit_2 <- tabnet_fit(x, attriy, tabnet_model=fit_1, epochs = 1),
    regexp = "Model dimensions"
  )

})

test_that("Supervised training can continue unsupervised training, with or wo from_epoch=", {

  expect_no_error(
    tabnet_fit(x, y, tabnet_model = ames_pretrain, epoch = 1)
  )

  expect_no_error(
    tabnet_fit(Attrition ~ ., data = attrition, tabnet_model = attr_pretrained, epochs = 1)
  )

  expect_no_error(
    tabnet_fit(x, y, tabnet_model = ames_pretrain, from_epoch = 1, epoch = 1 )
  )

})

test_that("serialization of tabnet_pretrain with saveRDS just works", {

  fit <- tabnet_fit(x, y, ames_pretrain, epoch = 1, learn_rate = 1e-12)

  tmp <- tempfile("model", fileext = "rds")
  withr::local_file(saveRDS(ames_pretrain, tmp))

  pretrain2 <- readRDS(tmp)
  fit2 <- tabnet_fit(x, y, pretrain2, epoch = 1, learn_rate = 1e-12)

  expect_equal(
    predict(fit, ames),
    predict(fit2, ames),
    tolerance = 20
  )

  expect_equal(as.numeric(fit2$fit$network$.check), 1)

})
