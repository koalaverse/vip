context("get_feature_names()")


test_that("get_feature_names() works for \"formula\" objects", {

  # Specify model formulae
  form1 <- y ~ x1 + x2 + I(x2 ^ 2) + sin(x2)
  form2 <- ~ x1 + x2 + I(x2 ^ 2) + sin(x2)  # no LHS
  form3 <- terms(y ~ ., data = data.frame(y = 1:5, x1 = 1:5, x2 = 1:5))

  # Expectations
  expect_identical(
    object = vip:::get_feature_names.formula(form1),
    expected = c("x1", "x2")
  )
  expect_error(
    object = vip:::get_feature_names.formula(form2)
  )
  expect_identical(  # check dot expansion
    object = vip:::get_feature_names.formula(form3),
    expected = c("x1", "x2")
  )

})


test_that("get_feature_names() works for \"C50\" objects", {
  skip_if_not_installed("C50")
  fit <- C50::C5.0(
    Species ~ ., data = iris
  )
  expect_equal(get_feature_names(fit), names(iris)[1L:4L])

})


test_that("get_feature_names() works for \"constparty\" objects", {
  skip_if_not_installed("partykit")
  suppressWarnings(fit <- partykit::ctree(
    mpg ~ ., data = mtcars
  ))
  expect_setequal(get_feature_names(fit), names(mtcars)[-1L])
})


test_that("get_feature_names() works for \"earth\" objects", {
  skip_if_not_installed("earth")
  fit <- earth::earth(
    mpg ~ ., data = mtcars
  )
  expect_setequal(get_feature_names(fit), names(mtcars)[-1L])
})


test_that("get_feature_names() works for \"gbm\" objects", {
  skip_if_not_installed("gbm")
  fit <- gbm::gbm(
    mpg ~ ., data = mtcars, distribution = "gaussian", n.trees = 1,
    interaction.depth = 1, n.minobsinnode = 1
  )
  expect_setequal(get_feature_names(fit), names(mtcars)[-1L])
})


test_that("get_feature_names() works for \"nnet\" objects", {
  skip_if_not_installed("nnet")
  fit <- nnet::nnet(mpg ~ ., data = mtcars, size = 1, trace = FALSE)
  expect_setequal(get_feature_names(fit), names(mtcars)[-1L])

  #
  # Example based on issue #84; https://github.com/koalaverse/vip/issues/84
  #

  # Formula interface
  fit1 <- nnet::nnet(Sepal.Length ~ . + I(Petal.Width^2), size = 2, data = iris,
                     linout = TRUE)

  # Matrix interface
  mm <- model.matrix(Sepal.Length ~ . - 1, data = iris)
  fit2 <- nnet::nnet(x = mm, y = iris$Sepal.Length, size = 2, data = iris,
                     linout = TRUE)

  # Expectations
  expect_identical(
    object = vip:::get_feature_names.nnet(fit1),
    expected = setdiff(x = names(iris), y = "Sepal.Length")
  )
  expect_error(
    object = vip:::get_feature_names.nnet(fit2)
  )
})


test_that("get_feature_names() works for \"randomForest\" objects", {
  skip_if_not_installed("randomForest")
  fit <- randomForest::randomForest(
    mpg ~ ., data = mtcars, ntree = 1, mtry = 1
  )
  expect_setequal(get_feature_names(fit), names(mtcars)[-1L])
})


test_that("get_feature_names() works for \"ppr\" objects", {
  fit <- stats::ppr(mpg ~ ., data = mtcars, nterms = 1)
  expect_setequal(get_feature_names(fit), names(mtcars)[-1L])
})


test_that("get_feature_names() works for \"lm\" objects", {
  fit <- stats::lm(
    mpg ~ ., data = mtcars
  )
  expect_setequal(get_feature_names(fit), names(mtcars)[-1L])
})


test_that("get_feature_names() works for \"nls\" objects", {
  fit <- stats::nls(
    mpg ~ SSlogis(log(wt), Asym, xmid, scal), data = mtcars
  )
  expect_setequal(get_feature_names(fit), "wt")
})


test_that("get_feature_names() works for \"rpart\" objects", {
  skip_if_not_installed("rpart")
  fit <- rpart::rpart(
    mpg ~ ., data = mtcars
  )
  expect_setequal(get_feature_names(fit), names(mtcars)[-1L])
})


test_that("get_feature_names() works for \"train\" objects", {
  skip_if_not_installed("caret")
  fit <- caret::train(
    mpg ~ ., data = mtcars, method = "lm"
  )
  expect_setequal(get_feature_names(fit), names(mtcars)[-1L])
})


test_that("get_feature_names() works for \"glmnet\" objects", {
  skip_if_not_installed("glmnet")
  fit <- glmnet::glmnet(
    x = stats::model.matrix(mpg ~ ., data = mtcars)[, -1], y = mtcars$mpg,
    nlambda = 1
  )
  expect_setequal(get_feature_names(fit), names(mtcars)[-1L])
})


test_that("get_feature_names() works for \"ranger\" objects", {
  skip_if_not_installed("ranger")
  for (.write.forest in c(TRUE, FALSE)) {
    for (.importance in c("none", "impurity")) {
      if (!.write.forest && .importance == "none") next
      fit <- ranger::ranger(
        mpg ~ ., data = mtcars, num.trees = 1, importance = .importance,
        write.forest = .write.forest
      )
      expect_setequal(get_feature_names(fit), names(mtcars)[-1L])
    }
  }
  fit <- ranger::ranger(
    mpg ~ ., data = mtcars, num.trees = 1, importance = "none",
    write.forest = FALSE
  )
  expect_error(get_feature_names(ranger_model))
})
