context("get_feature_names()")

test_that("get_feature_names() works for \"C50\" objects", {
  skip_if_not_installed("C50")
  C50_model <- C50::C5.0(Species ~ ., data = iris)
  expect_equal(get_feature_names(C50_model), names(iris)[1:4])

})


test_that("get_feature_names() works for \"earth\" objects", {
  skip_if_not_installed("earth")
  earth_model <- earth::earth(mpg ~ ., data = mtcars)
  expect_setequal(get_feature_names(earth_model), names(mtcars)[-1])
})


test_that("get_feature_names() works for \"gbm\" objects", {
  skip_if_not_installed("gbm")
  gbm_model <- gbm::gbm(Species ~ ., data = iris, distribution = "multinomial")
  expect_setequal(get_feature_names(gbm_model), names(iris)[1:4])
})


# test_that("get_feature_names extracts h2o object names", {
#
#   skip_on_cran()
#   skip_on_travis()
#   skip_on_appveyor()
#
#   y <- "mpg"
#   x <- setdiff(names(mtcars), y)
#   sink("temp"); h2o::h2o.init(); sink(NULL)
#   df <- h2o::as.h2o(mtcars)
#   h2o_model <- h2o::h2o.glm(x = x, y = y, training_frame = df)
#   expect_setequal(get_feature_names(h2o_model), x)
#   h2o::h2o.shutdown(prompt = FALSE)
#
# })


test_that("get_feature_names() works for \"lm\" objects", {

  lm_model <- stats::lm(mpg ~ ., data = mtcars)
  expect_setequal(get_feature_names(lm_model), names(mtcars)[-1])

})


test_that("get_feature_names() works for \"nls\" objects", {

  nls_model <- stats::nls(mpg ~ SSlogis(log(wt), Asym, xmid, scal), data = mtcars)
  expect_setequal(get_feature_names(nls_model), c("wt", "Asym", "xmid", "scal"))

})


test_that("get_feature_names() works for \"rpart\" objects", {

  rpart_model <- rpart::rpart(Species ~ ., data = iris)
  expect_setequal(get_feature_names(rpart_model), names(iris)[1:4])

})


test_that("get_feature_names() works for \"train\" objects", {

  skip_if_not_installed("caret")
  caret_model <- caret::train(mpg ~ ., data = mtcars, method = "glm")
  expect_setequal(get_feature_names(caret_model), names(mtcars)[-1])

})


test_that("get_feature_names() works for \"glmnet\" objects", {

  skip_if_not_installed("glmnet")
  y <- mtcars$mpg
  x <- stats::model.matrix(mpg ~ ., data = mtcars)[, -1]
  glmnet_model <- glmnet::glmnet(x = x, y = y)
  expect_setequal(get_feature_names(glmnet_model), names(mtcars)[-1])

})

