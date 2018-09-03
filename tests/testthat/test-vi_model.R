context("Variable importance scores")

# Load required packages
data(boston, package = "pdp")

set.seed(101)

test_that("vi() works for \"ranger\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("ranger")

  for (.importance in c("none", "impurity")) {
    for (.write.forest in c(TRUE, FALSE)) {
      fit <- ranger::ranger(cmedv ~ .,
                            data = boston,
                            num.trees = 2,
                            importance = .importance,
                            write.forest = .write.forest)
      if (identical(.importance, "impurity"))
        expect_is(vi(fit), class = c("tbl_df", "tbl", "data.frame"))
      else
        expect_error(vi(fit),
                     regexp = "No variable importance found. Please use 'importance' option when growing the forest.",
                     fixed = TRUE)
    }
  }

})

test_that("vi() works for \"C50\" objects", {

  skip_if_not_installed("C50")

  fit <- C50::C5.0(Species ~ ., data = iris)
  vis <- vi(fit)

  expect_is(vis, class = c("tbl_df", "tbl", "data.frame"))

})

test_that("vi() works for \"glmnet\" objects", {

  skip_if_not_installed("glmnet")

  fit <- glmnet::glmnet(
    x = stats::model.matrix(mpg ~ ., data = mtcars)[, -1], y = mtcars$mpg,
    nlambda = 1
  )
  expect_error(vi(fit),
               regexp = "Model-based variable importance scores are currently not available for objects of class \"elnetglmnet\".",
               fixed = TRUE)

})

test_that("vi() works for \"train\" objects", {

  skip_if_not_installed("caret")

  fit <- caret::train(
    mpg ~ ., data = mtcars, method = "lm"
  )
  expect_is(vi(fit), class = c("tbl_df", "tbl", "data.frame"))

})

test_that("vi() works for \"lm\" objects", {

  fit <- stats::lm(
    mpg ~ ., data = mtcars
  )
  expect_is(vi(fit), class = c("tbl_df", "tbl", "data.frame"))

})

test_that("vi() works for \"gbm\" objects", {

  skip_if_not_installed("gbm")
  fit <- gbm::gbm(
    mpg ~ ., data = mtcars, distribution = "gaussian", n.trees = 2,
    interaction.depth = 1, n.minobsinnode = 1
  )

  expect_is(vi(fit), class = c("tbl_df", "tbl", "data.frame"))

})

test_that("vi() works for \"nnet\" objects", {

  skip_if_not_installed("nnet")
  fit <- nnet::nnet(mpg ~ ., data = mtcars, size = 1, trace = FALSE)

  expect_error(vi(fit),
               regexp = "Model-based variable importance scores are currently not available for objects of class \"nnet.formulannet\".",
               fixed = TRUE)

})

test_that("vi() works for \"earth\" objects", {

  skip_if_not_installed("earth")
  fit <- earth::earth(mpg ~ ., data = mtcars)

  expect_is(vi(fit), class = c("tbl_df", "tbl", "data.frame"))

})

test_that("vi() works for \"rpart\" objects", {

  skip_if_not_installed("rpart")
  fit <- rpart::rpart(mpg ~ ., data = mtcars)

  expect_is(vi(fit), class = c("tbl_df", "tbl", "data.frame"))

})

test_that("vi() works for \"constparty\" objects", {

  skip_if_not_installed("partykit")
  suppressWarnings(fit <- partykit::ctree(mpg ~ ., data = mtcars))

  expect_is(vi(fit), class = c("tbl_df", "tbl", "data.frame"))

})

test_that("vi() works for \"randomForest\" objects", {

  skip_if_not_installed("randomForest")
  fit <- randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 2, mtry = 1)

  expect_is(vi(fit), class = c("tbl_df", "tbl", "data.frame"))

})

test_that("vi() works for \"ppr\" objects", {

  fit <- stats::ppr(mpg ~ ., data = mtcars, nterms = 1)

  expect_error(vi(fit),
               regexp = "Model-based variable importance scores are currently not available for objects of class \"ppr.formppr\".",
               fixed = TRUE)

})

test_that("vi() works for \"nls\" objects", {

  fit <- stats::nls(
    mpg ~ SSlogis(log(wt), Asym, xmid, scal), data = mtcars
  )

  expect_error(vi(fit),
               regexp = "Model-based variable importance scores are currently not available for objects of class \"nls\".",
               fixed = TRUE)

})
