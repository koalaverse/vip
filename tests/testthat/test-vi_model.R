context("Variable importance scores")

# Load required packages
data(boston, package = "pdp")

set.seed(101)

check_vi_model <- function(.model_function, .model_args, .package = "", .error_msg = "") {

  if (nzchar(.package))
    skip_if_not_installed(.package)

  fit <- do.call(.model_function, .model_args)

  if (! nzchar(.error_msg)) {
    expect_silent(vis <- vi(fit))
    expect_is(vis, class = c("tbl_df", "tbl", "data.frame"))
    expect_true(all(names(vis) %in% c("Variable", "Importance", "Sign")))
  } else {
    expect_error(vi(fit),
                 regexp = .error_msg,
                 fixed = TRUE)
  }

}


test_that("vi() works for \"ranger\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("ranger")

  # Cycle through variable importance types
  for (.importance in c("none", "impurity")) {
    for (.write.forest in c(TRUE, FALSE)) {
      fit <- ranger::ranger(
        formula = cmedv ~ .,
        data = boston,
        num.trees = 2,
        importance = .importance,
        write.forest = .write.forest
      )
      if (identical(.importance, "impurity")) {
        expect_is(vi(fit), class = c("tbl_df", "tbl", "data.frame"))
      }
      else {
        expect_error(
          object = vi(fit),
          regexp = "No variable importance found.",
          fixed = TRUE
        )
      }
    }
  }

})


test_that("vi() works for \"C50\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("C50")

  # Fit model
  fit <- C50::C5.0(Species ~ ., data = iris)

  # Cycle through variable importance types
  for (type in c("usage", "splits")) {

    # Compute variable importance
    res <- vi_model(fit, type = type)

    # Expectations
    expect_is(vi(fit), class = c("tbl_df", "tbl", "data.frame"))
    expect_identical(attr(res, which = "type"), expected = type)

  }

})


test_that("vi() works for \"glmnet\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("glmnet")

  # Fit model
  fit1 <- glmnet::glmnet(
    x = model.matrix(~ . - cmedv - 1, data = boston),
    y = boston$cmedv,
    nlambda = 100
  )

  # Fit model
  fit2 <- glmnet::cv.glmnet(
    x = model.matrix(~ . - cmedv - 1, data = boston),
    y = boston$cmedv,
    nfolds = 5
  )

  # Expectations
  expect_is(vi(fit1), class = c("tbl_df", "tbl", "data.frame"))
  expect_is(vi(fit2), class = c("tbl_df", "tbl", "data.frame"))
  expect_identical(attr(vi(fit1), which = "type"), expected = "|coefficient|")
  expect_identical(attr(vi(fit2), which = "type"), expected = "|coefficient|")

})


test_that("vi() works for \"train\" objects", {

  check_vi_model(.model_function = caret::train,
                 .model_args = list(mpg ~ ., data = mtcars, method = "lm"),
                 .package = "caret")

})

test_that("vi() works for \"lm\" objects", {

  check_vi_model(.model_function = stats::lm,
                 .model_args = list(mpg ~ ., data = mtcars))

})

test_that("vi() works for \"gbm\" objects", {

  check_vi_model(.model_function = gbm::gbm,
                 .model_args = list(mpg ~ ., data = mtcars, distribution = "gaussian", n.trees = 2,
                                    interaction.depth = 1, n.minobsinnode = 1),
                 .package = "gbm")

})

test_that("vi() works for \"nnet\" objects", {

  # Olden algorithm
  check_vi_model(.model_function = nnet::nnet,
                 .model_args = list(mpg ~ ., data = mtcars, size = 1, trace = FALSE),
                 .package = "nnet")

  # Garson algorithm
  check_vi_model(.model_function = nnet::nnet,
                 .model_args = list(mpg ~ ., data = mtcars, size = 1, trace = FALSE),
                 .package = "nnet")

})

test_that("vi() works for \"earth\" objects", {

  check_vi_model(.model_function = earth::earth,
                 .model_args = list(mpg ~ ., data = mtcars),
                 .package = "earth")

})

test_that("vi() works for \"rpart\" objects", {

  check_vi_model(.model_function = rpart::rpart,
                 .model_args = list(mpg ~ ., data = mtcars),
                 .package = "rpart")

})

test_that("vi() works for \"constparty\" objects", {

  skip_if_not_installed("partykit")
  suppressWarnings(fit <- partykit::ctree(mpg ~ ., data = mtcars))

  expect_is(vi(fit), class = c("tbl_df", "tbl", "data.frame"))

})

test_that("vi() works for \"randomForest\" objects", {

  check_vi_model(.model_function = randomForest::randomForest,
                 .model_args = list(mpg ~ ., data = mtcars, ntree = 2, mtry = 1),
                 .package = "randomForest")

})

test_that("vi() works for \"ppr\" objects", {

  check_vi_model(.model_function = stats::ppr,
                 .model_args = list(mpg ~ ., data = mtcars, nterms = 1),
                 .error_msg = "Model-based variable importance scores are currently not available for objects of class \"ppr.formppr\".")

})

test_that("vi() works for \"nls\" objects", {

  check_vi_model(.model_function = stats::nls,
                 .model_args = list(mpg ~ SSlogis(log(wt), Asym, xmid, scal), data = mtcars),
                 .error_msg = "Model-based variable importance scores are currently not available for objects of class \"nls\".")

})
