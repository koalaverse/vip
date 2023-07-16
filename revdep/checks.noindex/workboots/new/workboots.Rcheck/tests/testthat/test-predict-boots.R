# read in data to use in tests
# test_wf: wf using xgboost to predict body_mass_g from all predictors in the
#          palmer penguins dataset. one recipe step - step_dummy(all_nominal())
# test_train: training df of palmer penguins
# test_test: testing df of palmer penguins
test_wf <- readRDS("data/test_wf.rds")
test_train <- read.csv("data/test_train.csv")
test_test <- read.csv("data/test_test.csv")

test_that("predict_boots() returns prediction interval in expected format", {

  # generate predictions
  expect_warning(
    x <-
      predict_boots(
        workflow = test_wf,
        n = 5,
        training_data = test_train,
        new_data = test_test
      ),

    "At least 2000 resamples recommended for stable results."
  )

  # tests
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_named(x, c("rowid", ".preds"))
  expect_named(x$.preds[[1]], c("model", "model.pred"))
  expect_type(x$rowid, "integer")
  expect_type(x$.preds, "list")
  expect_type(x$.preds[[1]]$model, "character")
  expect_type(x$.preds[[1]]$model.pred, "double")
  expect_equal(nrow(x), nrow(test_test))

})

test_that("predict_boots() returns confidence interval in expected format", {

  # generate predictions
  expect_warning(
    x <-
      predict_boots(
        workflow = test_wf,
        n = 5,
        training_data = test_train,
        new_data = test_test,
        interval = "confidence"
      ),

    "At least 2000 resamples recommended for stable results."
  )

  # tests
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_named(x, c("rowid", ".preds"))
  expect_named(x$.preds[[1]], c("model", "model.pred"))
  expect_type(x$rowid, "integer")
  expect_type(x$.preds, "list")
  expect_type(x$.preds[[1]]$model, "character")
  expect_type(x$.preds[[1]]$model.pred, "double")
  expect_equal(nrow(x), nrow(test_test))

})

test_that("predict_boots() throws an error when not passed a workflow", {

  expect_error(
    predict_boots(
      workflow = test_train,
      n = 1,
      training_data = test_train,
      new_data = test_test
    ),

    "argument `workflow` must be of class \"workflow\"."
  )

})

test_that("predict_boots() throws an error when workflow is not final", {

  # load bad wf - same as test_wf but has 1 non-final tuning param
  test_wf_bad <- readRDS("data/test_wf_bad.rds")

  expect_error(
    predict_boots(
      workflow = test_wf_bad,
      n = 1,
      training_data = test_train,
      new_data = test_test
    ),

    "all tuning parameters must be final."
  )

})

test_that("predict_boots() throws an error when bad n is specified", {

  expect_error(
    predict_boots(
      workflow = test_wf,
      n = 0,
      training_data = test_train,
      new_data = test_test
    ),

    "argument `n` must be >= 1."
  )

  expect_error(
    predict_boots(
      workflow = test_wf,
      n = 1.5,
      training_data = test_train,
      new_data = test_test
    ),

    "argmuent `n` must be an integer."
  )

})

test_that("predict_boots() throws an error when training_data/new_data doesn't match expected format", {

  # predictors & outcome missing from training_data
  expect_error(
    predict_boots(
      workflow = test_wf,
      n = 1,
      training_data = test_train[, 3],
      new_data = test_test
    ),

    paste0("missing cols in training_data:\n",
           "species, island, bill_length_mm, bill_depth_mm, flipper_length_mm, sex, body_mass_g")
  )

  # predictors missing from new_data
  expect_error(
    predict_boots(
      workflow = test_wf,
      n = 1,
      training_data = test_train,
      new_data = test_test[, 3]
    ),

    paste0("missing cols in new_data:\n",
           "species, island, bill_length_mm, bill_depth_mm, flipper_length_mm, sex")
  )

})

