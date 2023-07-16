# read in data to use in tests
# test_wf: wf using xgboost to predict body_mass_g from all predictors in the
#          palmer penguins dataset. one recipe step - step_dummy(all_nominal())
# test_train: training df of palmer penguins
# test_test: testing df of palmer penguins
test_wf <- readRDS("data/test_wf.rds")
test_train <- read.csv("data/test_train.csv")

test_that("vi_boots() returns importances in expected format", {

  # generate predictions
  expect_warning(
    x <-
      vi_boots(
        workflow = test_wf,
        n = 5,
        training_data = test_train
      ),

    "At least 2000 resamples recommended for stable results."

  )


  # tests
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_named(x, c("variable", ".importances"))
  expect_type(x$variable, "character")
  expect_type(x$.importances, "list")

})

test_that("vi_boots() throws an error when not passed a workflow", {

  expect_error(
    vi_boots(
      workflow = test_train,
      n = 1,
      training_data = test_train
    ),

    "argument `workflow` must be of class \"workflow\"."
  )

})

test_that("vi_boots() throws an error when workflow is not final", {

  # load bad wf - same as test_wf but has 1 non-final tuning param
  test_wf_bad <- readRDS("data/test_wf_bad.rds")

  expect_error(
    vi_boots(
      workflow = test_wf_bad,
      n = 1,
      training_data = test_train
    ),

    "all tuning parameters must be final."
  )

})

test_that("vi_boots() throws an error when bad n is specified", {

  expect_error(
    vi_boots(
      workflow = test_wf,
      n = 0,
      training_data = test_train
    ),

    "argument `n` must be >= 1."
  )

  expect_error(
    vi_boots(
      workflow = test_wf,
      n = 1.5,
      training_data = test_train
    ),

    "argmuent `n` must be an integer."
  )

})

test_that("vi_boots() throws an error when training_data doesn't match expected format", {

  # predictors & outcome missing from training_data
  expect_error(
    vi_boots(
      workflow = test_wf,
      n = 1,
      training_data = test_train[, 3]
    ),

    paste0("missing cols in training_data:\n",
           "species, island, bill_length_mm, bill_depth_mm, flipper_length_mm, sex, body_mass_g")
  )

})



