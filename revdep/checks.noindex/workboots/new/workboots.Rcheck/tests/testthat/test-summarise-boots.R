# read in test data
test_preds <- readRDS("data/test_preds.rds")
test_importances <- readRDS("data/test_importances.rds")

test_that("summarise_predictions() returns predictions in expected format", {

  # read in data used to make predictions (new_data in predict_boots())
  test_test <- read.csv("data/test_test.csv")

  # generate summary
  x <- summarise_predictions(test_preds)

  # tests
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_named(x, c("rowid", ".preds", ".pred", ".pred_lower", ".pred_upper"))
  expect_type(x$.preds, "list")
  expect_type(x$.pred_lower, "double")
  expect_type(x$.pred, "double")
  expect_type(x$.pred_upper, "double")
  expect_type(x$.preds[[1]]$model, "character")
  expect_type(x$.preds[[1]]$model.pred, "double")
  expect_equal(nrow(x), nrow(test_test))

})

test_that("summarise_importances() returns importances in expected format", {

  # generate summary
  x <- summarise_importance(test_importances)

  # tests
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_named(x, c("variable", ".importances", ".importance", ".importance_lower", ".importance_upper"))
  expect_type(x$.importances, "list")
  expect_type(x$.importance_lower, "double")
  expect_type(x$.importance, "double")
  expect_type(x$.importance_upper, "double")
  expect_type(x$.importances[[1]]$model, "character")
  expect_type(x$.importances[[1]]$model.importance, "double")

})
