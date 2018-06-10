context("Variable importance")

# Load required packages
data(boston, package = "pdp")



test_that("vi_model() works for \"ranger\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("ranger")

  # Fitted model
  set.seed(101)
  fit <- ranger::ranger(cmedv ~ ., data = boston, importance = "impurity")
  vis <- vi:::vi_model.ranger(fit)

  # Expectations


})
