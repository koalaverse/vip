context("Abbreviate feature names")

# Load required packages
data(boston, package = "pdp")

test_that("abbreviate_names() works for \"ranger\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("ranger")

  # Fitted model
  set.seed(101)
  fit <- ranger::ranger(cmedv ~ ., data = boston, importance = "impurity")
  length <- 2
  vis <- vi(fit, abbreviate_feature_names = length)

  # abbreviations
  original_names <- abbreviate(names(subset(boston, select = -cmedv)), minlength = length)
  vi_names <- vis$Variable

  # Expectations
  expect_setequal(vi_names, original_names)

})
