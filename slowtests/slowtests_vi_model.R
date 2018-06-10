# These tests are a work in progress!

# Load required packages
library(caret)
# library(h2o)
library(vip)

# Methods to use with caret
regression_methods <- c(
  # "C5.0" = "C5.0",
  # "constparty",
  # "default",
  "earth" = "earth",
  "gbm" = "gbm",
  # "h2o" = "gbm_h2o",
  "lm" = "lm",
  "randomForest" = "rf",
  "party" = "cforest",
  "ranger" = "ranger",
  "rpart" = "rpart",
  "xgboost" = "xgbTree"
)

# Load Boston housing data
data(boston, package = "pdp")

# Fit models using caret
# h2o.init()
set.seed(101)
fitted_models <- plyr::llply(regression_methods, .fun = function(xx) {
  # message("Fitting model: ", xx)
  if (xx == "ranger") {
    train(
      x = data.matrix(subset(boston, select = -cmedv)),
      y = boston$cmedv,
      method = xx,
      importance = "impurity",  # ranger only
      trControl = trainControl(method = "cv", number = 5),
      tuneLength = 5
    )
  } else {
    train(
      x = data.matrix(subset(boston, select = -cmedv)),
      y = boston$cmedv,
      method = xx,
      trControl = trainControl(method = "cv", number = 5),
      tuneLength = 5
    )
  }
}, .progress = "text")

# Helper function
vi_check <- function(object) {
  fm <- object$finalModel
  x <- vi(fm, sort = FALSE)
  y <- vi_model(fm)
  identical(x, y)
}

# Checks
sapply(fitted_models, FUN = vi_check)  # party is random; hence, will be FALSE!
