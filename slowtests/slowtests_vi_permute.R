# These tests are a work in progress!

# Load required packages
library(randomForest)
library(vip)


# Regression -------------------------------------------------------------------

# Random forest
boston <- pdp::boston
set.seed(101)
rfo1 <- randomForest(cmedv ~ ., data = boston)

# VIPs
set.seed(102)
vips <- lapply(c("mse", "rmse", "r2"), FUN = function(m) {
  vip(rfo1, method = "permute", response_name = "cmedv", metric = m) +  # or use pred_fun = predict
    ggplot2::ggtitle(m)
})
grid.arrange(grobs = vips, ncol = 3)

# VIP based on user-supplied prediction function
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
set.seed(103)
pfun <- function(object, newdata) predict
vip(rfo1, method = "permute", response_name = "cmedv", metric = mae,
    smaller_is_better = TRUE, pred_fun = randomForest:::predict.randomForest) +
  ggplot2::ggtitle("Use-supplied metric: MAE")


# Classification (binary) -------------------------------------------------

# Random forest
pima <- na.omit(pdp::pima)
set.seed(201)
rfo2 <- randomForest(diabetes ~ ., data = pima)

# Prediction wrappers
class_probs <- function(object, newdata) {
  predict(object, newdata = newdata, type = "prob")
}
class_labels <- function(object, newdata) {
  predict(object, newdata = newdata, type = "response")
}

# VIPs
set.seed(202)
vips <- lapply(c("error", "auc", "logloss"), FUN = function(m) {
  vip(rfo2, method = "permute", response_name = "diabetes", metric = m,
      reference_class = "neg",
      pred_fun = if (m == "error") class_labels else class_probs) +
    ggplot2::ggtitle(m)
})
grid.arrange(grobs = vips, ncol = 3)


# Classification (multiclass) ---------------------------------------------

# Random forest
set.seed(301)
rfo3 <- randomForest(Species ~ ., data = iris)

# Prediction wrappers
class_probs <- function(object, newdata) {
  predict(object, newdata = newdata, type = "prob")
}
class_labels <- function(object, newdata) {
  predict(object, newdata = newdata, type = "response")
}

# VIPs
set.seed(302)
vips <- lapply(c("error", "mauc", "mlogloss"), FUN = function(m) {
  vip(rfo3, method = "permute", response_name = "Species", metric = m,
      pred_fun = if (m == "error") class_labels else class_probs) +
    ggplot2::ggtitle(m)
})
grid.arrange(grobs = vips, ncol = 3)

