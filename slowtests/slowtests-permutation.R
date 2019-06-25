# Load required packages
library(ranger)
library(vip)

# Friedman 1 benchmark data
set.seed(101)
friedman1 <- as.data.frame(mlbench::mlbench.friedman1(1000, sd = 0.1))

# Random forest
set.seed(102)
rfo <- ranger(y ~ ., data = friedman1)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}


vip(rfo, method = "permute", target = "y", pred_wrapper = pfun, metric = "rmse")

groups <- list(
  "X1-X5" = c("x.1", "x.2", "x.3", "x.4", "x.5"),
  "X6-X10" = c("x.6", "x.7", "x.8", "x.9", "x.10")
)
vi_permute(rfo, target = "y", pred_wrapper = pfun, metric = "rmse",
    feature_names = groups)

