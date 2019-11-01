# Load required packages
library(DALEX)
library(ggplot2)
library(iml)
library(ingredients)
library(microbenchmark)
library(ranger)
library(vip)

# Friedman 1 benchmark data
set.seed(101)
friedman1 <- as.data.frame(mlbench::mlbench.friedman1(10000, sd = 0.1))

# Random forest
set.seed(102)
rfo <- ranger(y ~ ., data = friedman1)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

# Metric/loss function
mfun <- ModelMetrics::rmse

# DALEX wrapper
imp_dalex <- function() {
  explainer <- explain(rfo, data = friedman1, y = friedman1$y)
  variable_importance(explainer, loss = mfun, type = "difference",
                      n_sample = -1)
}

# iml wrapper
imp_iml <- function() {
  model <- Predictor$new(rfo, data = friedman1, y = "y",
                         predict.fun = pfun)
  FeatureImp$new(model, loss = mfun, compare = "difference", n.repetitions = 1)
}

# vip wrapper
imp_vip <- function() {
  vi_permute(rfo, train = friedman1, target = "y", metric = "rmse",
             pred_wrapper = pfun, nsim = 1)
}

# Run benchmark
set.seed(103)
mb <- microbenchmark(
  imp_dalex(),
  imp_iml(),
  imp_vip(),
  times = 100L
)
levels(mb$expr) <- c("ingredients", "iml", "vip")
mb

# # Plot results
# pdf("figures/benchmark.pdf", width = 7, height = 4.326)
# autoplot(mb)
# dev.off()
