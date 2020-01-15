# Load required packages
library(DALEX)
library(ggplot2)
library(iml)
library(ingredients)
library(microbenchmark)
library(mmpf)
library(ranger)
library(vip)

# Friedman 1 benchmark data
set.seed(101)
friedman1 <- as.data.frame(mlbench::mlbench.friedman1(10000, sd = 0.1))
X <- subset(friedman1, select = -y)

# Random forest
set.seed(102)
rfo <- ranger(y ~ ., data = friedman1)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

# Metric/loss function
mfun <- metric_rmse
mfun2 <- function(x, y) metric_rmse(x, y)

# mmpf
imp_mmpf <- function() {
  sapply(names(X), FUN = function(x) {
    permutationImportance(friedman1, vars = x, y = "y", model = rfo, nperm = 1,
                          predict.fun = pfun, loss.fun = mfun2)
  })
}

# DALEX/ingredients wrapper
imp_ingredients <- function() {
  explainer <- explain(rfo, data = X, y = friedman1$y, verbose = FALSE)
  feature_importance(explainer, loss = mfun, type = "difference",
                     n_sample = NULL, B = 1)
}

# iml wrapper
imp_iml <- function() {
  model <- Predictor$new(rfo, data = friedman1, y = "y",
                         predict.fun = pfun)
  FeatureImp$new(model, loss = mfun, compare = "difference", n.repetitions = 1)
}

# vip wrapper
imp_vip <- function() {
  vi_permute(rfo, train = X, target = friedman1$y, metric = "rmse",
             pred_wrapper = pfun, nsim = 1)
}

# Run benchmark
set.seed(103)
mb <- microbenchmark(
  imp_ingredients(),
  imp_iml(),
  imp_vip(),
  imp_mmpf(),
  times = 100L
)
levels(mb$expr) <- c("ingredients", "iml", "vip", "mmpf")
mb
saveRDS(mb, file = "rjournal/benchmark-mmpf.rds")

# # Plot results
# pdf("figures/benchmark.pdf", width = 7, height = 4.326)
# autoplot(mb)
# dev.off()
