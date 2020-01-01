# Load required packages
library(ranger)

# Simulate Friedman benchmark data
trn1 <- gen_friedman(500, seed = 1421)
trn2 <- gen_friedman(500, seed = 1421, n_bins = 2)
trn3 <- gen_friedman(500, seed = 1421, n_bins = 3)

# Fit a random forest
set.seed(1502)  # for reproducibility
fit1 <- ranger(y ~ ., data = trn1)
fit2 <- ranger(y ~ ., data = trn2, probability = TRUE)
fit3 <- ranger(y ~ ., data = trn3, probability = TRUE)

# Prediction wrapper
pfun <- function(object, newdata) {
  pred <- predict(object, data = newdata)$predictions
  if (NCOL(pred) > 1) {
    pred <- pred[, 1L, drop = TRUE]
  }
  pred
}

# Compute SHAP-based VI scores
set.seed(1511)  # for reproducibiliity
vis1 <- vi_shap(fit1, pred_wrapper = pfun, nsim = 10)
vis2 <- vi_shap(fit2, pred_wrapper = pfun, nsim = 10)
vis3 <- vi_shap(fit3, pred_wrapper = pfun, nsim = 10)

# Display plots in a grid
grid.arrange(vip(vis1), vip(vis2), vip(vis3), nrow = 1)

# Try using `vip::vi()` and `vip::vip()` directly
set.seed(1612)  # for reproducibility
vis <- vi(fit1, method = "shap", pred_wrapper = pfun, nsim = 10,
          .progress = "text")
p <- vip(fit1, method = "shap", pred_wrapper = pfun, nsim = 10,
         .progress = "text")
grid.arrange(vip(vis), p, nrow = 1)
