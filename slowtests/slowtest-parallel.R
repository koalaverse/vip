library(doParallel)
library(foreach)
library(ranger)
library(vip)

set.seed(1043)
trn <- gen_friedman(500, n_features = 500)
(rfo <- ranger(y ~ ., data = trn))

pfun <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

cl <- makeCluster(4) # use 5 workers
registerDoParallel(cl) # register the parallel backend

system.time({
  vis1 <- vi(rfo, method = "permute", target = "y", metric = "rmse",
             pred_wrapper = pfun, train = trn, nsim = 10)
})

system.time({
  vis2 <- vi(rfo, method = "permute", target = "y", metric = "rmse",
             pred_wrapper = pfun, train = trn, parallel = TRUE,
             nsim = 10, .packages = "ranger")
})

system.time({
  vis3 <- vi(rfo, method = "permute", target = "y", metric = "rmse",
             pred_wrapper = pfun, train = trn, parallel = TRUE,
             nsim = 10, .packages = "ranger", parallelize_by = "repetitions")
})

stopCluster(cl)
