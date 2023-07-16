pkgname <- "radiant.model"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('radiant.model')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("auc")
### * auc

flush(stderr()); flush(stdout())

### Name: auc
### Title: Area Under the RO Curve (AUC)
### Aliases: auc

### ** Examples

auc(runif(20000), dvd$buy, "yes")
auc(ifelse(dvd$buy == "yes", 1, 0), dvd$buy, "yes")



cleanEx()
nameEx("confusion")
### * confusion

flush(stderr()); flush(stdout())

### Name: confusion
### Title: Confusion matrix
### Aliases: confusion

### ** Examples

data.frame(buy = dvd$buy, pred1 = runif(20000), pred2 = ifelse(dvd$buy == "yes", 1, 0)) %>%
  confusion(c("pred1", "pred2"), "buy") %>%
  str()



cleanEx()
nameEx("crs")
### * crs

flush(stderr()); flush(stdout())

### Name: crs
### Title: Collaborative Filtering
### Aliases: crs

### ** Examples

crs(ratings,
  id = "Users", prod = "Movies", pred = c("M6", "M7", "M8", "M9", "M10"),
  rate = "Ratings", data_filter = "training == 1"
) %>% str()



cleanEx()
nameEx("crtree")
### * crtree

flush(stderr()); flush(stdout())

### Name: crtree
### Title: Classification and regression trees based on the rpart package
### Aliases: crtree

### ** Examples

crtree(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>% summary()
result <- crtree(titanic, "survived", c("pclass", "sex")) %>% summary()
result <- crtree(diamonds, "price", c("carat", "clarity"), type = "regression") %>% str()



cleanEx()
nameEx("cv.crtree")
### * cv.crtree

flush(stderr()); flush(stdout())

### Name: cv.crtree
### Title: Cross-validation for Classification and Regression Trees
### Aliases: cv.crtree

### ** Examples

## Not run: 
##D result <- crtree(dvd, "buy", c("coupon", "purch", "last"))
##D cv.crtree(result, cp = 0.0001, pcp = seq(0, 0.01, length.out = 11))
##D cv.crtree(result, cp = 0.0001, pcp = c(0, 0.001, 0.002), fun = profit, cost = 1, margin = 5)
##D result <- crtree(diamonds, "price", c("carat", "color", "clarity"), type = "regression", cp = 0.001)
##D cv.crtree(result, cp = 0.001, pcp = seq(0, 0.01, length.out = 11), fun = MAE)
## End(Not run)




cleanEx()
nameEx("cv.gbt")
### * cv.gbt

flush(stderr()); flush(stdout())

### Name: cv.gbt
### Title: Cross-validation for Gradient Boosted Trees
### Aliases: cv.gbt

### ** Examples

## Not run: 
##D result <- gbt(dvd, "buy", c("coupon", "purch", "last"))
##D cv.gbt(result, params = list(max_depth = 1:6))
##D cv.gbt(result, params = list(max_depth = 1:6), fun = "logloss")
##D cv.gbt(
##D   result,
##D   params = list(learning_rate = seq(0.1, 1.0, 0.1)),
##D   maximize = TRUE, fun = profit, cost = 1, margin = 5
##D )
##D result <- gbt(diamonds, "price", c("carat", "color", "clarity"), type = "regression")
##D cv.gbt(result, params = list(max_depth = 1:2, min_child_weight = 1:2))
##D cv.gbt(result, params = list(learning_rate = seq(0.1, 0.5, 0.1)), fun = Rsq, maximize = TRUE)
##D cv.gbt(result, params = list(learning_rate = seq(0.1, 0.5, 0.1)), fun = MAE, maximize = FALSE)
##D rig_wrap <- function(preds, dtrain) {
##D   labels <- xgboost::getinfo(dtrain, "label")
##D   value <- rig(preds, labels, lev = 1)
##D   list(metric = "rig", value = value)
##D }
##D result <- gbt(titanic, "survived", c("pclass", "sex"), eval_metric = rig_wrap, maximize = TRUE)
##D cv.gbt(result, params = list(learning_rate = seq(0.1, 0.5, 0.1)))
## End(Not run)




cleanEx()
nameEx("cv.nn")
### * cv.nn

flush(stderr()); flush(stdout())

### Name: cv.nn
### Title: Cross-validation for a Neural Network
### Aliases: cv.nn

### ** Examples

## Not run: 
##D result <- nn(dvd, "buy", c("coupon", "purch", "last"))
##D cv.nn(result, decay = seq(0, 1, .5), size = 1:2)
##D cv.nn(result, decay = seq(0, 1, .5), size = 1:2, fun = profit, cost = 1, margin = 5)
##D result <- nn(diamonds, "price", c("carat", "color", "clarity"), type = "regression")
##D cv.nn(result, decay = seq(0, 1, .5), size = 1:2)
##D cv.nn(result, decay = seq(0, 1, .5), size = 1:2, fun = Rsq)
## End(Not run)




cleanEx()
nameEx("cv.rforest")
### * cv.rforest

flush(stderr()); flush(stdout())

### Name: cv.rforest
### Title: Cross-validation for a Random Forest
### Aliases: cv.rforest

### ** Examples

## Not run: 
##D result <- rforest(dvd, "buy", c("coupon", "purch", "last"))
##D cv.rforest(
##D   result,
##D   mtry = 1:3, min.node.size = seq(1, 10, 5),
##D   num.trees = c(100, 200), sample.fraction = 0.632
##D )
##D result <- rforest(titanic, "survived", c("pclass", "sex"), max.depth = 1)
##D cv.rforest(result, mtry = 1:3, min.node.size = seq(1, 10, 5))
##D cv.rforest(result, mtry = 1:3, num.trees = c(100, 200), fun = profit, cost = 1, margin = 5)
##D result <- rforest(diamonds, "price", c("carat", "color", "clarity"), type = "regression")
##D cv.rforest(result, mtry = 1:3, min.node.size = 1)
##D cv.rforest(result, mtry = 1:3, min.node.size = 1, fun = Rsq)
## End(Not run)




cleanEx()
nameEx("dtree")
### * dtree

flush(stderr()); flush(stdout())

### Name: dtree
### Title: Create a decision tree
### Aliases: dtree

### ** Examples

yaml::as.yaml(movie_contract) %>% cat()
dtree(movie_contract, opt = "max") %>% summary(output = TRUE)
dtree(movie_contract)$payoff
dtree(movie_contract)$prob
dtree(movie_contract)$solution_df




cleanEx()
nameEx("evalbin")
### * evalbin

flush(stderr()); flush(stdout())

### Name: evalbin
### Title: Evaluate the performance of different (binary) classification
###   models
### Aliases: evalbin

### ** Examples

data.frame(buy = dvd$buy, pred1 = runif(20000), pred2 = ifelse(dvd$buy == "yes", 1, 0)) %>%
  evalbin(c("pred1", "pred2"), "buy") %>%
  str()



cleanEx()
nameEx("evalreg")
### * evalreg

flush(stderr()); flush(stdout())

### Name: evalreg
### Title: Evaluate the performance of different regression models
### Aliases: evalreg

### ** Examples

data.frame(price = diamonds$price, pred1 = rnorm(3000), pred2 = diamonds$price) %>%
  evalreg(pred = c("pred1", "pred2"), "price") %>%
  str()




cleanEx()
nameEx("find_max")
### * find_max

flush(stderr()); flush(stdout())

### Name: find_max
### Title: Find maximum value of a vector
### Aliases: find_max

### ** Examples

find_max(1:10, 21:30)




cleanEx()
nameEx("find_min")
### * find_min

flush(stderr()); flush(stdout())

### Name: find_min
### Title: Find minimum value of a vector
### Aliases: find_min

### ** Examples

find_min(1:10, 21:30)




cleanEx()
nameEx("gbt")
### * gbt

flush(stderr()); flush(stdout())

### Name: gbt
### Title: Gradient Boosted Trees using XGBoost
### Aliases: gbt

### ** Examples

## Not run: 
##D gbt(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>% summary()
##D gbt(titanic, "survived", c("pclass", "sex")) %>% str()
## End(Not run)
gbt(titanic, "survived", c("pclass", "sex"), lev = "Yes", early_stopping_rounds = 0) %>% summary()
gbt(titanic, "survived", c("pclass", "sex"), early_stopping_rounds = 0) %>% str()
gbt(titanic, "survived", c("pclass", "sex"), eval_metric = paste0("error@", 0.5 / 6)) %>% str()
gbt(diamonds, "price", c("carat", "clarity"), type = "regression") %>% summary()
rig_wrap <- function(preds, dtrain) {
  labels <- xgboost::getinfo(dtrain, "label")
  value <- rig(preds, labels, lev = 1)
  list(metric = "rig", value = value)
}
gbt(titanic, "survived", c("pclass", "sex"), eval_metric = rig_wrap, maximize = TRUE) %>% str()



cleanEx()
nameEx("logistic")
### * logistic

flush(stderr()); flush(stdout())

### Name: logistic
### Title: Logistic regression
### Aliases: logistic

### ** Examples

logistic(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>% summary()
logistic(titanic, "survived", c("pclass", "sex")) %>% str()



cleanEx()
nameEx("mnl")
### * mnl

flush(stderr()); flush(stdout())

### Name: mnl
### Title: Multinomial logistic regression
### Aliases: mnl

### ** Examples

result <- mnl(
  ketchup,
  rvar = "choice",
  evar = c("price.heinz28", "price.heinz32", "price.heinz41", "price.hunts32"),
  lev = "heinz28"
)
str(result)




cleanEx()
nameEx("nb")
### * nb

flush(stderr()); flush(stdout())

### Name: nb
### Title: Naive Bayes using e1071::naiveBayes
### Aliases: nb

### ** Examples

nb(titanic, "survived", c("pclass", "sex", "age")) %>% summary()
nb(titanic, "survived", c("pclass", "sex", "age")) %>% str()




cleanEx()
nameEx("nn")
### * nn

flush(stderr()); flush(stdout())

### Name: nn
### Title: Neural Networks using nnet
### Aliases: nn

### ** Examples

nn(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>% summary()
nn(titanic, "survived", c("pclass", "sex")) %>% str()
nn(diamonds, "price", c("carat", "clarity"), type = "regression") %>% summary()



cleanEx()
nameEx("onehot")
### * onehot

flush(stderr()); flush(stdout())

### Name: onehot
### Title: One hot encoding of data.frames
### Aliases: onehot

### ** Examples

head(onehot(diamonds, df = TRUE))
head(onehot(diamonds, all = TRUE, df = TRUE))



cleanEx()
nameEx("plot.confusion")
### * plot.confusion

flush(stderr()); flush(stdout())

### Name: plot.confusion
### Title: Plot method for the confusion matrix
### Aliases: plot.confusion

### ** Examples

data.frame(buy = dvd$buy, pred1 = runif(20000), pred2 = ifelse(dvd$buy == "yes", 1, 0)) %>%
  confusion(c("pred1", "pred2"), "buy") %>%
  plot()



cleanEx()
nameEx("plot.crtree")
### * plot.crtree

flush(stderr()); flush(stdout())

### Name: plot.crtree
### Title: Plot method for the crtree function
### Aliases: plot.crtree

### ** Examples

result <- crtree(titanic, "survived", c("pclass", "sex"), lev = "Yes")
plot(result)
result <- crtree(diamonds, "price", c("carat", "clarity", "cut"))
plot(result, plots = "prune")
result <- crtree(dvd, "buy", c("coupon", "purch", "last"), cp = .01)
plot(result, plots = "imp")




cleanEx()
nameEx("plot.dtree")
### * plot.dtree

flush(stderr()); flush(stdout())

### Name: plot.dtree
### Title: Plot method for the dtree function
### Aliases: plot.dtree

### ** Examples

dtree(movie_contract, opt = "max") %>% plot()
dtree(movie_contract, opt = "max") %>% plot(final = TRUE, orient = "TD")




cleanEx()
nameEx("plot.evalbin")
### * plot.evalbin

flush(stderr()); flush(stdout())

### Name: plot.evalbin
### Title: Plot method for the evalbin function
### Aliases: plot.evalbin

### ** Examples

data.frame(buy = dvd$buy, pred1 = runif(20000), pred2 = ifelse(dvd$buy == "yes", 1, 0)) %>%
  evalbin(c("pred1", "pred2"), "buy") %>%
  plot()



cleanEx()
nameEx("plot.evalreg")
### * plot.evalreg

flush(stderr()); flush(stdout())

### Name: plot.evalreg
### Title: Plot method for the evalreg function
### Aliases: plot.evalreg

### ** Examples

data.frame(price = diamonds$price, pred1 = rnorm(3000), pred2 = diamonds$price) %>%
  evalreg(pred = c("pred1", "pred2"), "price") %>%
  plot()




cleanEx()
nameEx("plot.gbt")
### * plot.gbt

flush(stderr()); flush(stdout())

### Name: plot.gbt
### Title: Plot method for the gbt function
### Aliases: plot.gbt

### ** Examples

result <- gbt(titanic, "survived", c("pclass", "sex"), early_stopping_rounds = 0)
plot(result)




cleanEx()
nameEx("plot.logistic")
### * plot.logistic

flush(stderr()); flush(stdout())

### Name: plot.logistic
### Title: Plot method for the logistic function
### Aliases: plot.logistic

### ** Examples

result <- logistic(titanic, "survived", c("pclass", "sex"), lev = "Yes")
plot(result, plots = "coef")



cleanEx()
nameEx("plot.mnl")
### * plot.mnl

flush(stderr()); flush(stdout())

### Name: plot.mnl
### Title: Plot method for the mnl function
### Aliases: plot.mnl

### ** Examples

result <- mnl(
  ketchup,
  rvar = "choice",
  evar = c("price.heinz28", "price.heinz32", "price.heinz41", "price.hunts32"),
  lev = "heinz28"
)
plot(result, plots = "coef")




cleanEx()
nameEx("plot.mnl.predict")
### * plot.mnl.predict

flush(stderr()); flush(stdout())

### Name: plot.mnl.predict
### Title: Plot method for mnl.predict function
### Aliases: plot.mnl.predict

### ** Examples

result <- mnl(
  ketchup,
  rvar = "choice",
  evar = c("price.heinz28", "price.heinz32", "price.heinz41", "price.hunts32"),
  lev = "heinz28"
)
pred <- predict(result, pred_cmd = "price.heinz28 = seq(3, 5, 0.1)")
plot(pred, xvar = "price.heinz28")




cleanEx()
nameEx("plot.model.predict")
### * plot.model.predict

flush(stderr()); flush(stdout())

### Name: plot.model.predict
### Title: Plot method for model.predict functions
### Aliases: plot.model.predict

### ** Examples

regress(diamonds, "price", c("carat", "clarity")) %>%
  predict(pred_cmd = "carat = 1:10") %>%
  plot(xvar = "carat")
logistic(titanic, "survived", c("pclass", "sex", "age"), lev = "Yes") %>%
  predict(pred_cmd = c("pclass = levels(pclass)", "sex = levels(sex)", "age = 0:100")) %>%
  plot(xvar = "age", color = "sex", facet_col = "pclass")




cleanEx()
nameEx("plot.nb")
### * plot.nb

flush(stderr()); flush(stdout())

### Name: plot.nb
### Title: Plot method for the nb function
### Aliases: plot.nb

### ** Examples

result <- nb(titanic, "survived", c("pclass", "sex"))
plot(result)
result <- nb(titanic, "pclass", c("sex", "age"))
plot(result)




cleanEx()
nameEx("plot.nb.predict")
### * plot.nb.predict

flush(stderr()); flush(stdout())

### Name: plot.nb.predict
### Title: Plot method for nb.predict function
### Aliases: plot.nb.predict

### ** Examples

result <- nb(titanic, "survived", c("pclass", "sex", "age"))
pred <- predict(
  result,
  pred_cmd = c("pclass = levels(pclass)", "sex = levels(sex)", "age = seq(0, 100, 20)")
)
plot(pred, xvar = "age", facet_col = "sex", facet_row = "pclass")
pred <- predict(result, pred_data = titanic)
plot(pred, xvar = "age", facet_col = "sex")




cleanEx()
nameEx("plot.nn")
### * plot.nn

flush(stderr()); flush(stdout())

### Name: plot.nn
### Title: Plot method for the nn function
### Aliases: plot.nn

### ** Examples

result <- nn(titanic, "survived", c("pclass", "sex"), lev = "Yes")
plot(result, plots = "net")
plot(result, plots = "olden")



cleanEx()
nameEx("plot.regress")
### * plot.regress

flush(stderr()); flush(stdout())

### Name: plot.regress
### Title: Plot method for the regress function
### Aliases: plot.regress

### ** Examples

result <- regress(diamonds, "price", c("carat", "clarity"))
plot(result, plots = "coef", conf_lev = .99, intercept = TRUE)
## Not run: 
##D plot(result, plots = "dist")
##D plot(result, plots = "scatter", lines = c("line", "loess"))
##D plot(result, plots = "resid_pred", lines = "line")
##D plot(result, plots = "dashboard", lines = c("line", "loess"))
## End(Not run)



cleanEx()
nameEx("plot.rforest")
### * plot.rforest

flush(stderr()); flush(stdout())

### Name: plot.rforest
### Title: Plot method for the rforest function
### Aliases: plot.rforest

### ** Examples

result <- rforest(titanic, "survived", c("pclass", "sex"), lev = "Yes")




cleanEx()
nameEx("plot.rforest.predict")
### * plot.rforest.predict

flush(stderr()); flush(stdout())

### Name: plot.rforest.predict
### Title: Plot method for rforest.predict function
### Aliases: plot.rforest.predict

### ** Examples

result <- mnl(
  ketchup,
  rvar = "choice",
  evar = c("price.heinz28", "price.heinz32", "price.heinz41", "price.hunts32"),
  lev = "heinz28"
)
pred <- predict(result, pred_cmd = "price.heinz28 = seq(3, 5, 0.1)")
plot(pred, xvar = "price.heinz28")




cleanEx()
nameEx("plot.simulater")
### * plot.simulater

flush(stderr()); flush(stdout())

### Name: plot.simulater
### Title: Plot method for the simulater function
### Aliases: plot.simulater

### ** Examples

simdat <- simulater(
  const = "cost 3",
  norm = "demand 2000 1000",
  discrete = "price 5 8 .3 .7",
  form = "profit = demand * (price - cost)",
  seed = 1234
)
plot(simdat, bins = 25)




cleanEx()
nameEx("predict.crtree")
### * predict.crtree

flush(stderr()); flush(stdout())

### Name: predict.crtree
### Title: Predict method for the crtree function
### Aliases: predict.crtree

### ** Examples

result <- crtree(titanic, "survived", c("pclass", "sex"), lev = "Yes")
predict(result, pred_cmd = "pclass = levels(pclass)")
result <- crtree(titanic, "survived", "pclass", lev = "Yes")
predict(result, pred_data = titanic) %>% head()



cleanEx()
nameEx("predict.gbt")
### * predict.gbt

flush(stderr()); flush(stdout())

### Name: predict.gbt
### Title: Predict method for the gbt function
### Aliases: predict.gbt

### ** Examples

result <- gbt(titanic, "survived", c("pclass", "sex"), early_stopping_rounds = 0)
predict(result, pred_cmd = "pclass = levels(pclass)")
result <- gbt(diamonds, "price", "carat:color", type = "regression")
predict(result, pred_cmd = "carat = 1:3")
predict(result, pred_data = diamonds) %>% head()



cleanEx()
nameEx("predict.logistic")
### * predict.logistic

flush(stderr()); flush(stdout())

### Name: predict.logistic
### Title: Predict method for the logistic function
### Aliases: predict.logistic

### ** Examples

result <- logistic(titanic, "survived", c("pclass", "sex"), lev = "Yes")
predict(result, pred_cmd = "pclass = levels(pclass)")
logistic(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>%
  predict(pred_cmd = "sex = c('male','female')")
logistic(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>%
  predict(pred_data = titanic)



cleanEx()
nameEx("predict.mnl")
### * predict.mnl

flush(stderr()); flush(stdout())

### Name: predict.mnl
### Title: Predict method for the mnl function
### Aliases: predict.mnl

### ** Examples

result <- mnl(
  ketchup,
  rvar = "choice",
  evar = c("price.heinz28", "price.heinz32", "price.heinz41", "price.hunts32"),
  lev = "heinz28"
)
predict(result, pred_cmd = "price.heinz28 = seq(3, 5, 0.1)")
predict(result, pred_data = slice(ketchup, 1:20))




cleanEx()
nameEx("predict.nb")
### * predict.nb

flush(stderr()); flush(stdout())

### Name: predict.nb
### Title: Predict method for the nb function
### Aliases: predict.nb

### ** Examples

result <- nb(titanic, "survived", c("pclass", "sex", "age"))
predict(result, pred_data = titanic)
predict(result, pred_data = titanic, pred_names = c("Yes", "No"))
predict(result, pred_cmd = "pclass = levels(pclass)")
result <- nb(titanic, "pclass", c("survived", "sex", "age"))
predict(result, pred_data = titanic)
predict(result, pred_data = titanic, pred_names = c("1st", "2nd", "3rd"))
predict(result, pred_data = titanic, pred_names = "")




cleanEx()
nameEx("predict.nn")
### * predict.nn

flush(stderr()); flush(stdout())

### Name: predict.nn
### Title: Predict method for the nn function
### Aliases: predict.nn

### ** Examples

result <- nn(titanic, "survived", c("pclass", "sex"), lev = "Yes")
predict(result, pred_cmd = "pclass = levels(pclass)")
result <- nn(diamonds, "price", "carat:color", type = "regression")
predict(result, pred_cmd = "carat = 1:3")
predict(result, pred_data = diamonds) %>% head()



cleanEx()
nameEx("predict.regress")
### * predict.regress

flush(stderr()); flush(stdout())

### Name: predict.regress
### Title: Predict method for the regress function
### Aliases: predict.regress

### ** Examples

result <- regress(diamonds, "price", c("carat", "clarity"))
predict(result, pred_cmd = "carat = 1:10")
predict(result, pred_cmd = "clarity = levels(clarity)")
result <- regress(diamonds, "price", c("carat", "clarity"), int = "carat:clarity")
predict(result, pred_data = diamonds) %>% head()




cleanEx()
nameEx("predict.rforest")
### * predict.rforest

flush(stderr()); flush(stdout())

### Name: predict.rforest
### Title: Predict method for the rforest function
### Aliases: predict.rforest

### ** Examples

result <- rforest(titanic, "survived", c("pclass", "sex"), lev = "Yes")
predict(result, pred_cmd = "pclass = levels(pclass)")
result <- rforest(diamonds, "price", "carat:color", type = "regression")
predict(result, pred_cmd = "carat = 1:3")
predict(result, pred_data = diamonds) %>% head()




cleanEx()
nameEx("profit")
### * profit

flush(stderr()); flush(stdout())

### Name: profit
### Title: Calculate Profit based on cost:margin ratio
### Aliases: profit

### ** Examples

profit(runif(20000), dvd$buy, "yes", cost = 1, margin = 2)
profit(ifelse(dvd$buy == "yes", 1, 0), dvd$buy, "yes", cost = 1, margin = 20)
profit(ifelse(dvd$buy == "yes", 1, 0), dvd$buy)



cleanEx()
nameEx("radiant.model")
### * radiant.model

flush(stderr()); flush(stdout())

### Name: radiant.model
### Title: radiant.model
### Aliases: radiant.model

### ** Examples

## Not run: 
##D radiant.model()
## End(Not run)



cleanEx()
nameEx("radiant.model_viewer")
### * radiant.model_viewer

flush(stderr()); flush(stdout())

### Name: radiant.model_viewer
### Title: Launch radiant.model in the Rstudio viewer
### Aliases: radiant.model_viewer

### ** Examples

## Not run: 
##D radiant.model_viewer()
## End(Not run)



cleanEx()
nameEx("radiant.model_window")
### * radiant.model_window

flush(stderr()); flush(stdout())

### Name: radiant.model_window
### Title: Launch radiant.model in an Rstudio window
### Aliases: radiant.model_window

### ** Examples

## Not run: 
##D radiant.model_window()
## End(Not run)



cleanEx()
nameEx("regress")
### * regress

flush(stderr()); flush(stdout())

### Name: regress
### Title: Linear regression using OLS
### Aliases: regress

### ** Examples

regress(diamonds, "price", c("carat", "clarity"), check = "standardize") %>% summary()
regress(diamonds, "price", c("carat", "clarity")) %>% str()




cleanEx()
nameEx("repeater")
### * repeater

flush(stderr()); flush(stdout())

### Name: repeater
### Title: Repeated simulation
### Aliases: repeater

### ** Examples

simdat <- simulater(
  const = c("var_cost 5", "fixed_cost 1000"),
  norm = "E 0 100;",
  discrete = "price 6 8 .3 .7;",
  form = c(
    "demand = 1000 - 50*price + E",
    "profit = demand*(price-var_cost) - fixed_cost",
    "profit_small = profit < 100"
  ),
  seed = 1234
)

repdat <- repeater(
  simdat,
  nr = 12,
  vars = c("E", "price"),
  sum_vars = "profit",
  byvar = ".sim",
  form = "profit_365 = profit_sum < 36500",
  seed = 1234,
)

head(repdat)
summary(repdat)
plot(repdat)




cleanEx()
nameEx("rforest")
### * rforest

flush(stderr()); flush(stdout())

### Name: rforest
### Title: Random Forest using Ranger
### Aliases: rforest

### ** Examples

rforest(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>% summary()
rforest(titanic, "survived", c("pclass", "sex")) %>% str()
rforest(titanic, "survived", c("pclass", "sex"), max.depth = 1)
rforest(diamonds, "price", c("carat", "clarity"), type = "regression") %>% summary()




cleanEx()
nameEx("rig")
### * rig

flush(stderr()); flush(stdout())

### Name: rig
### Title: Relative Information Gain (RIG)
### Aliases: rig

### ** Examples

rig(runif(20000), dvd$buy, "yes")
rig(ifelse(dvd$buy == "yes", 1, 0), dvd$buy, "yes")



cleanEx()
nameEx("sensitivity.dtree")
### * sensitivity.dtree

flush(stderr()); flush(stdout())

### Name: sensitivity.dtree
### Title: Evaluate sensitivity of the decision tree
### Aliases: sensitivity.dtree

### ** Examples

dtree(movie_contract, opt = "max") %>%
  sensitivity(
    vars = "legal fees 0 100000 10000",
    decs = c("Sign with Movie Company", "Sign with TV Network"),
    custom = FALSE
  )




cleanEx()
nameEx("sim_cor")
### * sim_cor

flush(stderr()); flush(stdout())

### Name: sim_cor
### Title: Simulate correlated normally distributed data
### Aliases: sim_cor

### ** Examples

sim <- sim_cor(100, .74, c(0, 10), c(1, 5), exact = TRUE)
cor(sim)
sim_summary(sim)




cleanEx()
nameEx("sim_summary")
### * sim_summary

flush(stderr()); flush(stdout())

### Name: sim_summary
### Title: Print simulation summary
### Aliases: sim_summary

### ** Examples

simulater(
  const = "cost 3",
  norm = "demand 2000 1000",
  discrete = "price 5 8 .3 .7",
  form = c("profit = demand * (price - cost)", "profit5K = profit > 5000"),
  seed = 1234
) %>% sim_summary()




cleanEx()
nameEx("simulater")
### * simulater

flush(stderr()); flush(stdout())

### Name: simulater
### Title: Simulate data for decision analysis
### Aliases: simulater

### ** Examples

simulater(
  const = "cost 3",
  norm = "demand 2000 1000",
  discrete = "price 5 8 .3 .7",
  form = "profit = demand * (price - cost)",
  seed = 1234
) %>% str()




cleanEx()
nameEx("store.mnl.predict")
### * store.mnl.predict

flush(stderr()); flush(stdout())

### Name: store.mnl.predict
### Title: Store predicted values generated in the mnl function
### Aliases: store.mnl.predict

### ** Examples

result <- mnl(
  ketchup,
  rvar = "choice",
  evar = c("price.heinz28", "price.heinz32", "price.heinz41", "price.hunts32"),
  lev = "heinz28"
)
pred <- predict(result, pred_data = ketchup)
ketchup <- store(ketchup, pred, name = c("heinz28", "heinz32", "heinz41", "hunts32"))




cleanEx()
nameEx("store.model")
### * store.model

flush(stderr()); flush(stdout())

### Name: store.model
### Title: Store residuals from a model
### Aliases: store.model

### ** Examples

regress(diamonds, rvar = "price", evar = c("carat", "cut"), data_filter = "price > 1000") %>%
  store(diamonds, ., name = "resid") %>%
  head()




cleanEx()
nameEx("store.model.predict")
### * store.model.predict

flush(stderr()); flush(stdout())

### Name: store.model.predict
### Title: Store predicted values generated in model functions
### Aliases: store.model.predict

### ** Examples

regress(diamonds, rvar = "price", evar = c("carat", "cut")) %>%
  predict(pred_data = diamonds) %>%
  store(diamonds, ., name = c("pred", "pred_low", "pred_high")) %>%
  head()




cleanEx()
nameEx("store.nb.predict")
### * store.nb.predict

flush(stderr()); flush(stdout())

### Name: store.nb.predict
### Title: Store predicted values generated in the nb function
### Aliases: store.nb.predict

### ** Examples

result <- nb(titanic, rvar = "survived", evar = c("pclass", "sex", "age"))
pred <- predict(result, pred_data = titanic)
titanic <- store(titanic, pred, name = c("Yes", "No"))




cleanEx()
nameEx("store.rforest.predict")
### * store.rforest.predict

flush(stderr()); flush(stdout())

### Name: store.rforest.predict
### Title: Store predicted values generated in the rforest function
### Aliases: store.rforest.predict

### ** Examples

result <- rforest(
  ketchup,
  rvar = "choice",
  evar = c("price.heinz28", "price.heinz32", "price.heinz41", "price.hunts32"),
  lev = "heinz28"
)
pred <- predict(result, pred_data = ketchup)
ketchup <- store(ketchup, pred, name = c("heinz28", "heinz32", "heinz41", "hunts32"))




cleanEx()
nameEx("summary.confusion")
### * summary.confusion

flush(stderr()); flush(stdout())

### Name: summary.confusion
### Title: Summary method for the confusion matrix
### Aliases: summary.confusion

### ** Examples

data.frame(buy = dvd$buy, pred1 = runif(20000), pred2 = ifelse(dvd$buy == "yes", 1, 0)) %>%
  confusion(c("pred1", "pred2"), "buy") %>%
  summary()



cleanEx()
nameEx("summary.crs")
### * summary.crs

flush(stderr()); flush(stdout())

### Name: summary.crs
### Title: Summary method for Collaborative Filter
### Aliases: summary.crs

### ** Examples

crs(ratings,
  id = "Users", prod = "Movies", pred = c("M6", "M7", "M8", "M9", "M10"),
  rate = "Ratings", data_filter = "training == 1"
) %>% summary()



cleanEx()
nameEx("summary.crtree")
### * summary.crtree

flush(stderr()); flush(stdout())

### Name: summary.crtree
### Title: Summary method for the crtree function
### Aliases: summary.crtree

### ** Examples

result <- crtree(titanic, "survived", c("pclass", "sex"), lev = "Yes")
summary(result)
result <- crtree(diamonds, "price", c("carat", "color"), type = "regression")
summary(result)



cleanEx()
nameEx("summary.dtree")
### * summary.dtree

flush(stderr()); flush(stdout())

### Name: summary.dtree
### Title: Summary method for the dtree function
### Aliases: summary.dtree

### ** Examples

dtree(movie_contract, opt = "max") %>% summary(input = TRUE)
dtree(movie_contract, opt = "max") %>% summary(input = FALSE, output = TRUE)




cleanEx()
nameEx("summary.evalbin")
### * summary.evalbin

flush(stderr()); flush(stdout())

### Name: summary.evalbin
### Title: Summary method for the evalbin function
### Aliases: summary.evalbin

### ** Examples

data.frame(buy = dvd$buy, pred1 = runif(20000), pred2 = ifelse(dvd$buy == "yes", 1, 0)) %>%
  evalbin(c("pred1", "pred2"), "buy") %>%
  summary()



cleanEx()
nameEx("summary.evalreg")
### * summary.evalreg

flush(stderr()); flush(stdout())

### Name: summary.evalreg
### Title: Summary method for the evalreg function
### Aliases: summary.evalreg

### ** Examples

data.frame(price = diamonds$price, pred1 = rnorm(3000), pred2 = diamonds$price) %>%
  evalreg(pred = c("pred1", "pred2"), "price") %>%
  summary()




cleanEx()
nameEx("summary.gbt")
### * summary.gbt

flush(stderr()); flush(stdout())

### Name: summary.gbt
### Title: Summary method for the gbt function
### Aliases: summary.gbt

### ** Examples

result <- gbt(titanic, "survived", c("pclass", "sex"), early_stopping_rounds = 0) %>% str()
summary(result)



cleanEx()
nameEx("summary.logistic")
### * summary.logistic

flush(stderr()); flush(stdout())

### Name: summary.logistic
### Title: Summary method for the logistic function
### Aliases: summary.logistic

### ** Examples


result <- logistic(titanic, "survived", "pclass", lev = "Yes")
result <- logistic(titanic, "survived", "pclass", lev = "Yes")
summary(result, test_var = "pclass")
res <- logistic(titanic, "survived", c("pclass", "sex"), int = "pclass:sex", lev = "Yes")
summary(res, sum_check = c("vif", "confint", "odds"))
titanic %>%
  logistic("survived", c("pclass", "sex", "age"), lev = "Yes") %>%
  summary("vif")



cleanEx()
nameEx("summary.mnl")
### * summary.mnl

flush(stderr()); flush(stdout())

### Name: summary.mnl
### Title: Summary method for the mnl function
### Aliases: summary.mnl

### ** Examples

result <- mnl(
  ketchup,
  rvar = "choice",
  evar = c("price.heinz28", "price.heinz32", "price.heinz41", "price.hunts32"),
  lev = "heinz28"
)
summary(result)




cleanEx()
nameEx("summary.nb")
### * summary.nb

flush(stderr()); flush(stdout())

### Name: summary.nb
### Title: Summary method for the nb function
### Aliases: summary.nb

### ** Examples

result <- nb(titanic, "survived", c("pclass", "sex", "age"))
summary(result)




cleanEx()
nameEx("summary.nn")
### * summary.nn

flush(stderr()); flush(stdout())

### Name: summary.nn
### Title: Summary method for the nn function
### Aliases: summary.nn

### ** Examples

result <- nn(titanic, "survived", "pclass", lev = "Yes")
summary(result)



cleanEx()
nameEx("summary.regress")
### * summary.regress

flush(stderr()); flush(stdout())

### Name: summary.regress
### Title: Summary method for the regress function
### Aliases: summary.regress

### ** Examples

result <- regress(diamonds, "price", c("carat", "clarity"))
summary(result, sum_check = c("rmse", "sumsquares", "vif", "confint"), test_var = "clarity")
result <- regress(ideal, "y", c("x1", "x2"))
summary(result, test_var = "x2")
ideal %>%
  regress("y", "x1:x3") %>%
  summary()




cleanEx()
nameEx("summary.rforest")
### * summary.rforest

flush(stderr()); flush(stdout())

### Name: summary.rforest
### Title: Summary method for the rforest function
### Aliases: summary.rforest

### ** Examples

result <- rforest(titanic, "survived", "pclass", lev = "Yes")
summary(result)




cleanEx()
nameEx("summary.simulater")
### * summary.simulater

flush(stderr()); flush(stdout())

### Name: summary.simulater
### Title: Summary method for the simulater function
### Aliases: summary.simulater

### ** Examples

simdat <- simulater(norm = "demand 2000 1000", seed = 1234)
summary(simdat)




cleanEx()
nameEx("test_specs")
### * test_specs

flush(stderr()); flush(stdout())

### Name: test_specs
### Title: Add interaction terms to list of test variables if needed
### Aliases: test_specs

### ** Examples

test_specs("a", "a:b")
test_specs("a", c("a:b", "b:c"))
test_specs("a", c("a:b", "b:c", "I(c^2)"))
test_specs(c("a", "b", "c"), c("a:b", "b:c", "I(c^2)"))




cleanEx()
nameEx("var_check")
### * var_check

flush(stderr()); flush(stdout())

### Name: var_check
### Title: Check if main effects for all interaction effects are included
###   in the model
### Aliases: var_check

### ** Examples

var_check("a:d", c("a", "b", "c", "d"))
var_check(c("a", "b"), c("a", "b"), "a:c")
var_check(c("a", "b"), c("a", "b"), "a:c")
var_check(c("a", "b"), c("a", "b"), c("a:c", "I(b^2)"))




cleanEx()
nameEx("write.coeff")
### * write.coeff

flush(stderr()); flush(stdout())

### Name: write.coeff
### Title: Write coefficient table for linear and logistic regression
### Aliases: write.coeff

### ** Examples


regress(
  diamonds,
  rvar = "price", evar = c("carat", "clarity", "color", "x"),
  int = c("carat:clarity", "clarity:color", "I(x^2)"), check = "standardize"
) %>%
  write.coeff(sort = TRUE) %>%
  format_df(dec = 3)

logistic(titanic, "survived", c("pclass", "sex"), lev = "Yes") %>%
  write.coeff(intercept = FALSE, sort = TRUE) %>%
  format_df(dec = 2)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
