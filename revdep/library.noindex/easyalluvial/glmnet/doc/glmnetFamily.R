## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
fam <- binomial()
class(fam)
names(fam)

## -----------------------------------------------------------------------------
set.seed(1)
x <- matrix(rnorm(500), ncol = 5)
y <- rowSums(x[, 1:2]) + rnorm(100)

## ----message = FALSE----------------------------------------------------------
library(glmnet)
oldfit <- glmnet(x, y, family = "gaussian")
newfit <- glmnet(x, y, family = gaussian())

## -----------------------------------------------------------------------------
thresh <- 1e-18
oldfit <- glmnet(x, y, family="gaussian", thresh = thresh)
newfit <- glmnet(x, y, family = gaussian(), thresh = thresh)

# tests for equality
library(testthat)
for (key in c("a0", "beta", "df", "dim", "lambda", "dev.ratio", 
              "nulldev", "offset", "nobs")) {
  expect_equal(oldfit[[key]], newfit[[key]])
}

## -----------------------------------------------------------------------------
biny <- ifelse(y > 0, 1, 0)  # binary data
cnty <- ceiling(exp(y))      # count data

# fitting binomial GLMs the old and new way
oldfit <- glmnet(x, biny, family = "binomial")
newfit <- glmnet(x, biny, family = binomial())

# fitting Poisson GLMs the old and new way
oldfit <- glmnet(x, cnty, family = "poisson")
newfit <- glmnet(x, cnty, family = poisson())

## -----------------------------------------------------------------------------
newfit <- glmnet(x, biny, family = binomial(link = "probit"))

## -----------------------------------------------------------------------------
newfit <- glmnet(x, cnty, family = quasipoisson())

## ---- eval=FALSE--------------------------------------------------------------
#  library(MASS)
#  newfit <- glmnet(x, cnty, family = negative.binomial(theta = 5))

## -----------------------------------------------------------------------------
class(newfit)

## -----------------------------------------------------------------------------
fit <- glmnet(x, y, family = "gaussian")
class(fit)

## -----------------------------------------------------------------------------
set.seed(2020)
n <- 100
p <- 4
x <- matrix(runif(n * p, 5, 10), n)
y <- rpois(n, exp(rowMeans(x)))

# glm fit
glmfit <- glm(y ~ x - 1, family = poisson)
coef(glmfit)

## -----------------------------------------------------------------------------
oldfit <- glmnet(x, y, family = "poisson", standardize = FALSE, 
                 intercept = FALSE, lambda = 0)
coef(oldfit)

## -----------------------------------------------------------------------------
glmnet.control(mxitnr = 50)  # increase maximum no. of IRLS iterations allowed
newfit <- glmnet(x, y, family = poisson(), standardize = FALSE, 
                 intercept = FALSE, lambda = 0)
coef(newfit)

## -----------------------------------------------------------------------------
thresh <- 1e-15
glmfit <- glm(y ~ x-1, family = poisson,
              control = list(epsilon = thresh, maxit = 100))
newfit <- glmnet(x, y, family = poisson(), standardize = FALSE, intercept = FALSE,
                 lambda = 0, thresh = thresh)

# coef(glmfit) doesn't have intercept but coef(newfit does)
expect_equal(as.numeric(coef(glmfit)), 
             as.numeric(coef(newfit))[2:5])

