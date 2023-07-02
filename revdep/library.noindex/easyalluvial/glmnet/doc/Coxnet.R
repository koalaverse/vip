## ----include=FALSE------------------------------------------------------------
# the code in this chunk enables us to truncate the print output for each
# chunk using the `out.lines` option
# save the built-in output hook
hook_output <- knitr::knit_hooks$get("output")

# set a new output hook to truncate text output
knitr::knit_hooks$set(output = function(x, options) {
  if (!is.null(n <- options$out.lines)) {
    x <- xfun::split_lines(x)
    if (length(x) > n) {
      # truncate the output
      x <- c(head(x, n), "....\n")
    }
    x <- paste(x, collapse = "\n")
  }
  hook_output(x, options)
})

## -----------------------------------------------------------------------------
library(glmnet)
library(survival)
data(CoxExample)
x <- CoxExample$x
y <- CoxExample$y
y[1:5, ]

## -----------------------------------------------------------------------------
fit <- glmnet(x, y, family = "cox")

## -----------------------------------------------------------------------------
plot(fit)

## ----out.lines = 10-----------------------------------------------------------
coef(fit, s = 0.05)

## -----------------------------------------------------------------------------
set.seed(1)
cvfit <- cv.glmnet(x, y, family = "cox", type.measure = "C")

## -----------------------------------------------------------------------------
plot(cvfit)

## -----------------------------------------------------------------------------
cvfit$lambda.min
cvfit$lambda.1se

## -----------------------------------------------------------------------------
# create x matrix
set.seed(1)
nobs <- 100; nvars <- 15
x <- matrix(rnorm(nobs * nvars), nrow = nobs)

# create response
ty <- rep(rexp(nobs / 5), each = 5)
tcens <- rbinom(n = nobs, prob = 0.3, size = 1)
y <- Surv(ty, tcens)

# coefficients from these two models will not line up because
# of different tie handling methods
glmnet_fit <- glmnet(x, y, family = "cox", lambda = 0)
coxph_fit <- coxph(y ~ x)
plot(coef(glmnet_fit), coef(coxph_fit))
abline(0, 1)

## -----------------------------------------------------------------------------
# coefficients from these two models will line up
glmnet_fit <- glmnet(x, y, family = "cox", lambda = 0)
coxph_fit <- coxph(y ~ x, ties = "breslow")
plot(coef(glmnet_fit), coef(coxph_fit))
abline(0, 1)

## -----------------------------------------------------------------------------
# create x matrix
set.seed(2)
nobs <- 100; nvars <- 15
xvec <- rnorm(nobs * nvars)
xvec[sample.int(nobs * nvars, size = 0.4 * nobs * nvars)] <- 0
x <- matrix(xvec, nrow = nobs)  # non-sparse x
x_sparse <- Matrix::Matrix(xvec, nrow = nobs, sparse = TRUE)  # sparse x

# create start-stop data response
beta <- rnorm(5)
fx <- x_sparse[, 1:5] %*% beta / 3
ty <- rexp(nobs, drop(exp(fx)))
tcens <- rbinom(n = nobs, prob = 0.3, size = 1)
starty <- runif(nobs)
yss <- Surv(starty, starty + ty, tcens)

# fit regularized Cox model with start-stop data
fit <- glmnet(x, yss, family = "cox")

## -----------------------------------------------------------------------------
cv.fit <- cv.glmnet(x, yss, family = "cox", nfolds = 5)
plot(cv.fit)

## -----------------------------------------------------------------------------
glmnet_fit <- glmnet(x, yss, family = "cox", lambda = 0)
coxph_fit <- coxph(yss ~ x)
plot(coef(glmnet_fit), coef(coxph_fit))
abline(0, 1)

## -----------------------------------------------------------------------------
strata <- rep(1:5, length.out = nobs)
y2 <- stratifySurv(y, strata)
str(y2[1:6])

## -----------------------------------------------------------------------------
fit <- glmnet(x, y2, family = "cox")

## -----------------------------------------------------------------------------
cv.fit <- cv.glmnet(x, y2, family = "cox", nfolds = 5)
plot(cv.fit)

## -----------------------------------------------------------------------------
y3 <- y
attr(y3, "strata") <- strata
str(y3[1:6])  # note that the strata attribute is no longer there

## ----error=TRUE---------------------------------------------------------------
fit <- glmnet(x, y3, family = "cox")

## -----------------------------------------------------------------------------
fit <- glmnet(x, y, family = "cox")
survival::survfit(fit, s = 0.05, x = x, y = y)

## -----------------------------------------------------------------------------
plot(survival::survfit(fit, s = 0.05, x = x, y = y))

## -----------------------------------------------------------------------------
survival::survfit(fit, s = 0.05, x = x, y = y, newx = x[1:3, ])
plot(survival::survfit(fit, s = 0.05, x = x, y = y, newx = x[1:3, ]))

## -----------------------------------------------------------------------------
y2 <- stratifySurv(y, rep(1:2, length.out = nobs))
fit <- glmnet(x, y2, family = "cox")
survival::survfit(fit, s = 0.01, x = x, y = y2)

# survival curve plot for first two individuals in dataset
plot(survival::survfit(fit, s = 0.01, x = x, y = y2, 
                       newx = x[1:2, ], newstrata = strata[1:2]))

## -----------------------------------------------------------------------------
sf <- survival::survfit(fit, x = x, y = y2)
length(sf)
length(fit$lambda)

## -----------------------------------------------------------------------------
cv.fit <- cv.glmnet(x, y2, family = "cox", nfolds = 5)
survival::survfit(cv.fit, x = x, y = y2)
survival::survfit(cv.fit, s = "lambda.min", x = x, y = y2)

