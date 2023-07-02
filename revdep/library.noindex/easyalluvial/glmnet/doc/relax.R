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

## ----out.lines = 15-----------------------------------------------------------
library(glmnet)
data(QuickStartExample)
x <- QuickStartExample$x
y <- QuickStartExample$y
fit <- glmnet(x, y, relax = TRUE)
print(fit)

## -----------------------------------------------------------------------------
par(mfrow = c(1, 3), mar=c(4,4,5.5,1))
plot(fit, main = "gamma = 1")
plot(fit, gamma = 0.5, main = "gamma = 0.5")
plot(fit, gamma = 0, main = "gamma = 0")

## -----------------------------------------------------------------------------
set.seed(1)
cfit <- cv.glmnet(x, y, relax = TRUE)
plot(cfit)

## -----------------------------------------------------------------------------
plot(cfit, se.bands = FALSE)

## -----------------------------------------------------------------------------
predict(cfit, newx = x[1:5, ], s = "lambda.min", gamma = "gamma.min")

## -----------------------------------------------------------------------------
print(cfit)

## ----`relaxed`----------------------------------------------------------------
fit <- glmnet(x,y)
fitr <- relax.glmnet(fit, x = x, y = y)

## -----------------------------------------------------------------------------
print(cfit)
print.cv.glmnet(cfit)

## -----------------------------------------------------------------------------
fitr <- cv.glmnet(x, y, gamma = 0, relax = TRUE)
plot(fitr)

