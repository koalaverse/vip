# test.weights.R

source("test.prolog.R")
source("check.models.equal.R")
library(earth)
options(warn=1) # print warnings as they occur
check.equal <- function(x, y, msg="")
{
    diff <- x - y
    if (any(abs(diff) > 1e-8)) {
        cat(msg, "\n1st matrix:\n", sep="")
        print(x)
        cat("\n2nd matrix:\n")
        print(y)
        cat("\ndiff:\n")
        print(diff)
        stop("check.equal failed for ", msg, call.=FALSE)
    }
}
check.earth.lm.models.equal <- function(lm.mod, earth.mod)
{
    lm.mod.name <- deparse(substitute(lm.mod))
    earth.mod.name <- deparse(substitute(earth.mod))
    msg <- sprint("%s vs %s", lm.mod.name, earth.mod.name)
    check.equal(lm.mod$coefficients,       earth.mod$coefficients,       msg=sprint("%s coefficients", msg))
    check.equal(lm.mod$rss,                earth.mod$rss,                msg=sprint("%s rss", msg))
    check.equal(lm.mod$residuals,          earth.mod$residuals,          msg=sprint("%s residuals", msg))
    check.equal(summary(lm.mod)$r.squared, earth.mod$rsq,                msg=sprint("%s rsq", msg))
    check.equal(summary(lm.mod)$r.squared, earth.mod$rsq.per.reponse[1], msg=sprint("%s rsq.per.response", msg))
}
# artifical data
xxx <- 1:9
yyy <- 1:9
yyy[5] <- 9
data <- data.frame(x=xxx, y=yyy)
colnames(data) <- c("x", "y")

# Check against a linear model with weights, using linpreds.
# This also checks the backward pass's handling of weights.

lm1 <- lm(y~., data=data)
a1 <- earth(y~., data=data, linpreds=TRUE)
check.earth.lm.models.equal(lm1, a1)

weights <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
lm2 <- lm(y~., data=data, weights=weights)
a2  <- earth(y~., data=data, linpreds=TRUE, weights=weights)
check.earth.lm.models.equal(lm2, a2)

# check that we can get the weights from the data as per lm
lm2.a <- lm(y~xxx, data=data, weights=x) # weights from model frame
a2.a  <- earth(y~xxx, data=data, linpreds=TRUE, weights=x) # weights from model frame
a2.b  <- earth(y~xxx, data=data, linpreds=TRUE, weights=xxx) # weights from global env
check.earth.lm.models.equal(lm2.a, a2.a)
check.earth.lm.models.equal(a2.b, a2.a)

weights <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
lm3 <- lm(y~., data=data, weights=weights)
a3  <- earth(y~., data=data, linpreds=TRUE, weights=weights, trace=-1)
check.earth.lm.models.equal(lm3, a3)

expect.err(try(earth(y~., data=data, wp=3, Scale.y=TRUE)), "Scale.y=TRUE is not allowed with wp")
allthrees <- rep(3.0, length.out=nrow(data))
options(warn=2)
expect.err(try(earth(allthrees~x, data=data)), "Cannot scale y (values are all equal to 3)")
options(warn=1)
allthrees.mod <- earth(allthrees~x, data=data)
print(summary(allthrees.mod))
# Scale.y=FALSE allows us to use a response that is constant (silences the error message)
allthrees.mod.noscale <- earth(allthrees~x, data=data, Scale.y=FALSE) # intercept only
print(summary(allthrees.mod.noscale))
stopifnot(identical(allthrees.mod$coefficients, allthrees.mod.noscale$coefficients))

subset <- c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
lm3.weights <- lm(y~., data=data, weights=weights, subset=subset)
a3.weights  <- earth(y~., data=data, linpreds=TRUE, weights=weights, trace=-1, subset=subset)
check.earth.lm.models.equal(lm3.weights, a3.weights)

lm4 <- lm(y~., data=data, weights=.1 * weights)
a4  <- earth(y~., data=data, linpreds=TRUE, weights=.1 * weights,
             minspan=1, endspan=1, penalty=-1, thresh=1e-8, trace=-1)
cat("a4:\n")
print(a4)
check.earth.lm.models.equal(lm4, a4)

# We want to see the effect only on the forward pass, so disable the
# backward pass with penalty=-1.  This also prevents "termination of the
# forward pass with a negative GRSq" with this artifical data.
#
# We can't use thresh=0, because then very small weights will still cause a usable
# reduction in RSq (remember that weights of zero are changed to very small weights
# in the current implementation).  So instead we use thresh=1e-8.
# This is a problem only with this very artifical data.  With real data, we
# want to use the standard thresh=.001, even with weights.

cat("=== a5.noweights ===\n")
par(mfrow = c(2, 2))
par(mar = c(3, 3, 3, 1))
par(mgp = c(1.5, 0.5, 0))
a5.noweights <- earth(y~., data=data,
                      minspan=1, endspan=1, penalty=-1, thresh=1e-8, trace=3)
plotmo(a5.noweights, col.response=2, do.par=F, main="a5.noweights", grid.col="gray", jitter=0)
# TODO why does this model differ from the above model?
a5.noweights.force <- earth(y~., data=data, Force.weights=T,
                      minspan=1, endspan=1, penalty=-1, thresh=1e-8, trace=3)
cat("a5.noweights.force:\n")
print(a5.noweights.force)
plotmo(a5.noweights.force, col.response=2, do.par=F, main="a5.noweights.force", grid.col="gray", jitter=0)

cat("=== a6.azeroweight ===\n")
a6.azeroweight  <- earth(y~., data=data, weights=c(1, 1, 1, 1, 0, 1, 1, 1, 1),
                         minspan=1, endspan=1, penalty=-1, thresh=1e-8, trace=3)
cat("a6.azeroweight:\n")
print(a6.azeroweight)
plotmo(a6.azeroweight, col.response=2, do.par=F, main="a6.azeroweight", grid.col="gray", jitter=0)

cat("=== a7.asmallweight ===\n") # different set of weights (pick up notch in data, but with different forward pass RSq's)
a7.asmallweight  <- earth(y~., data=data, weights=c(1, 1, 1, 1, .5, 1, 1, 1, 1),
                          minspan=1, endspan=1, penalty=-1, thresh=1e-8, trace=3)
plotmo(a7.asmallweight, col.response=2, do.par=F, main="a7.asmallweight", grid.col="gray", jitter=0)

cat("=== a7.xy.asmallweight ===\n") # x,y interface
a7.xy.asmallweight  <- earth(xxx, yyy, weights=c(1, 1, 1, 1, .5, 1, 1, 1, 1),
                          minspan=1, endspan=1, penalty=-1, thresh=1e-8, trace=3)
check.earth.lm.models.equal(a7.xy.asmallweight, a7.xy.asmallweight)

cat("=== a8 ===\n")
par(mfrow = c(3, 2)) # new page
par(mar = c(3, 3, 3, 1))
par(mgp = c(1.5, 0.5, 0))
data$y <- c(0, 0, 0, 1, 0, 1, 1, 1, 1) != 0

# glm models first without weights
a8 <- earth(y~., data=data,
            linpreds=TRUE, glm=list(family=binomial),
            minspan=1, endspan=1, penalty=-1, thresh=1e-8, trace=-1)
plotmo(a8,
       col.response=2, do.par=F, main="a8 glm no weights\ntype=\"response\"",
       grid.col="gray", ylim=c(-.2, 1.2), jitter=0)
plotmo(a8, type="earth",
       col.response=2, do.par=F, main="a8 glm no weights\ntype=\"earth\"",
       grid.col="gray", ylim=c(-.2, 1.2), jitter=0)
glm.a8 <- glm(y~., data=data, family=binomial)
stopifnot(coefficients(a8$glm.list[[1]]) == coefficients(glm.a8))

cat("=== a8.weights ===\n")
# now glm models with weights
glm.weights <- c(.8,1,1,.5,1,1,1,1,1)
# The following calls to earth and glm both give "Warning: non-integer #successes in a binomial glm"
# See https://stackoverflow.com/questions/12953045/warning-non-integer-successes-in-a-binomial-glm-survey-packages
a8.weights <- earth(y~., data=data,
                    linpreds=TRUE, glm=list(family=binomial),
                    weights=glm.weights,
                    minspan=1, endspan=1, penalty=-1, thresh=1e-8, trace=-1)
cat("a8.weights:\n")
print(a8.weights)
plotmo(a8.weights, type="response",
       col.response=2, do.par=F, main="a8.weights glm\ntype=\"response\"",
       grid.col="gray", ylim=c(-.2, 1.2), jitter=0)
plotmo(a8.weights, type="earth",
       col.response=2, do.par=F, main="a8.weights glm\ntype=\"earth\"",
       grid.col="gray", ylim=c(-.2, 1.2), jitter=0)
glm.a8.weights <- glm(y~., data=data, weights=glm.weights, family=binomial)
stopifnot(coefficients(a8.weights$glm.list[[1]]) == coefficients(glm.a8.weights))
stopifnot(a8.weights$glm.list[[1]]$aic == glm.a8.weights$aic)
source("check.earth.matches.glm.R")
check.earth.matches.glm(a8.weights, glm.a8.weights, newdata=data[2:6,])

options(warn=2) # treat warnings as errors
# same as a8.weights but use family=quasibinomial
# (test no Warning: non-integer #successes in a binomial glm)
a8.weights.quasibinomial <- earth(y~., data=data,
                    linpreds=TRUE, glm=list(family=quasibinomial),
                    weights=glm.weights,
                    minspan=1, endspan=1, penalty=-1, thresh=1e-8, trace=-1)
options(warn=1)
cat("a8.weights.quasibinomial:\n")
print(a8.weights.quasibinomial)
check.models.equal(a8.weights, a8.weights.quasibinomial, "a8.weights, a8.weights.quasibinomial", newdata=data[2,])

# glm model with weights and subset
# To suppress "Warning: non-integer #successes in a binomial glm" we use quasibinomial rather than binomial
# See https://stackoverflow.com/questions/12953045/warning-non-integer-successes-in-a-binomial-glm-survey-packages
a8.subset <- c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
a8.weights.subset <- earth(y~., data=data,
                    linpreds=TRUE, glm=list(family=quasibinomial),
                    weights=glm.weights, subset=a8.subset,
                    minspan=1, endspan=1, penalty=-1, thresh=1e-8, trace=1)
glm.a8.weights.subset <- glm(y~., data=data, weights=glm.weights,  subset=a8.subset, family=quasibinomial)
stopifnot(coefficients(a8.weights.subset$glm.list[[1]]) == coefficients(glm.a8.weights.subset))
stopifnot(a8.weights.subset$glm.list[[1]]$deviance == glm.a8.weights.subset$deviance)
# AIC is NA because we use quasibinomial rather than binomial
stopifnot(is.na(a8.weights.subset$glm.list[[1]]$aic))
stopifnot(is.na(glm.a8.weights.subset$aic))
cat("summary(a8.weights.subset:\n")
print(summary(a8.weights.subset))
cat("summary(glm,a8.weights.subset:\n")
print(summary(glm.a8.weights.subset))

cat("=== a8.weights including a zero weight ===\n")
# now glm models with weights including a zero weight
glm.weights <- c(.8,1,1,0,1,1,1,1,1)
a8.azeroweight <- earth(y~., data=data,
                    linpreds=TRUE, glm=list(family=binomial),
                    weights=glm.weights,
                    minspan=1, endspan=1, penalty=-1, thresh=1e-8, trace=-1)
plotmo(a8.azeroweight, type="response",
       col.response=2, do.par=F, main="a8.azeroweight glm\ntype=\"response\"",
       grid.col="gray", ylim=c(-.2, 1.2), jitter=0)
plotmo(a8.azeroweight, type="earth",
       col.response=2, do.par=F, main="a8.azeroweight glm\ntype=\"earth\"",
       grid.col="gray", ylim=c(-.2, 1.2), jitter=0)
glm.a8.azeroweight <- glm(y~., data=data, weights=glm.weights, family=binomial)
# # TODO this fails because a weight is 0 in glm.weights
# print(coefficients(a8.azeroweight$glm.list[[1]]))
# print(coefficients(glm.a8.azeroweight))
# stopifnot(coefficients(a8.azeroweight$glm.list[[1]]) == coefficients(glm.a8.azeroweight))

cat("=== plot.earth with weights ===\n")
# we also test id.n=TRUE and id.n=-1 here
par(mfrow=c(2,2), mar=c(4, 3.2, 3, 3), mgp=c(1.6, 0.6, 0), oma=c(0,0,3,0), cex=1)
plot(a3, id.n=TRUE, SHOWCALL=TRUE, caption="compare a3 to to lm3", do.par=FALSE,
     which=c(3,4), caption.cex=1.5)
plot(lm3, id.n=9, which=c(1,2), sub.caption="")
par(org.par)

cat("=== plot.earth with earth-glm model and weights ===\n")
plot(a8, id.n=TRUE, caption="a8")
plot(a8.weights, id.n=TRUE, caption="a8.weights")
plotres(glm.a8.weights, id.n=TRUE, caption="plotres: glm.a8.weights")
plot(a8.weights, id.n=TRUE, delever=TRUE, caption="a8.weights delever=TRUE")

set.seed(2019)
plotmo(a8.weights,     pt.col=2, caption="plotmo: a8.weights")
set.seed(2019)
plotmo(glm.a8.weights, pt.col=2, caption="plotmo: glm.a8.weights")

cat("=== plot.earth with earth-glm model and weights including a zero weight ===\n")
set.seed(2019)
plotmo(a8.azeroweight,     pt.col=2, caption="plotmo: a8.azeroweight")
set.seed(2019)
plotmo(glm.a8.azeroweight, pt.col=2, caption="plotmo: glm.a8.azeroweight")

cat("=== plot.earth with earth-glm model, weights ===\n")

# multivariate models

noise <- .01 * c(1,2,3,2,1,3,5,2,0)
data <- data.frame(x1=c(1,2,3,4,5,6,7,8,9), x2=c(1,2,3,3,3,6,7,8,9), y=(1:9)+noise)
data[5,] <- c(5, 5, 6)
colnames(data) <- c("x1", "x2", "y")

weights <- c(3, 2, 1, 1, 2, 3, 1, 2, 3)
lm20 <- lm(y~., data=data, weights=weights)
a20  <- earth(y~., data=data, linpreds=TRUE, weights=weights,
              minspan=1, endspan=1, penalty=-1, thresh=1e-8, trace=-1)
check.earth.lm.models.equal(lm20, a20)

a21.noweights <- earth(y~., data=data, # no weights for comparison
                       minspan=1, endspan=1, penalty=-1, thresh=1e-8, trace=-1)
plotmo(a21.noweights, col.resp=2, trace=-1, caption="a21.noweights", jitter=0)

weights <- c(1, 1, 1, 1, .5, 1, 1, 1, 1)
a10  <- earth(y~., data=data, weights=weights,
              minspan=1, endspan=1, penalty=-1, thresh=1e-8, trace=-1)
plotmo(a10, col.resp=2, caption="a10", jitter=0)

test.zigzag <- function()
{
    par(mfrow = c(2, 2), mar = c(3, 3, 3, 1), mgp = c(1.5, 0.5, 0), oma=c(0,0,0,0))
    TRACE <- 0
    THRESH <- 0
    PMETHOD <- "none"

#     # models are identical
#     x <- 1:21
#     y <- c(1:3, 2)
#     y <- rep(y, length.out=length(x))
#     data <- data.frame(x=x, y=y)
#     a <- earth(y~x, data=data, minspan=1, endspan=1, trace=TRACE, pmethod=PMETHOD, thresh=THRESH, Scale.y=FALSE, nk=201)
#     plot(x, y, type="p", pch=20)
#     lines(x, predict(a), col=3, pch=20)
#     aw <- earth(y~x, data=data, minspan=1, endspan=1, trace=TRACE, pmethod=PMETHOD, thresh=THRESH, Scale.y=FALSE, nk=201, Force.weights=T)
#     plot(x, y, type="p", pch=20)
#     lines(x, predict(aw), col=3, pch=20)

    # models are not identical
    x <- 1:81
    y <- c(1:3, 2)
    y <- rep(y, length.out=length(x))
    data <- data.frame(x=x, y=y)
    a <- earth(y~x, data=data, minspan=1, endspan=1, trace=TRACE, pmethod=PMETHOD, thresh=THRESH, Scale.y=FALSE, nk=201)
    plot(x, y, type="p", pch=20, main="without weights")
    lines(x, predict(a), col=3, pch=20)
    aw <- earth(y~x, data=data, minspan=1, endspan=1, trace=TRACE, pmethod=PMETHOD, thresh=THRESH, Scale.y=FALSE, nk=201, Force.weights=T)
    plot(x, y, type="p", pch=20, main="with weights")
    lines(x, predict(aw), col=3, pch=20)
}
# zigzag
test.zigzag()

# commented out because too slow and next test essentially covers this
# # trees
# a.trees <- earth(Volume~., data=trees, trace=2)
# aw.trees <- earth(Volume~., data=trees, trace=2, Force.weights=TRUE)
# plotmo(a.trees, do.par=2, caption="trees: top and bottom should be similar")
# plotmo(aw.trees, do.par=FALSE)
# par(org.par)

# bivariate.with.interaction
set.seed(2015)
n <- 18
x <- matrix(runif(2 * n, -1, 1), ncol=2)
x <- x[order(x[,1]), , drop=FALSE] # sort first column for convenience
colnames(x) <- paste("x", 1:ncol(x), sep="")
bivariate.with.interaction <- function(x)
{
    x[,1] + x[,2] + x[,1] * x[,2] + .05 * rnorm(nrow(x))
}
set.seed(1)
y <- bivariate.with.interaction(x)
a.biv  <- earth(x, y, degree=2, trace=2)
aw.biv <- earth(x, y, degree=2, trace=2, Force.weights=TRUE)
cat("aw.biv:\n")
print(aw.biv)

par(mfrow=c(2,3), mar=c(4, 3.2, 3, 3), mgp=c(1.6, 0.6, 0), cex = 0.8, oma=c(0,0,3,0))
plotmo(a.biv,  do.par=FALSE, caption="bivariate: top and bottom should be similar")
plotmo(aw.biv, do.par=FALSE)

# Comparison to glm and rpart
#
# The response y is split into two curves, we will weight the second lower
# curve and see how that affects the earth curve.
#
# With weight=1 the earth curve should be half way between the top and
# bottom curve.  With say weight=10, the bottom curve is given much more
# weight than the top curve, so the model should be closer to the bottom
# curve.
#
# We also compare the earth curve to to other models that support weights.
# Each vertical line of plots should be approximately the same.

library(gam)
library(rpart)
n <- 100
x1 <- c((-n:n) / n, (-n:n) / n)
x2 <- c((n:-n) / n, (-n:n) / n)
y <- x1 * x1
y[(2 * n + 2) : (3 * n + 2)] <- -.25 * y[(2 * n + 2): (3 * n + 2)]
y[(3 * n + 3) : (4 * n + 2)] <- .25 * y[(3 * n + 3) : (4 * n + 2)]
data <- data.frame(x1=x1, x2=x2, y=y)

par(mfcol = c(3, 5), mar = c(1.5, 4, 3, 2), mgp = c(1.5, 0.5, 0), oma=c(0,0,4,0))

cat("comparison to glm and rpart: unweighted\n")
a200 <- earth(y~x1, data=data)
plotmo(a200, do.par=FALSE, pt.col=2, main="unweighted\nearth", cex=.7, pt.cex=.2, grid.col=TRUE)
mtext("comparison to glm and rpart", outer=TRUE, line=2)
gam200 <- gam(y~s(x1, 5), data=data)
plotmo(gam200, do.par=FALSE, pt.col=2, main="gam", cex=.7, pt.cex=.2, grid.col=TRUE)
rpart <- rpart(y~x1, data=data, method="anova", control=rpart.control(cp=.001))
plotmo(rpart, do.par=FALSE, pt.col=2, main="rpart", cex=.7, pt.cex=.2, grid.col=TRUE, trace=-1)

cat("comparison to glm and rpart: weight=.1\n")
weight <- .1
w <- c(rep_len(1, 2 * n + 1), rep_len(weight, 2 * n + 1))
aw201 <- earth(y~x1, data=data, weights=w)
expect.err(try(earth(y~., data=data, wp=3, Scale.y=TRUE)), "Scale.y=TRUE is not allowed with wp")
expect.err(try(earth(y~., data=data, Scale.y=999)), "Scale.y=999 but it should be FALSE, TRUE, 0, or 1")
plotmo(aw201, do.par=FALSE, pt.col=2, main=sprint("weight %g\nearth", weight), cex=.7, pt.cex=.2, grid.col=TRUE)
gamw201 <- gam(y~s(x1, 5), data=data, weights=w)
plotmo(gamw201, do.par=FALSE, pt.col=2, main="", cex=.7, pt.cex=.2, grid.col=TRUE)
rpart <- rpart(y~x1, data=data, method="anova", control=rpart.control(cp=.001), weights=w)
plotmo(rpart, do.par=FALSE, pt.col=2, main="", cex=.7, pt.cex=.2, grid.col=TRUE, trace=-1)

cat("comparison to glm and rpart: weight=1\n")
weight <- 1
w <- c(rep_len(1, 2 * n + 1), rep_len(weight, 2 * n + 1))
aw202 <- earth(y~x1, data=data, weights=w)
plotmo(aw202, do.par=FALSE, pt.col=2, main=sprint("weight %g\nearth", weight), cex=.7, pt.cex=.2, grid.col=TRUE)
gamw202 <- gam(y~s(x1, 5), data=data, weights=w)
plotmo(gamw202, do.par=FALSE, pt.col=2, main="", cex=.7, pt.cex=.2, grid.col=TRUE)
rpart <- rpart(y~x1, data=data, method="anova", control=rpart.control(cp=.001), weights=w)
plotmo(rpart, do.par=FALSE, pt.col=2, main="", cex=.7, pt.cex=.2, grid.col=TRUE, trace=-1)

cat("comparison to glm and rpart: weight=2\n")
weight <- 2
w <- c(rep_len(1, 2 * n + 1), rep_len(weight, 2 * n + 1))
aw203 <- earth(y~x1, data=data, weights=w)
plotmo(aw203, do.par=FALSE, pt.col=2, main=sprint("weight %g\nearth", weight), cex=.7, pt.cex=.2, grid.col=TRUE)
gamw203 <- gam(y~s(x1, 5), data=data, weights=w)
plotmo(gamw203, do.par=FALSE, pt.col=2, main="", cex=.7, pt.cex=.2, grid.col=TRUE)
rpart <- rpart(y~x1, data=data, method="anova", control=rpart.control(cp=.001), weights=w)
plotmo(rpart, do.par=FALSE, pt.col=2, main="", cex=.7, pt.cex=.2, grid.col=TRUE, trace=-1)

cat("comparison to glm and rpart: weight=10\n")
weight <- 10
w <- c(rep_len(1, 2 * n + 1), rep_len(weight, 2 * n + 1))
aw204 <- earth(y~x1, data=data, weights=w)
plotmo(aw204, do.par=FALSE, pt.col=2, main=sprint("weight %g\nearth", weight), cex=.7, pt.cex=.2, grid.col=TRUE)
gamw204 <- gam(y~s(x1, 5), data=data, weights=w)
plotmo(gamw204, do.par=FALSE, pt.col=2, main="", cex=.7, pt.cex=.2, grid.col=TRUE)
rpart <- rpart(y~x1, data=data, method="anova", control=rpart.control(cp=.001), weights=w)
plotmo(rpart, do.par=FALSE, pt.col=2, main="", cex=.7, pt.cex=.2, grid.col=TRUE, trace=-1)

# # TODO the following are meant to do degree2 weight tests,
# #      but they are unconvincing either way, so commented out
#
# par(mfcol = c(3, 3), mar = c(1.5, 4, 3, 2), mgp = c(1.5, 0.5, 0), oma=c(0,0,6,0))
#
# y <- x2 * x2 * y
# data <- data.frame(x1=x1, x2=x2, y=y)
#
# cat("degree2 comparison to glm and rpart: unweighted\n")
# a200 <- earth(y~x1+x2, data=data, degree=2)
# plotmo(a200, do.par=FALSE, pt.col=2, cex=.7, pt.cex=.2, grid.col=TRUE, trace=-1, persp.ticktype="d")
# mtext("comparison to glm and rpart, degree2, unweighted\nleft side earth, right side gam200", outer=TRUE, line=2)
# gam200 <- gam(y~s(x1, 7)+s(x2, 7)+s(x1, 7)*s(x2, 7), data=data)
# plotmo(gam200, do.par=FALSE, pt.col=2, cex=.7, pt.cex=.2, grid.col=TRUE, all2=T, trace=-1, persp.ticktype="d")
# rpart <- rpart(y~x1+x2, data=data, method="anova", control=rpart.control(cp=.001, minbucket=3))
# plotmo(rpart, do.par=FALSE, pt.col=2, main="rpart", cex=.7, pt.cex=.2, grid.col=TRUE, trace=-1)
# # plotres(rpart)
#
# cat("degree2 comparison to glm and rpart: weight=2\n")
# weight <- 2
# w <- c(rep_len(1, 2 * n + 1), rep_len(weight, 2 * n + 1))
# aw201 <- earth(y~x1+x2, data=data, weights=w, degree=2)
# plotmo(aw201, do.par=FALSE, pt.col=2, cex=.7, pt.cex=.2, grid.col=TRUE, trace=-1, persp.ticktype="d")
# mtext("comparison to glm and rpart, degree2, weight 2\nleft side earth, right side gam200", outer=TRUE, line=2)
# gamw201 <- gam(y~s(x1, 7)+s(x2, 7)+s(x1, 7)*s(x2, 7), data=data, weights=w)
# plotmo(gamw201, do.par=FALSE, pt.col=2, cex=.7, pt.cex=.2, grid.col=TRUE, trace=-1, all2=TRUE, persp.ticktype="d")
# rpart <- rpart(y~x1, data=data, method="anova", control=rpart.control(cp=.001), weights=w)
# plotmo(rpart, do.par=FALSE, pt.col=2, main="", cex=.7, pt.cex=.2, grid.col=TRUE, trace=-1)
#
# cat("degree2 comparison to glm and rpart: weight=10\n")
# weight <- 10
# w <- c(rep_len(1, 2 * n + 1), rep_len(weight, 2 * n + 1))
# aw201 <- earth(y~x1+x2, data=data, weights=w, degree=2)
# plotmo(aw201, do.par=FALSE, pt.col=2, cex=.7, pt.cex=.2, grid.col=TRUE, trace=-1, persp.ticktype="d")
# mtext("comparison to glm and rpart, degree2, weight 10\nleft side earth, right side gam200", outer=TRUE, line=2)
# gamw201 <- gam(y~s(x1, 7)+s(x2, 7)+s(x1, 7)*s(x2, 7), data=data, weights=w)
# plotmo(gamw201, do.par=FALSE, pt.col=2, main="gam200", cex=.7, pt.cex=.2, grid.col=TRUE, trace=-1, all2=TRUE, persp.ticktype="d")
# rpart <- rpart(y~x1, data=data, method="anova", control=rpart.control(cp=.001), weights=w)
# plotmo(rpart, do.par=FALSE, pt.col=2, main="", cex=.7, pt.cex=.2, grid.col=TRUE, trace=-1)

# test bug fix for bug reported by damien georges (required adding check for "(weights)" to get.namesx)
set.seed(2016)
n <- 100
x1 <- factor(sample(c("A", "B", "C"), n, replace = TRUE)) # factorial variable
x2 <- runif(n) # continuous variable
x3 <- rnorm(n) # continuous variable
y <- factor(ifelse((as.numeric(x1) + x2 + x3) / mean(as.numeric(x1) + x2 + x3) > .8, "yes", "no"))
dat <- data.frame(y=y, x1=x1, x2=x2, x3=x3)

a <- earth(formula=y ~ x1 + x2 + x3, data=dat, glm=list(family=binomial))
print(summary(a))
yhat <- predict(a, dat[, c('x1', 'x2', 'x3')], type='response')

w <- rep(1, n) # vector of equal weights
aw <- earth(formula=y ~ x1 + x2 + x3, data=dat, glm=list(family=binomial), weight=w)
print(summary(aw))
yhatw <- predict(aw, dat[, c('x1', 'x2', 'x3')], type='response')
stopifnot(identical(yhat, yhat))
check.models.equal(a, aw)

w <- rep(1, n) # vector of equal weights
aw.force <- earth(formula=y ~ x1 + x2 + x3, data=dat, glm=list(family=binomial), weight=w, Force.weights=TRUE)
print(summary(aw.force))
yhatw <- predict(aw.force, dat[, c('x1', 'x2', 'x3')], type='response')
stopifnot(identical(yhat, yhat))
check.earth.lm.models.equal(a, aw.force)

cat("---check Scale.y-------------------------------------------\n")

xxx <- 1:9
yyy <- 1:9
yyy[3] <- 9
datxy <- data.frame(x=xxx, y=yyy)
colnames(datxy) <- c("xxx", "yyy")

mod1 <- earth(yyy~., datxy, Scale.y=FALSE)
mod2 <- earth(yyy~., datxy, Scale.y=TRUE)
check.models.equal(mod1, mod2, "mod1, mod2", newdata=dataxy[3,])

mod3 <- earth(yyy~., datxy, weights=weights, Scale.y=FALSE)
mod4 <- earth(yyy~., datxy, weights=weights, Scale.y=TRUE)
check.models.equal(mod3, mod4, "mod3, mod4", newdata=dataxy[3,])

data(ozone1)

mod5 <- earth(O3~., ozone1, Scale.y=FALSE)
mod6 <- earth(O3~., ozone1, Scale.y=TRUE)
check.models.equal(mod5, mod6, "mod5, mod6", newdata=ozone1[3,])

# trace=2 so we see "Fixed rank deficient bx"
mod7 <- earth(O3~., ozone1, weights=sqrt(ozone1$O3), Scale.y=FALSE, trace=2)
mod8 <- earth(O3~., ozone1, weights=sqrt(ozone1$O3), Scale.y=TRUE,  trace=2)
check.models.equal(mod7, mod8, "mod7, mod8", newdata=ozone1[3,])

data(etitanic)

# nk=5 for speed
mod9  <- earth(survived~., etitanic, nk=5, weights=sqrt(etitanic$age), Scale.y=FALSE)
mod10 <- earth(survived~., etitanic, nk=5, weights=sqrt(etitanic$age), Scale.y=TRUE)
check.models.equal(mod9, mod10, "mod9, mod10", newdata=etitanic[2,])

# use nk=7 to minimize differences between code for weighted and unweighted models in earth.c
mod.O3vh    <- earth(O3+vh~wind+doy, ozone1, nk=7, Scale.y=FALSE, trace=1)
w1 <- rep(1, length.out=nrow(ozone1))
mod.O3vh.w1 <- earth(O3+vh~wind+doy, ozone1, nk=7, weights=w1, Force.weights=TRUE, Scale.y=FALSE, trace=1)
check.models.equal(mod.O3vh, mod.O3vh.w1, "mod.O3vh, mod.O3vh.w1", newdata=ozone1[2,])

w3 <- rep(3, length.out=nrow(ozone1))
mod.O3vh.w3 <- earth(O3+vh~wind+doy, ozone1, nk=7, weights=w3, Force.weights=TRUE, Scale.y=FALSE)
check.equal(mod.O3vh$grsq, mod.O3vh.w3$grsq)
check.equal(mod.O3vh$rsq, mod.O3vh.w3$rsq)
check.equal(mod.O3vh$coefficients, mod.O3vh.w3$coefficients)
# check.models.equal(mod.O3vh, mod.O3vh.w3, "(mod.O3vh, mod.O3vh.w3") # not exactly equal but close

mod.O3vh.Scaley    <- earth(O3+vh~wind+doy, ozone1, nk=7, Scale.y=TRUE, trace=0)
w1 <- rep(1, length.out=nrow(ozone1))
mod.O3vh.w1.Scaley <- earth(O3+vh~wind+doy, ozone1, nk=7, weights=w1, Force.weights=TRUE, Scale.y=TRUE)
check.models.equal(mod.O3vh.Scaley, mod.O3vh.w1.Scaley, "mod.O3vh.Scaley, mod.O3vh.w1.Scaley", newdata=ozone1[2,])

# multiple response models, Scale.y will be visible (i.e. models with different Scale.y will be different)
mod.O3vh        <- earth(O3+vh~wind+doy, ozone1, degree=2, Scale.y=FALSE)
print(mod.O3vh)
mod.O3vh.Scaley <- earth(O3+vh~wind+doy, ozone1, degree=2, Scale.y=TRUE)
print(mod.O3vh.Scaley)
rsq.diff        <- abs(mod.O3vh$rsq.per.response[1]        - mod.O3vh$rsq.per.response[2])
rsq.diff.Scaley <- abs(mod.O3vh.Scaley$rsq.per.response[1] - mod.O3vh.Scaley$rsq.per.response[2])
# Scale.y=TRUE for multiple response models should make the rsq for the two responses closer
# i.e. with Scale.y=TRUE, vh should not overwhelm O3 because vh has much bigger values
stopifnot(rsq.diff.Scaley < rsq.diff)

wO3 <- sqrt(ozone1$O3)
mod.O3vh.wO3        <- earth(O3+vh~wind+doy, ozone1, degree=2, weights=wO3, Scale.y=FALSE)
print(mod.O3vh.wO3)
mod.O3vh.wO3.Scaley <- earth(O3+vh~wind+doy, ozone1, degree=2, weights=wO3, Scale.y=TRUE)
print(mod.O3vh.wO3.Scaley)
rsq.diff.wO3        <- abs(mod.O3vh.wO3$rsq.per.response[1]        - mod.O3vh.wO3$rsq.per.response[2])
rsq.diff.wO3.Scaley <- abs(mod.O3vh.wO3.Scaley$rsq.per.response[1] - mod.O3vh.wO3.Scaley$rsq.per.response[2])
# Scale.y=TRUE for multiple response models should make the rsq for the two responses closer
stopifnot(rsq.diff.wO3.Scaley < rsq.diff.wO3)

# nk=5 for speed
mod11 <- earth(pclass~., etitanic, nk=5, weights=sqrt(etitanic$age), Scale.y=FALSE)
print(mod11)
mod12 <- earth(pclass~., etitanic, nk=5, weights=sqrt(etitanic$age), Scale.y=TRUE)
print(mod12)

source("test.epilog.R")
