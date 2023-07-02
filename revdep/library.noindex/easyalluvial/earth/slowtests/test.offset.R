# test.offset.R

source("test.prolog.R")
library(earth)

almost.equal <- function(x, y, max=1e-8)
{
    stopifnot(max >= 0 && max <= .01)
    length(x) == length(y) && max(abs(x - y)) < max
}
# check that earth model matches lm model in all essential details
check.earth.matches.lm <- function(earth, lm, newdata=data[c(3,1,9),],
                                   check.coef.names=TRUE,
                                   check.casenames=TRUE,
                                   max=1e-8,
                                   max.residuals=1e-8)
{
    check.names <- function(earth.names, lm.names)
    {
        if(check.casenames &&
        # lm always adds rownames even if "1", "2", "3": this seems
        # wasteful and not particulary helpful, so earth doesn't do
        # this, hence the first !isTRUE(all.equal) below
           !isTRUE(all.equal(lm.names, paste(1:length(lm.names)))) &&
           !isTRUE(all.equal(earth.names, lm.names))) {
            print(earth.names)
            print(lm.names)
            stop(deparse(substitute(earth.names)), " != ",
                 deparse(substitute(lm.names)))
        }
    }
    cat0("check ", deparse(substitute(earth)), " vs ",
         deparse(substitute(lm)), "\n")

    # sort is needed because earth may reorder predictors based in importance
    stopifnot(almost.equal(sort(coef(earth)), sort(coef(lm)), max=max))
    if(check.coef.names)
        stopifnot(identical(sort(names(coef(earth))), sort(names(coef(lm)))))

    stopifnot(length(earth$coefficients) == length(lm$coefficients))
    stopifnot(almost.equal(sort(earth$coefficients), sort(lm$coefficients), max=max))

    stopifnot(length(earth$residuals) == length(lm$residuals))
    stopifnot(almost.equal(earth$residuals, lm$residuals, max=max.residuals))

    stopifnot(length(earth$fitted.values) == length(lm$fitted.values))
    stopifnot(almost.equal(earth$fitted.values, lm$fitted.values, max=max))

    stopifnot(almost.equal(fitted(earth), fitted(lm), max=max))
    if(!is.null(names(fitted(earth))) && !is.null(names(fitted(lm))))
        check.names(names(fitted(earth)), names(fitted(lm)))
    stopifnot(almost.equal(residuals(earth), residuals(lm), max=max.residuals))
    if(!is.null(names(residuals(earth))) && !is.null(names(residuals(lm))))
        check.names(names(residuals(earth)), names(residuals(lm)))

    predict.earth <- predict(earth)
    predict.lm    <- predict(lm)
    stopifnot(almost.equal(predict.earth, predict.lm, max=max))
    if(!is.null(names(predict.earth)) && !is.null(names(predict.lm)))
        check.names(names(predict.earth), names(predict.lm))

    predict.earth <- predict(earth, newdata=newdata)
    predict.lm    <- predict(lm, newdata=newdata)
    stopifnot(almost.equal(predict.earth, predict.lm, max=max))
    if(!is.null(names(predict.earth)) && !is.null(names(predict.lm)))
        check.names(names(predict.earth), names(predict.lm))
    # we calculate earth rsq manually to match the rsq technique of lm
    # this is necessary to get the same rsq when an offset is used
    if(is.null(earth$offset)) {
        earth.rsq <- earth$rsq
        rss <- if (is.null(earth$weights))
                   sum(earth$residuals^2)
               else
                   sum(earth$weights * earth$residuals^2)
    } else {
        if (is.null(earth$weights)) {
            mss <- sum((earth$fitted.values - mean(earth$fitted.values))^2)
            rss <- sum(earth$residuals^2)
        } else {
            stopifnot(almost.equal(lm$weights, earth$weights, max=max))
            m <- sum(earth$weights * earth$fitted.values /sum(earth$weights))
            mss <- sum(earth$weights * (earth$fitted.values - m)^2)
            rss <- sum(earth$weights * earth$residuals^2)
        }
        earth.rsq <- mss / (mss + rss)
    }
    stopifnot(almost.equal(earth.rsq, summary(lm)$r.squared, max=max))

    # check internal consistency of earth model
    stopifnot(earth$gcv == earth$gcv[1])
    stopifnot(almost.equal(earth$rsq.per.response[1], earth$rsq, max=1e-15))
    stopifnot(almost.equal(earth$grsq.per.response[1], earth$grsq, max=1e-15))
    if(is.null(earth$weights))
        stopifnot(almost.equal(earth$rss.per.response, earth$rss, max=1e-10))
}
# check that earth-glm model matches glm model in all essential details
check.earth.matches.glm <- function(earth, glm, newdata=data[c(3,1,9),],
                                   check.coef.names=TRUE,
                                   check.casenames=FALSE,
                                   max=1e-8,
                                   max.residuals=1e-8)
{
    check.names <- function(earth.names, glm.names)
    {
        if(check.casenames &&
        # glm always adds rownames even if "1", "2", "3": this seems
        # wasteful and not particulary helpful, so earth doesn't do
        # this, hence the first !isTRUE(all.equal) below
           !isTRUE(all.equal(glm.names, paste(1:length(glm.names)))) &&
           !isTRUE(all.equal(earth.names, glm.names))) {
            print(earth.names)
            print(glm.names)
            stop(deparse(substitute(earth.names)), " != ",
                 deparse(substitute(glm.names)))
        }
    }
    cat0("check ", deparse(substitute(earth)), " vs ",
         deparse(substitute(glm)), "\n")

    # sort is needed because earth may reorder predictors based in importance
    earth.glm <- earth$glm.list[[1]]
    stopifnot(!is.null(earth.glm))
    stopifnot(almost.equal(sort(coef(earth.glm)), sort(coef(glm)), max=max))
    if(check.coef.names)
        stopifnot(identical(sort(names(coef(earth.glm))), sort(names(coef(glm)))))

    stopifnot(length(earth.glm$coefficients) == length(glm$coefficients))
    stopifnot(almost.equal(sort(earth.glm$coefficients), sort(glm$coefficients), max=max))

    stopifnot(length(earth.glm$residuals) == length(glm$residuals))
    stopifnot(almost.equal(earth.glm$residuals, glm$residuals, max=max))

    stopifnot(length(earth.glm$fitted.values) == length(glm$fitted.values))
    stopifnot(almost.equal(earth.glm$fitted.values, glm$fitted.values, max=max))

    stopifnot(almost.equal(fitted(earth.glm), fitted(glm), max=max))
    if(!is.null(names(fitted(earth.glm))) && !is.null(names(fitted(glm))))
        check.names(names(fitted(earth.glm)), names(fitted(glm)))

    stopifnot(almost.equal(residuals(earth.glm), residuals(glm), max=max.residuals))
    if(!is.null(names(residuals(earth.glm))) && !is.null(names(residuals(glm))))
        check.names(names(residuals(earth.glm)), names(residuals(glm)))

    stopifnot(almost.equal(residuals(earth, type="response"),     residuals(glm, type="response"), max=max.residuals))
    stopifnot(almost.equal(residuals(earth, type="glm.response"), residuals(glm, type="response"), max=max.residuals))
    stopifnot(almost.equal(residuals(earth, type="deviance"),     residuals(glm, type="deviance"), max=max.residuals))
    stopifnot(almost.equal(residuals(earth, type="glm.pearson"),  residuals(glm, type="pearson"), max=max.residuals))
    stopifnot(almost.equal(residuals(earth, type="glm.working"),  residuals(glm, type="working"), max=max.residuals))
    # commented out because partial residuals don't match (because factors are expanded differently?)
    # stopifnot(almost.equal(residuals(earth, type="glm.partial"),  residuals(glm, type="partial"), max=max.residuals))

    # predict without newdata
    predict.glm    <- predict(glm)
    predict.earth <- predict(earth)
    stopifnot(almost.equal(predict.earth, predict.glm, max=max))
    if(!is.null(names(predict.earth)) && !is.null(names(predict.glm)))
        check.names(names(predict.earth), names(predict.glm))

    # predict type=default
    predict.glm   <- predict(glm, newdata=newdata)
    predict.earth <- predict(earth, newdata=newdata)
    stopifnot(almost.equal(predict.earth, predict.glm, max=max))
    if(!is.null(names(predict.earth)) && !is.null(names(predict.glm)))
        check.names(names(predict.earth), names(predict.glm))

    # predict type="response"
    predict.glm.response   <- predict(glm, newdata=newdata, type="response")
    predict.earth.response <- predict(earth, newdata=newdata, type="response")
    if(!is.null(names(predict.earth)) && !is.null(names(predict.glm)))
        check.names(names(predict.earth), names(predict.glm))
    stopifnot(almost.equal(predict.earth.response, predict.glm.response, max=max))
    if(!is.null(names(predict.earth.response)) && !is.null(names(predict.glm.response)))
        check.names(names(predict.earth.response), names(predict.glm.response))

    # predict type="link"
    predict.earth.link <- predict(earth, newdata=newdata, type="link")
    predict.glm.link   <- predict(glm, newdata=newdata, type="link")
    stopifnot(almost.equal(predict.earth.link, predict.glm.link, max=max))
    if(!is.null(names(predict.earth)) && !is.null(names(predict.lm)))
        check.names(names(predict.earth), names(predict.glm))

    # check internal consistency of earth model
    stopifnot(earth$gcv == earth$gcv[1])
    stopifnot(almost.equal(earth$rsq.per.response[1], earth$rsq, max=1e-15))
    stopifnot(almost.equal(earth$grsq.per.response[1], earth$grsq, max=1e-15))
    if(is.null(earth$weights))
        stopifnot(almost.equal(earth$rss.per.response, earth$rss, max=1e-10))
}
devratio <- function(mod)
{
    if(is.null(mod$deviance))
        mod <- mod$glm.list[[1]]
    stopifnot(!is.null(mod))
    stopifnot(!is.null(mod$deviance))
    stopifnot(!is.null(mod$null.deviance))
    sprint("devratio %.2f", 1 - mod$deviance / mod$null.deviance)
}
print.devratio <- function(s, mod)
{
    printf("%-22s %s\n", s, devratio(mod))
}
#------------------------------------------------------------------------------
# linear model

n <- 100
set.seed(2019)
x1 <- ((1:n) + runif(n, min=0, max=10)) / n
set.seed(2019)
x2 <- ((1:n) + runif(n, min=0, max=10)) / n
y <- 3 * x1 + rnorm(n)

myoffset <- (1:n) / n
data <- data.frame(y=y, x1=x1, myoffset=myoffset)

lm.weights <- lm(y ~ x1, data=data, weights=sin(myoffset))
earth.weights <- earth(y ~ x1, data=data, weights=sin(myoffset),
                linpreds=TRUE, thresh=0, penalty=-1)
check.earth.matches.lm(earth.weights, lm.weights)

myoffset <- (1:n) / n
data <- data.frame(y=y, x1=x1, myoffset=myoffset)
lm4 <- lm(y ~ x1 + offset(myoffset), data=data)
earth4 <- earth(y ~ x1 + offset(myoffset), data=data,
                linpreds=TRUE, thresh=0, penalty=-1)
check.earth.matches.lm(earth4, lm4)
cat("==print(earth4)==\n")
print(earth4)
cat("==summary(earth4)==\n")
print(summary(earth4))
cat("==summary(earth4, details=TRUE)==\n")
print(summary(earth4, details=TRUE))

par(mfrow=c(4, 2), mar=c(3, 3, 3, 1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 5, 0))
set.seed(2019)
plotmo(lm4, trace=0, pt.col=2, do.par=FALSE)
mtext(
    "row1: lm4\nrow2: earth4\nrow3: lm4   grid.levels=list(myoffset=-3)\nrow4: earth4   grid.levels=list(myoffset=-3)",
    outer=TRUE, cex=.8)
set.seed(2019)
plotmo(earth4, trace=0, pt.col=2, do.par=FALSE)
empty.plot()
set.seed(2019)
plotmo(lm4, trace=0, pt.col=2, do.par=FALSE, grid.levels=list(myoffset=-3))
set.seed(2019)
plotmo(earth4, trace=0, pt.col=2, do.par=FALSE, grid.levels=list(myoffset=-3))
par(org.par)

plotres(lm4)
plotres(earth4)

# linear model with weights and offset

lm4.weights <- lm(y ~ x1 + offset(exp(myoffset)), data=data, weights=sin(myoffset))
earth4.weights <- earth(y ~ x1 + offset(exp(myoffset)), data=data, weights=sin(myoffset),
                linpreds=TRUE, thresh=0, penalty=-1)
check.earth.matches.lm(earth4.weights, lm4.weights)
print(earth4.weights)
print(summary(earth4.weights))

#------------------------------------------------------------------------------
# error handling

data <- data.frame(y=y, x1=x1)
expect.err(try(earth(y ~ x1 + offset(myoffset), data=data)), "the offset variable 'myoffset' in 'offset(myoffset)' must be in the data")
expect.err(try(earth(y ~ x1 + offset(myoffset))), "if an offset is specified in the formula, the 'data' argument must be used")

data <- data.frame(y=y, x1=x1, offset0=rep(0, length.out=n), offset1=rep(1, length.out=n))
expect.err(try(earth(y ~ x1 + offset(offset0) + offset(offset1), data=data)), "only one offset is allowed")

#------------------------------------------------------------------------------
# poisson model with and without linear predictors

library(MASS)
data(Insurance)
Ins <- Insurance
Ins$Claims[Ins$Claims > 100] <- 100
Ins$day <- (1:nrow(Insurance)) / nrow(Insurance) # non linear term (like a seasonal effect)
Ins$Claims <- round(Ins$Claims * (1 + sin(2 * pi * Ins$day)))
pois <- glm(Claims ~ offset(log(Holders)) + Group + Age + day,
            data = Ins, family = poisson)
earth.pois.linpreds <- earth(Claims ~ offset(log(Holders)) + Group + Age + day,
                             data = Ins, glm=list(family = poisson),
                             linpreds=TRUE, thresh=0, penalty=-1)

stopifnot(isTRUE(all.equal(coef(earth.pois.linpreds), coefficients(earth.pois.linpreds))))
stopifnot(isTRUE(all.equal(coef(earth.pois.linpreds, type="glm"), coefficients(earth.pois.linpreds, type="glm"))))
stopifnot(isTRUE(all.equal(coef(earth.pois.linpreds, type="earth"), coefficients(earth.pois.linpreds, type="earth"))))
stopifnot(identical(names(coef(earth.pois.linpreds)), rownames(earth.pois.linpreds$coefficients)))
stopifnot(identical(names(coef(earth.pois.linpreds)), rownames(earth.pois.linpreds$glm.coefficients)))
stopifnot(identical(names(coef(earth.pois.linpreds, type="glm")), rownames(earth.pois.linpreds$glm.coefficients)))
stopifnot(max(abs(coef(earth.pois.linpreds) - earth.pois.linpreds$glm.coefficients)) == 0)
stopifnot(max(abs(coef(earth.pois.linpreds, type="response") - earth.pois.linpreds$glm.coefficients)) == 0)
stopifnot(max(abs(coef(earth.pois.linpreds, type="earth") - earth.pois.linpreds$coefficients)) == 0)
stopifnot(max(abs(coef(earth.pois.linpreds) - earth.pois.linpreds$glm.list[[1]]$coefficients)) == 0)
stopifnot(max(abs(coef(earth.pois.linpreds, type="glm") - earth.pois.linpreds$coefficients)) > 99)

check.earth.matches.glm(earth.pois.linpreds, pois, newdata=Ins[4:6,])
earth.pois <- earth(Claims ~ Group + Age  + day + offset(log(Holders)),
                    data = Ins, glm=list(family = poisson))
cat("==print(earth.pois)==\n")
print(earth.pois)
cat("==summary(earth.pois)==\n")
print(summary(earth.pois))
cat("==summary(earth.pois, details=TRUE)==\n")
print(summary(earth.pois, details=TRUE))
earth.pois.no.penalty <- earth(Claims ~ Group + Age  + day + offset(log(Holders)),
                               data = Ins, glm=list(family = poisson),
                               thresh=0, penalty=-1)
print.devratio("pois",                  pois)
print.devratio("earth.pois.linpreds",   earth.pois.linpreds$glm.list[[1]])
print.devratio("earth.pois",            earth.pois$glm.list[[1]])
print.devratio("earth.pois.no.penalty", earth.pois.no.penalty$glm.list[[1]])

par(mfrow=c(3, 4), mar=c(3, 3, 3, 1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 5, 0))
set.seed(2019)
plotmo(pois, trace=0, pt.col=2, do.par=FALSE, ylim=c(0,50))
mtext(sprint(
    "row1: pois (%s)\nrow2: earth.pois.linpreds (%s)\nrow3: earth.pois.linpreds(all1=TRUE)",
    devratio(pois), devratio(earth.pois.linpreds)),
    outer=TRUE, cex=.8)
set.seed(2019)
plotmo(earth.pois.linpreds, trace=0, pt.col=2, do.par=FALSE, ylim=c(0,50))
empty.plot()
set.seed(2019)
plotmo(earth.pois.linpreds, all1=T, trace=-1, pt.col=2, do.par=FALSE, ylim=c(0,50))
par(org.par)

plotres(pois, type="response", caption='pois, type="response"')
plotres(earth.pois.linpreds, type="response", caption='earth.pois.linpreds, type="response"')

par(mfrow=c(3, 4), mar=c(3, 3, 3, 1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 5, 0))
set.seed(2019)
plotmo(pois, trace=0, pt.col=2, do.par=FALSE, ylim=c(0,50), grid.levels=list(Holders=20))
mtext(
    "----- grid.levels=list(Holders=20)) -----\nrow1: pois\nrow2: earth.pois.linpreds\nrow3: earth.pois.linpreds(all1=TRUE)",
    outer=TRUE, cex=.8)
set.seed(2019)
plotmo(earth.pois.linpreds, trace=0, pt.col=2, do.par=FALSE, ylim=c(0,50), grid.levels=list(Holders=20))
empty.plot()
set.seed(2019)
plotmo(earth.pois.linpreds, all1=T, trace=-1, pt.col=2, do.par=FALSE, ylim=c(0,50), grid.levels=list(Holders=20))
par(org.par)

plotmo(earth.pois.linpreds, pmethod="partdep", do.par=2,
       caption=sprint("earth.pois.linpreds, pmethod=\"partdep\", %s", devratio(earth.pois.linpreds)))
plotmo(earth.pois.linpreds, pmethod="partdep", do.par=0,
       grid.levels=list(Age=">35"), degree1="day", main="day with Age=\">35\"")
par(org.par)
plotmo(earth.pois,          pmethod="partdep",
       caption=sprint("earth.pois, pmethod=\"partdep\", %s", devratio(earth.pois)))
plotmo(earth.pois.no.penalty, pmethod="partdep",
       caption=sprint("earth.pois.no.penalty, pmethod=\"partdep\", %s", devratio(earth.pois.no.penalty)))

#------------------------------------------------------------------------------
# poisson model with weights

Ins <- Insurance
Ins$Claims[Ins$Claims > 100] <- 100
Ins$day <- (1:nrow(Insurance)) / nrow(Insurance) # non linear term (like a seasonal effect)
Ins$Claims <- round(Ins$Claims * (1 + sin(2 * pi * Ins$day)))
weights <- 1:nrow(Ins)

pois.weights <- glm(Claims ~ Group + Age + day,
                    data = Ins, family = poisson, weights=weights)

earth.pois.linpreds.weights <- earth(Claims ~ Group + Age + day,
                                     data = Ins, glm=list(family = poisson),
                                     weights=weights,
                                     linpreds=TRUE, thresh=0, penalty=-1)
check.earth.matches.glm(earth.pois.linpreds.weights, pois.weights, newdata=Ins[1:3,])

#------------------------------------------------------------------------------
# poisson model with weights, some of which are zero

Ins <- Insurance
Ins$Claims[Ins$Claims > 100] <- 100
Ins$day <- (1:nrow(Insurance)) / nrow(Insurance) # non linear term (like a seasonal effect)
Ins$Claims <- round(Ins$Claims * (1 + sin(2 * pi * Ins$day)))
weights <- 1:nrow(Ins)
weights[4] <- 0
weights[8] <- 0

pois.weights.some.zero <- glm(Claims ~ Group + Age + day,
                    data = Ins, family = poisson, weights=weights)

earth.pois.linpreds.weights.some.zero <- earth(Claims ~ Group + Age + day,
                                     data = Ins, glm=list(family = poisson),
                                     weights=weights,
                                     linpreds=TRUE, thresh=0, penalty=-1)
check.earth.matches.glm(earth.pois.linpreds.weights.some.zero, pois.weights.some.zero, newdata=Ins[1:3,],
                        max=1e-5, max.residuals=1e-2) # TODO why does max.residuals have to be so big here?

plotres(pois.weights.some.zero, caption="pois.weights.some.zero")
plotres(earth.pois.linpreds.weights.some.zero, caption="earth.pois.linpreds.weights.some.zero")
plotmo(pois.weights.some.zero, caption="pois.weights.some.zero")
plotmo(earth.pois.linpreds.weights.some.zero, caption="earth.pois.linpreds.weights.some.zero")

#------------------------------------------------------------------------------
# multiple response models

data(trees)
tr <- trees
set.seed(2019)
tr$Vol2 <- tr$Volume + 10 * rnorm(nrow(tr))

earth10 <- earth(Volume ~ Girth + offset(log(Height)), data=tr,
                 linpreds=TRUE, thresh=0, penalty=-1)
lm10 <- lm(Volume ~ Girth + offset(log(Height)), data=tr)
check.earth.matches.lm(earth10, lm10, newdata=tr[c(3:5),])
cat("earth10:\n")
print(summary(earth10))

earth20 <- earth(Vol2 ~ Girth + offset(log(Height)), data=tr,
                 linpreds=TRUE, thresh=0, penalty=-1)
cat("earth20:\n")
print(summary(earth20))

earth30 <- earth(cbind(Volume, Vol2) ~ Girth + offset(log(Height)), data=tr,
                 linpreds=TRUE, thresh=0, penalty=-1)
cat("earth30:\n")
print(summary(earth30))

plotmo(lm10, all1=TRUE, pt.col=2)
plotmo(earth10, all1=TRUE, pt.col=2)
plotmo(earth20, all1=TRUE, pt.col=2)
plotmo(earth30, nresponse=1, all1=TRUE, pt.col=2)
plotmo(earth30, nresponse=2, all1=TRUE, pt.col=2)

plotres(lm10)
plotres(earth10)
plotres(earth20)
plotres(earth30, nresponse=2)
plotres(earth30, nresponse=1)

# multiple response pois model with weights (basic test)

Ins <- Insurance
Ins$Claims[Ins$Claims > 100] <- 100
Ins$day <- (1:nrow(Insurance)) / nrow(Insurance) # non linear term (like a seasonal effect)
Ins$Claims <- round(Ins$Claims * (1 + sin(2 * pi * Ins$day)))
Ins$Claims2 <- Insurance$Claims2 <- round(Insurance$Claims^1.5)
weights <- 1:nrow(Ins)
weights[4] <- 0
weights[8] <- 0

earth.pois.multiple.response <-
    earth(x=Insurance$Age, y=cbind(Insurance$Claims, Insurance$Claims2),
          trace=1, # Insurance$Age expands to x.L x.Q x.C
          glm=list(family = poisson), weights=weights)
cat("earth.pois.multiple.response:\n")
print(earth.pois.multiple.response)
cat("summary(earth.pois.multiple.response:\n")
print(summary(earth.pois.multiple.response))
plotmo(earth.pois.multiple.response, nresponse=1, pt.col=2)

# test update.earth with weights and offset

data(trees)
tr <- trees
set.seed(2019)
tr$Vol2 <- tr$Volume + 10 * rnorm(nrow(tr))
my.weights <- 1:nrow(tr)
my.weights[3] <- 0

earth30 <- earth(Volume ~ Girth + offset(log(Height)), data=tr,
                 linpreds=TRUE, thresh=0, penalty=-1)
lm30 <- lm(Volume ~ Girth + offset(log(Height)), data=tr)
check.earth.matches.lm(earth30, lm30, newdata=tr[c(3:5),])

lm31 <- lm(Volume ~ Girth, data=tr)
earth31 <- earth(Volume ~ Girth, data=tr,
                 linpreds=TRUE, thresh=0, penalty=-1)
earth31.offset <- update(earth31, formula.=Volume ~ Girth + offset(log(Height)))
check.earth.matches.lm(earth31.offset, lm30, newdata=tr[c(3:5),])
earth.nooffset <- update(earth31.offset, formula.=Volume ~ Girth)
check.earth.matches.lm(earth.nooffset, lm31, newdata=tr[c(3:5),])

lm31.weights <- lm(Volume ~ Girth, data=tr, weights=my.weights)
earth31.weights <- update(earth31, weights=my.weights)
# lower max is needed below because of zeros in my.weights
check.earth.matches.lm(earth31.weights, lm31.weights, newdata=tr[c(3:5),], max=1e-6, max.residuals=1e-6)

lm31.weights.offset <- lm(Volume ~ Girth + offset(log(Height)), data=tr, weights=my.weights)
earth31.weights.offset <- update(earth31.weights, formula=Volume ~ Girth + offset(log(Height)))
check.earth.matches.lm(earth31.weights.offset, lm31.weights.offset, newdata=tr[c(3:5),], max=1e-6, max.residuals=1e-6)
cat("earth31.weights.offset:\n")
print(summary(earth31.weights.offset))
cat("\nnearth31.weights.offset$modvars:\n")
print.default(earth31.weights.offset$modvars)

source("test.epilog.R")
