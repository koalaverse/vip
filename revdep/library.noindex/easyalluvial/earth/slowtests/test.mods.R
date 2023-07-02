# test.mods.R: test earth's ability to build various models

source("test.prolog.R")
library(earth)
options(digits=4)

SHORTTEST <- TRUE # use TRUE for production testing against test.mods.Rout.save
TRACE <- 0
PRINT.DATA <- FALSE
FORCE.WEIGHTS <- FALSE
# GLOBAL.SEEDS <- 1:10
GLOBAL.SEEDS <- 1
COLLINEAR.TESTS <- TRUE
SUMMARY <- FALSE
PLOT <- FALSE
TIME <- FALSE
COMPARE_TO_WEIGHTED_MODEL <- FALSE
RANDOMFOREST <- FALSE
MARS <- TRUE
RPROF <- FALSE
if(SHORTTEST) {
    GLOBAL.SEEDS <- 1
    COLLINEAR.TESTS <- FALSE
    SUMMARY <- FALSE
    PLOT <- FALSE
    TIME <- FALSE
    COMPARE_TO_WEIGHTED_MODEL <- FALSE
    RANDOMFOREST <- FALSE
    # MARS <- FALSE
    RPROF <- FALSE
}
itest <- 0

test.rsqs.global <- nterms.global <- delta.rsqs.global <- nknots.global <- NULL
other.rsqs.global <- NULL
mars.rsqs.global <- mars.nterms.global <- NULL

printf <- function(format, ...) cat(sprint(format, ...)) # like c printf

sq <- function(x) x * x

sos <- function(x) sum(as.vector(x^2)) # sum of squares

test.mod <- function(func, x, xtest, collinear.x2, npreds, nk=NULL, degree=2, ...)
{
    itest <<- itest + 1
    # sanity checks
    stopifnot(collinear.x2 == 0 || collinear.x2 == 1)
    stopifnot(npreds >= 1, npreds <= 9)
    stopifnot(nk >= 1, nk <= 201)
    stopifnot(degree >= 1, degree <= 5)
    set.seed(1994 + global.seed + itest)
    x <- x[, 1:npreds, drop=FALSE]
    y <- func(x)
    nk <- if(is.null(nk)) min(200, max(20, 2 * ncol(x))) + 1 else nk
    if(length(GLOBAL.SEEDS) > 1)
        printf("global.seed %g ", global.seed)
    printf("TEST %-2g%s n %-3g p %-1g %-16.16s nk %-3g deg %-1g ",
        itest, if(collinear.x2) " colx2" else "",
        nrow(x), ncol(x), deparse(substitute(func)), nk, degree)
    gc()
    if(TIME) {
        # system.time adds quite a lot of time overhead (because of its calls to gc)
        earth.time <- system.time(mod <- earth(x, y, nk=nk, degree=degree,
                                  trace=TRACE, Force.weights=FORCE.WEIGHTS, ...))
        time.string <- sprint(" [time %5.3f]", earth.time[3])
    } else {
        mod <- earth(x, y, nk=nk, degree=degree,
                     trace=TRACE, Force.weights=FORCE.WEIGHTS, ...)
        time.string <- ""
    }
    ytest <- func(xtest)
    fitted <- predict(mod, xtest)
    stopifnot(length(fitted) == nrow(xtest))
    test.rsq <- 1 - sos(ytest - fitted) / sos(ytest - mean(ytest))
    if(TRACE > 0)
        printf("TEST %-2g n %-3g p %-1g %-16.16s nk %-2g degree %-2g ",
            itest, nrow(x), ncol(x), deparse(substitute(func)), nk, degree)
    if(mod$grsq < .3) { # all bets are off with a very low GRsq
        test.rsq  <- max(-.1, test.rsq)
        delta.rsq <- test.rsq - max(0, mod$grsq)
    } else
        delta.rsq <- test.rsq - mod$grsq

    extra.msg <- ""
    if(COMPARE_TO_WEIGHTED_MODEL && !FORCE.WEIGHTS) {
        # build a weighted model and print a message if significantly different
        modw <- earth(x, y, nk=nk, degree=degree,
                      trace=TRACE, Force.weights=TRUE, ...)
        fittedw <- predict(modw, xtest)
        test.rsqw <- 1 - sos(ytest - fittedw) / sos(ytest - mean(ytest))
        deltaw <- test.rsq - test.rsqw
        extra.msg <- sprint("%s grsqw % 4.2f test.rsqw % 4.2f deltaw % 4.2f%s",
                        extra.msg, modw$grsq, test.rsqw, deltaw,
                        if(abs(deltaw) > .5) "!" else "")
    }
    if(RANDOMFOREST) { # build a randomForest model
        require(randomForest)
        rf <- randomForest(x, y, ntree=1000)
        fitted.rf <- predict(rf, xtest)
        rsq.rf <- 1 - sos(ytest - fitted.rf) / sos(ytest - mean(ytest))
        other.rsqs.global  <<- c(other.rsqs.global, rsq.rf)
        delta <- test.rsq - rsq.rf
        extra.msg <- sprint("%s rsq.rf % 4.2f delta % 4.2f%s",
                               extra.msg, rsq.rf, delta,
                               if(abs(delta) > .5) "!" else "")
    }
    if(MARS) { # build an mda::mars model
        require(mda)
        mars <- mars(x, y, nk=nk, degree=degree)
        mars <- mars.to.earth(mars, trace=FALSE)
        fitted.mars <- predict(mars, xtest)
        rsq.mars <- 1 - sos(ytest - fitted.mars) / sos(ytest - mean(ytest))
        mars.rsqs.global   <<- c(mars.rsqs.global, rsq.mars)
        mars.nterms.global <<- c(mars.nterms.global, length(mars$selected.terms))
        delta <- test.rsq - rsq.mars
        extra.msg <- sprint("%s rsq.mars % 4.3f delta % 4.2f%s",
                               extra.msg, rsq.mars, delta,
                               if(abs(delta) > .5) "!" else "")
    }
    printf("nterms %-2g%s grsq % 4.2f test.rsq % 4.2f grsq-test.rsq % 5.2f%s%s%s\n",
        length(mod$selected.terms), time.string, mod$grsq, test.rsq, delta.rsq,
        if(delta.rsq < -.3) " baddelta"   else "",
        if(test.rsq  < -1)  " badtestrsq" else "",
        extra.msg)
    test.rsqs.global  <<- c(test.rsqs.global, test.rsq)
    nterms.global     <<- c(nterms.global, length(mod$selected.terms))
    delta.rsqs.global <<- c(delta.rsqs.global, delta.rsq)
    nknots.global     <<- c(nknots.global, length(unique(as.vector(mod$cuts))))
    if(SUMMARY) {
        print(summary(mod))
        printf("\n")
    }
    if(PRINT.DATA) {
        print(cbind(y, x))
        printf("\n")
    }
    if(PLOT ||
            # following is to always produce a plot so diffps ok in test.mods.bat
            (!interactive() && itest == 1 && nrow(x) == 100)) {
        caption <- sprint("TEST %g%s n %d p %d %-.20s nk %g deg %g grsq %.2f test.rsq %.2f",
                    itest, if(collinear.x2) " col.x2" else "", nrow(x), ncol(x),
                    deparse(substitute(func)), nk, degree, mod$grsq, test.rsq)
        # plotmo(mod, trace=-1, pt.col="red", pt.cex=.8, caption=caption,
        #        cex.caption=if(npreds<=2) .7 else .9)
        plotmo(mod, trace=-1, pt.col="red", pt.cex=.8, caption=caption,
               cex.caption=if(npreds<=2) .7 else .9, type2="im")
    }
    mod
}
ran <- function(n) runif(n, -1, 1)
# ran <- function(n) 2 * rnorm(n)

testn <- function(n, collinear.x2=FALSE)
{
    itest <<- 0
    max.ncol <- 10
    set.seed(2015 + global.seed + n)
    x <- matrix(ran(max.ncol * n), ncol=max.ncol)
    x <- x[order(x[,1]), , drop=FALSE] # sort first column for convenience
    if(collinear.x2)
        x[,2] <- x[,1] + .3 * rnorm(nrow(x))
    colnames(x) <- paste("x", 1:ncol(x), sep="")

    xtest <- matrix(ran(max.ncol * 1e4), ncol=max.ncol)
    if(collinear.x2)
        xtest[,2] <- xtest[,1] + .3 * rnorm(nrow(xtest))
    xtest <- xtest[order(xtest[,1]), , drop=FALSE]
    colnames(xtest) <- c(paste("x", 1:max.ncol, sep=""))

    univariate <- function(x)
    {
        x[,1] + .3 * rnorm(nrow(x))
    }
    test.mod(univariate, x, xtest, collinear.x2, npreds=1, degree=1)
    test.mod(univariate, x, xtest, collinear.x2, npreds=2, degree=2) # extra predictor

    bi <- function(x)
    {
        x[,1] + x[,2] + .3 * rnorm(nrow(x))
    }
    test.mod(bi, x, xtest, collinear.x2, npreds=2, degree=1)
    test.mod(bi, x, xtest, collinear.x2, npreds=2, degree=2)
    test.mod(bi, x, xtest, collinear.x2, npreds=3, degree=2) # extra predictor

    bi.interact <- function(x)
    {
        x[,1] + x[,2] + (x[,1] * x[,2]) + .3 * rnorm(nrow(x))
    }
    test.mod(bi.interact, x, xtest, collinear.x2, npreds=2, degree=1)
    test.mod(bi.interact, x, xtest, collinear.x2, npreds=2, degree=2)
    test.mod(bi.interact, x, xtest, collinear.x2, npreds=3, degree=2) # extra predictor

    bi.interact2 <- function(x)
    {
        x[,1] - x[,2] + (x[,1] * x[,2]) + .3 * rnorm(nrow(x))
    }
    test.mod(bi.interact2, x, xtest, collinear.x2, npreds=2, degree=1)
    test.mod(bi.interact2, x, xtest, collinear.x2, npreds=2, degree=2)
    test.mod(bi.interact2, x, xtest, collinear.x2, npreds=3, degree=2) # extra predictor

    bi.interact3 <- function(x)
    {
        x[,1] + x[,2] - .5 * (x[,1] * x[,2]) + .3 * rnorm(nrow(x))
    }
    test.mod(bi.interact3, x, xtest, collinear.x2, npreds=2, degree=1)
    test.mod(bi.interact3, x, xtest, collinear.x2, npreds=2, degree=2)
    test.mod(bi.interact3, x, xtest, collinear.x2, npreds=3, degree=2) # extra predictor
    printf("\n")

    tri <- function(x)
    {
        x[,1] + x[,2] - x[,3] + .1 * rnorm(nrow(x))
    }
    test.mod(tri, x, xtest, collinear.x2, npreds=3, degree=1)
    test.mod(tri, x, xtest, collinear.x2, npreds=3, degree=2)
    test.mod(tri, x, xtest, collinear.x2, npreds=4, degree=2) # extra predictor

    tri.interact <- function(x)
    {
        x[,1] - x[,2] + sin(x[,3]) + (x[,1] * x[,2]) + .2 * rnorm(nrow(x))
    }
    test.mod(tri.interact, x, xtest, collinear.x2, npreds=3, degree=1)
    test.mod(tri.interact, x, xtest, collinear.x2, npreds=3, degree=2)
    test.mod(tri.interact, x, xtest, collinear.x2, npreds=3, degree=3)

    # TODO this and next function often cause a negative test.rsq (even though grsq is high)
    tri.interact2 <- function(x)
    {
        x[,1] + x[,2] + sin(x[,3]) - (x[,1] * x[,2]) + .2 * rnorm(nrow(x))
    }
    test.mod(tri.interact2, x, xtest, collinear.x2, npreds=3, degree=1)
    test.mod(tri.interact2, x, xtest, collinear.x2, npreds=3, degree=2)
    test.mod(tri.interact2, x, xtest, collinear.x2, npreds=3, degree=3)

    tri.interact3 <- function(x)
    {
        x[,1] - x[,2] + sq(x[,3]) + (x[,1] * x[,2]) + .2 * rnorm(nrow(x))
    }
    test.mod(tri.interact3, x, xtest, collinear.x2, npreds=3, degree=1)
    test.mod(tri.interact3, x, xtest, collinear.x2, npreds=3, degree=2)
    test.mod(tri.interact3, x, xtest, collinear.x2, npreds=3, degree=3)

    tri.two.interacts <- function(x)
    {
        x[,1] + x[,2] - sq(x[,3]) + (x[,1] * x[,2]) + sq(x[,1] * sq(x[,3])) + .1 * rnorm(nrow(x))
    }
    test.mod(tri.two.interacts, x, xtest, collinear.x2, npreds=3, degree=1)
    test.mod(tri.two.interacts, x, xtest, collinear.x2, npreds=3, degree=2)
    printf("\n")

    sin.3.x1 <- function(x)
    {
        # curve looks like this   /\
        #                           \/
        sin(3 * x[,1])
    }
    test.mod(sin.3.x1, x, xtest, collinear.x2, npreds=1, nk=51, degree=1)
    test.mod(sin.3.x1, x, xtest, collinear.x2, npreds=2, nk=51, degree=1) # x2 is noise
    test.mod(sin.3.x1, x, xtest, collinear.x2, npreds=2, nk=51, degree=2)
    printf("\n")

    sin.5.x1 <- function(x)
    {
        # curve looks like this   \  /\
        #                          \/  \
        sin(5 * x[,1])
    }
    test.mod(sin.5.x1, x, xtest, collinear.x2, npreds=1, nk=51, degree=1)
    test.mod(sin.5.x1, x, xtest, collinear.x2, npreds=2, nk=51, degree=1) # x2 is noise
    test.mod(sin.5.x1, x, xtest, collinear.x2, npreds=2, nk=51, degree=2)
    printf("\n")

    if(n > 30) {
        sin.5.x1.noise <- function(x)
        {
            sin(5 * x[,1]) + .5 * rnorm(nrow(x))
        }
        test.mod(sin.5.x1.noise, x, xtest, collinear.x2, npreds=1, degree=1)
        test.mod(sin.5.x1.noise, x, xtest, collinear.x2, npreds=2, degree=1)
        test.mod(sin.5.x1.noise, x, xtest, collinear.x2, npreds=2, degree=2)
        test.mod(sin.5.x1.noise, x, xtest, collinear.x2, npreds=1, degree=1, nk=51)
        test.mod(sin.5.x1.noise, x, xtest, collinear.x2, npreds=2, degree=1, nk=51)
        test.mod(sin.5.x1.noise, x, xtest, collinear.x2, npreds=2, degree=2, nk=51)
        printf("\n")
    }
    if(n > 100) { # need many points because the function is so curvy
        sin.10.x1 <- function(x)
        {
            # curve looks like this   \  /\  /\  /\
            # (three humps)            \/  \/  \/  \
            sin(10 * x[,1])
        }
        test.mod(sin.10.x1, x, xtest, collinear.x2, npreds=1, degree=1)
        test.mod(sin.10.x1, x, xtest, collinear.x2, npreds=2, degree=1)
        test.mod(sin.10.x1, x, xtest, collinear.x2, npreds=2, degree=2)
        test.mod(sin.10.x1, x, xtest, collinear.x2, npreds=1, degree=1, nk=51)
        test.mod(sin.10.x1, x, xtest, collinear.x2, npreds=2, degree=1, nk=51)
        test.mod(sin.10.x1, x, xtest, collinear.x2, npreds=1, degree=2, nk=51)
        # even with thresh=0 here we still don't cover all curves, ditto for models below
        test.mod(sin.10.x1, x, xtest, collinear.x2, npreds=1, degree=1, nk=51, thresh=1e-5)
        test.mod(sin.10.x1, x, xtest, collinear.x2, npreds=2, degree=1, nk=51, thresh=1e-5)
        test.mod(sin.10.x1, x, xtest, collinear.x2, npreds=2, degree=2, nk=51, thresh=1e-5)
        printf("\n")
    }
    if(n > 100) {
        sin.10.x1.noise <- function(x)
        {
            sin(10 * x[,1]) + .5 * rnorm(nrow(x))
        }
        test.mod(sin.10.x1.noise, x, xtest, collinear.x2, npreds=1, degree=1)
        test.mod(sin.10.x1.noise, x, xtest, collinear.x2, npreds=2, degree=1)
        test.mod(sin.10.x1.noise, x, xtest, collinear.x2, npreds=2, degree=2)
        test.mod(sin.10.x1.noise, x, xtest, collinear.x2, npreds=1, degree=1, nk=51)
        test.mod(sin.10.x1.noise, x, xtest, collinear.x2, npreds=2, degree=1, nk=51)
        test.mod(sin.10.x1.noise, x, xtest, collinear.x2, npreds=1, degree=2, nk=51)
        test.mod(sin.10.x1.noise, x, xtest, collinear.x2, npreds=1, degree=1, nk=51, thresh=1e-5)
        test.mod(sin.10.x1.noise, x, xtest, collinear.x2, npreds=2, degree=1, nk=51, thresh=1e-5)
        test.mod(sin.10.x1.noise, x, xtest, collinear.x2, npreds=2, degree=2, nk=51, thresh=1e-5)
        printf("\n")
    }
    # commented out because need too many cases because the function is so curvy
    # if(n > 100) { # need many points because the function is so curvy
    #     sin.20.x1 <- function(x)
    #     {
    #         sin(20 * x[,1])
    #     }
    #     test.mod(sin.20.x1, x, xtest, collinear.x2, npreds=1, degree=1)
    #     test.mod(sin.20.x1, x, xtest, collinear.x2, npreds=1, degree=2)
    #     test.mod(sin.20.x1, x, xtest, collinear.x2, npreds=1, degree=1, nk=51)
    #     test.mod(sin.20.x1, x, xtest, collinear.x2, npreds=1, degree=2, nk=51)
    #     test.mod(sin.20.x1, x, xtest, collinear.x2, npreds=1, degree=1, nk=51, thresh=1e-5)
    #     test.mod(sin.20.x1, x, xtest, collinear.x2, npreds=1, degree=2, nk=51, thresh=1e-5)
    #     printf("\n")
    # }
    # if(n > 100) { # need many points because the function is so curvy
    #     sin.20.x1.noise <- function(x) # six humps
    #     {
    #         sin(20 * x[,1]) + .5 * rnorm(nrow(x))
    #     }
    #     test.mod(sin.20.x1.noise, x, xtest, collinear.x2, npreds=1, degree=1)
    #     test.mod(sin.20.x1.noise, x, xtest, collinear.x2, npreds=1, degree=2)
    #     test.mod(sin.20.x1.noise, x, xtest, collinear.x2, npreds=1, degree=1, nk=51)
    #     test.mod(sin.20.x1.noise, x, xtest, collinear.x2, npreds=1, degree=2, nk=51)
    #     test.mod(sin.20.x1.noise, x, xtest, collinear.x2, npreds=1, degree=1, nk=51, thresh=1e-5)
    #     test.mod(sin.20.x1.noise, x, xtest, collinear.x2, npreds=1, degree=2, nk=51, thresh=1e-5)
    #     printf("\n")
    # }
    sin.3.x1.plus.x2 <- function(x)
    {
        sin(3 * x[,1]) + x[,2]
    }
    test.mod(sin.3.x1.plus.x2, x, xtest, collinear.x2, npreds=2, degree=1)
    test.mod(sin.3.x1.plus.x2, x, xtest, collinear.x2, npreds=2, degree=2)
    printf("\n")

    # TODO this function tends to have the most rsq discrepancies with randomForest models
    sin.2.x1.times.x2 <- function(x)
    {
        sin(2 * x[,1]) * x[,2]
    }
    test.mod(sin.2.x1.times.x2, x, xtest, collinear.x2, npreds=2, degree=1)
    test.mod(sin.2.x1.times.x2, x, xtest, collinear.x2, npreds=2, degree=2)
    printf("\n")

    # TODO this and the next function seem to most often cause a big
    #      discrepancy between grsq and test.rsq
    cos.2.x1.times.x2 <- function(x) # cos(2 * x1) looks like /\
    {
        cos(2 * x[,1]) * x[,2]
    }
    test.mod(cos.2.x1.times.x2, x, xtest, collinear.x2, npreds=2, degree=1)
    test.mod(cos.2.x1.times.x2, x, xtest, collinear.x2, npreds=2, degree=2)
    printf("\n")

    cos.2.x1.times.x2.noise <- function(x)
    {
        cos(2 * x[,1]) * x[,2] + .3 * rnorm(nrow(x))
    }
    test.mod(cos.2.x1.times.x2.noise, x, xtest, collinear.x2, npreds=2, degree=1)
    test.mod(cos.2.x1.times.x2.noise, x, xtest, collinear.x2, npreds=2, degree=2)
    printf("\n")

    eqn56 <- function(x) # Friedman MARS paper equation 56 (note that this is additive)
    {
        0.1 * exp(4 * x[,1]) +
        4 / (1 + exp(-20 * (x[,2] - 0.5))) +
        3 * x[,3] +
        2 * x[,4] +
        x[,5]
    }
    test.mod(eqn56, x, xtest, collinear.x2, npreds=5, degree=1)
    test.mod(eqn56, x, xtest, collinear.x2, npreds=5, degree=2)
    test.mod(eqn56, x, xtest, collinear.x2, npreds=5, degree=3)
    test.mod(eqn56, x, xtest, collinear.x2, npreds=5, nk=99, degree=1)
    test.mod(eqn56, x, xtest, collinear.x2, npreds=5, nk=99, degree=2)
    test.mod(eqn56, x, xtest, collinear.x2, npreds=5, nk=99, degree=3)
    printf("\n")

    eqn56.extra.preds <- function(x)
    {
        eqn56(x)
    }
    test.mod(eqn56.extra.preds, x, xtest, collinear.x2, npreds=9, degree=1)
    test.mod(eqn56.extra.preds, x, xtest, collinear.x2, npreds=9, degree=2)
    if(n > 30)
        test.mod(eqn56.extra.preds, x, xtest, collinear.x2, npreds=9, degree=3)
    test.mod(eqn56.extra.preds, x, xtest, collinear.x2, npreds=9, degree=1, nk=99)
    test.mod(eqn56.extra.preds, x, xtest, collinear.x2, npreds=9, degree=2, nk=99)
    if(n > 30)
        test.mod(eqn56.extra.preds, x, xtest, collinear.x2, npreds=9, degree=3, nk=99)
    printf("\n")

    eqn56.noise <- function(x)
    {
        eqn56(x) + rnorm(nrow(x))
    }
    test.mod(eqn56.noise, x, xtest, collinear.x2, npreds=5, degree=1)
    test.mod(eqn56.noise, x, xtest, collinear.x2, npreds=5, degree=2)
    test.mod(eqn56.noise, x, xtest, collinear.x2, npreds=5, degree=3)
    test.mod(eqn56.noise, x, xtest, collinear.x2, npreds=5, nk=99, degree=1)
    # commented out the following because they are slow
    # test.mod(eqn56.noise, x, xtest, collinear.x2, npreds=5, nk=99, degree=2)
    # test.mod(eqn56.noise, x, xtest, collinear.x2, npreds=5, nk=99, degree=3)
    printf("\n")

    if(n > 30) {
        eqn56.noise.extra.preds <- function(x)
        {
            eqn56(x) + rnorm(nrow(x))
        }
        test.mod(eqn56.noise.extra.preds, x, xtest, collinear.x2, npreds=9, degree=1)
        test.mod(eqn56.noise.extra.preds, x, xtest, collinear.x2, npreds=9, degree=2)
        test.mod(eqn56.noise.extra.preds, x, xtest, collinear.x2, npreds=9, degree=3)
        test.mod(eqn56.noise.extra.preds, x, xtest, collinear.x2, npreds=9, degree=1, nk=99)
        # commented out the following because they are slow
        # test.mod(eqn56.noise.extra.preds, x, xtest, collinear.x2, npreds=9, degree=2, nk=99)
        # test.mod(eqn56.noise.extra.preds, x, xtest, collinear.x2, npreds=9, degree=3, nk=99)
        printf("\n")
    }
    # force linpreds in 1 and 2 degree terms
    test.mod(eqn56, x, xtest, collinear.x2, npreds=5, linpreds=c("^x1$","x3","5"))
    test.mod(eqn56, x, xtest, collinear.x2, npreds=5, linpreds=c(3,5))

    # check symmetry by using negative of eqn56 (may not be completely symmetric)
    neg.eqn56 <- function(x)
    {
        -eqn56(x)
    }
    test.mod(neg.eqn56, x, xtest, collinear.x2, npreds=5, linpreds=c(3,5))
    printf("\n")

    five.preds <- function(x) # x1 and x2, and x3 and x4 interact
    {
        y <- 0
        for (i in 1:5)
            y <- y + sin(2 * x[,i])
        y + x[,1] * cos(4 * x[,2]) + (x[,3]-2) * x[,4]
    }
    test.mod(five.preds, x, xtest, collinear.x2, npreds=5, degree=1)
    test.mod(five.preds, x, xtest, collinear.x2, npreds=5, degree=2)
    test.mod(five.preds, x, xtest, collinear.x2, npreds=5, degree=3)
    test.mod(five.preds, x, xtest, collinear.x2, npreds=5, degree=1, nk=51)
    # commented out the following because they are slow
    # test.mod(five.preds, x, xtest, collinear.x2, npreds=5, degree=2, nk=51)
    # test.mod(five.preds, x, xtest, collinear.x2, npreds=5, degree=3, nk=51)
    printf("\n")

    if(n > 30) {
        five.preds.noise <- function(x)
        {
            five.preds(x) + .3 * rnorm(nrow(x))
        }
        test.mod(five.preds.noise, x, xtest, collinear.x2, npreds=5, degree=1)
        test.mod(five.preds.noise, x, xtest, collinear.x2, npreds=5, degree=2)
        test.mod(five.preds.noise, x, xtest, collinear.x2, npreds=5, degree=3)
        test.mod(five.preds.noise, x, xtest, collinear.x2, npreds=5, degree=1, nk=51)
        # commented out the following because they are slow
        # test.mod(five.preds.noise, x, xtest, collinear.x2, npreds=5, degree=2, nk=51)
        # test.mod(five.preds.noise, x, xtest, collinear.x2, npreds=5, degree=3, nk=51)
        printf("\n")
    }
    pure.noise <- function(x)
    {
        rnorm(nrow(x))
    }
    test.mod(pure.noise, x, xtest, collinear.x2, npreds=1, degree=1)
    test.mod(pure.noise, x, xtest, collinear.x2, npreds=2, degree=1)
    test.mod(pure.noise, x, xtest, collinear.x2, npreds=2, degree=2)
    if(n < 100) {
        cat("Skipping further tests because n < 100\n\n")
        return(invisible())
    }
    test.mod(pure.noise, x, xtest, collinear.x2, npreds=1, degree=1, nk=51)
    test.mod(pure.noise, x, xtest, collinear.x2, npreds=2, degree=2, nk=51)
    test.mod(pure.noise, x, xtest, collinear.x2, npreds=2, degree=2, nk=51)
    test.mod(pure.noise, x, xtest, collinear.x2, npreds=5, degree=1)
    test.mod(pure.noise, x, xtest, collinear.x2, npreds=5, degree=2)
    test.mod(pure.noise, x, xtest, collinear.x2, npreds=5, degree=1, nk=51)
    # commented out the following because it is slow
    # test.mod(pure.noise, x, xtest, collinear.x2, npreds=5, degree=2, nk=51)
    printf("\n")

    if(n > 100) { # need many points (Fast MARS paper uses 400 and 800 for robot.arm)
        robot.arm <- function(x) # Friedman Fast MARS paper
        {
            l1     <- x[,1]
            l2     <- x[,2]
            theta1 <- x[,3]
            theta2 <- x[,4]
            phi    <- x[,5]

            x1 <- l1 * cos(theta1) - l2 * cos(theta1 + theta2) * cos(phi)
            y <-  l1 * sin(theta1) - l2 * sin(theta1 + theta2) * cos(phi)
            z <-  l2 * sin(theta2) * sin(phi)

            sqrt(x1^2 + y^2 + z^2)
        }
        x[,1] <- (x[,1] + 1) / 2    # l1  0..1
        x[,2] <- (x[,2] + 1) / 2    # l2  0..1
        x[,3] <- pi * (x[,3] + 1)   # theta1
        x[,4] <- pi * (x[,4] + 1)   # theta2
        x[,5] <- pi * x[,5] / 2     # phi
        colnames(x) <- c("l1", "l2", "theta1", "theta2", "phi", paste("x", 6:ncol(x), sep=""))

        xtest[,1] <- (xtest[,1] + 1) / 2    # l1  0..1
        xtest[,2] <- (xtest[,2] + 1) / 2    # l2  0..1
        xtest[,3] <- pi * (xtest[,3] + 1)   # theta1
        xtest[,4] <- pi * (xtest[,4] + 1)   # theta2
        xtest[,5] <- pi * xtest[,5] / 2     # phi
        colnames(xtest) <- c("l1", "l2", "theta1", "theta2", "phi", paste("x", 6:ncol(x), sep=""))

            test.mod(robot.arm, x, xtest, collinear.x2, npreds=5, nk=51, degree=3)
            test.mod(robot.arm, x, xtest, collinear.x2, npreds=5, nk=99, degree=2)
            test.mod(robot.arm, x, xtest, collinear.x2, npreds=5, nk=99, degree=3)
            test.mod(robot.arm, x, xtest, collinear.x2, npreds=5, nk=99, degree=5)
            printf("\n")
    }
    if(n > 30) { # need many points (Meinshausen paper uses 1000)
        sin.sin <- function(x) # from Meinshausen "Node Harvest" paper
        {
            sin(pi * (x[,1] + 1)) * sin(pi * (x[,2] + 1))
        }
        # thresh=.0001 else get intercept only model
        test.mod(sin.sin, x, xtest, collinear.x2, npreds=2, degree=2, nk=99, thresh=.0001)
        test.mod(sin.sin, x, xtest, collinear.x2, npreds=4, degree=2, nk=99, thresh=.0001) # extra noise predictors
        printf("\n")
    }
    if(n > 100) { # need many points (Meinshausen paper uses 1000)
        sin.sin.noise <- function(x)
        {
            # we use less noise than the paper because we only have a max of 300 points
            sin(pi * x[,1]) * sin(pi * x[,2]) + rnorm(nrow(x), sd=.25)
        }
        test.mod(sin.sin.noise, x, xtest, collinear.x2, npreds=2, degree=2, nk=99, thresh=.0001)
        test.mod(sin.sin.noise, x, xtest, collinear.x2, npreds=4, degree=2, nk=99, thresh=.0001) # extra noise predictors
        printf("\n")
    }
    invisible()
}
my.summary <- function(x)
{
    q <- stats::quantile(x, probs = c(0, .01, .05, .1, .5, .9, 1))
    q <- c(q[1:4], mean(x), q[5:7])
    q <- as.numeric(sprint("%.3f", q))
    names(q) <- c("min", "1%", "5%", "10%", "mean", "median", "95%", "max")
    q
}
start.time <- proc.time()
global.seed <- GLOBAL.SEEDS[1]
cat("begin GLOBAL.SEEDS ", GLOBAL.SEEDS, " FORCE.WEIGHTS ", FORCE.WEIGHTS, "\n", sep="")
if(RPROF)
    Rprof("Rprof.out")
if(SHORTTEST) {
    testn(100)
} else for(global.seed in GLOBAL.SEEDS) {
    testn(30)
    testn(100)
    testn(300)
    if(COLLINEAR.TESTS)
        testn(100, collinear.x2=TRUE) # collinear.x2 preds expose the need for Adjust.endspan
}
if(RPROF) {
    Rprof(NULL)
    print(summaryRprof())
}
cat("end GLOBAL.SEEDS ", GLOBAL.SEEDS, " FORCE.WEIGHTS ", FORCE.WEIGHTS,
    " COLLINEAR.TESTS ", COLLINEAR.TESTS, "\n", sep="")
printf("test.rsq (bigger is better):\n")
print(my.summary(test.rsqs.global))
printf("grsq-test.rsq (closest to zero is best, but positive is better than negative):\n")
print(my.summary(delta.rsqs.global))
# printf("%.3f ", my.summary(delta.rsqs.global)); printf("\n")
printf("nterms (smaller is better):\n")
print(my.summary(nterms.global))
printf("nknots (smaller is better):\n")
print(my.summary(nknots.global))
if(!is.null(other.rsqs.global)) {
    printf("rf.rsq:\n")
    print(my.summary(other.rsqs.global))
}
if(!is.null(mars.rsqs.global)) {
    printf("mars.rsq:\n")
    print(my.summary(mars.rsqs.global))
    printf("mars.nterms:\n")
    print(my.summary(mars.nterms.global))
}
if(TIME)
    printf("[testn time %.3f]\n", (proc.time() - start.time)[3])
source("test.epilog.R")
