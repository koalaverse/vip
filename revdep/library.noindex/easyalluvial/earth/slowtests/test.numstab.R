# test.numstab.R: Expose any numerical instability of earth across platforms.
#
# This file was created by running earth and plotmo slowtests
# with earth on Win7 built with "--mfpmath=387" (instead of "-mtune=native"
# or "-mfpmath=sse -msse2").
# Differences between the output in the test suites from standard earth
# were collected and put into this file.
# So this code duplicates code in earth and plotmo slowtests.
# Most but not all differences were captured and put into this file.
# This file was originally created in in Oct 2020 for earth 5.3.0.

source("test.prolog.R")

library(earth)
library(mda)
data(ozone1)
data(trees)
data(etitanic)

cat("\n#=== from test.full.R ===========================================\n")
set.seed(2020)

PLOT <- TRUE                # TRUE to do plots too, FALSE for speed
options.old <- options()
options(warn=1) # print warnings as they occur

printh <- function(x, expect.warning=FALSE, max.print=0) # like print but with a header
{
    cat("===", deparse(substitute(x)), " ", sep="")
    if(expect.warning)
        cat(" expect warning -->")
    else if (NROW(x) > 1)
        cat("\n")
    if (max.print > 0)
        print(head(x, n=max.print))
    else
        print(x)
}

ozone.test <- function(itest, sModel, x, y, degree=2, nk=51,
                    plotit=PLOT, trace=0, smooth.col="red")
{
    fite <- earth(x, y, degree=degree, nk=nk, trace=trace)
    fitm <- mars(x, y, degree=degree, nk=nk)

    cat("itest",
        sprint("%-3d", itest),
        sprint("%-32s", sModel),
        "degree", sprint("%-2d", degree), "nk", sprint("%-3g", nk),
        "nTerms",  sprint("%-2d", sum(fite$selected.terms != 0)),
        "of", sprint("%-3d", nrow(fite$dirs)),
        "GRSq", sprint("%4.2g", fite$grsq),
        "GRSq ratio", fite$grsq/mars.to.earth(fitm)$grsq,
        "\n")
    caption <- paste("itest ", itest, ": ", sModel, " degree=", degree, " nk=", nk, sep="")
    printh(summary(fite))
    printh(summary(fite, style="bf"))
    if(plotit) {
        fitme <- mars.to.earth(fitm)
        plotmo(fite, caption=paste("NUMSTAB EARTH", caption), trace=-1)
        plotmo(fitme, caption=paste("MARS", caption), trace=-1)
        plot(fite, npoints=500, smooth.col=smooth.col, caption=paste("EARTH", caption), info=TRUE)
        plot(fitme, caption=paste("MARS", caption), info=TRUE)
        fitme <- update(fitme)  # generate model selection data
        plot.earth.models(list(fite, fitme), caption=paste(itest, ": Compare earth to mars ", sModel, sep=""))
    }
    fite
}
set.seed(2020)
data(ozone1)
attach(ozone1)
itest <- 1

set.seed(2020)
cat("--Expect warning from mda::mars: NAs introduced by coercion\n") # why do we get a warning?
x.global <- cbind(wind, exp(humidity))
y <- doy
# smooth.col is 0 else get loess errors
# trace==2 so we see "Fixed rank deficient bx by removing 2 terms, 7 terms remain"
ozone.test(itest, "doy ~ wind+exp(humidity)", x.global, y, degree=1, nk=21, smooth.col=0, trace=2)

# test Auto.linpreds with data sent in by a user
ndata <- matrix(data=c(
-0.0781, -0.6109, -0.216, -1.5172, 0.8184, -1.1242,
-0.0781, -0.5885, -0.216, -1.3501, 0.8184, -0.8703,
-0.0781, -0.5885, -0.216, -1.3501, 0.8184, -0.9549,
-0.0781, -0.5885, -0.216, -1.3501, 1.4136, -0.8703,
-2.5759, -0.5885, 1.1665, -1.3501, 2.0089, -0.9549,
-2.5759, -0.5885, 1.1665, -1.3501, 2.0089, -0.8703,
-0.0781, -0.4937, -0.216, -0.9949, -0.372, -1.0396,
-0.0781, -0.4463, -0.216, -0.8278, -0.372, -0.447,
-0.0781, -0.4463, -0.216, -0.8278, -0.372, -0.701,
-0.0781, -0.4463, -0.216, -0.8278, -0.372, -0.6163,
-0.0781, -0.4463, -0.216, -0.8278, 0.8184, -0.447,
-0.0781, -0.4463, -0.216, -0.8278, 0.8184, -0.6163,
-0.0781, -0.4463, 1.1665, -0.8278, 0.8184, -0.447,
-0.0781, -0.4379, 1.1665, 0.2585, -0.372, -0.1085,
-0.0781, -0.2147, 1.1665, 0.0496, -0.372, -0.1085,
-0.0781, -0.2147, -0.216, 0.2585, -0.372, -0.0238,
-0.0781, -0.1589, -0.216, 0.2585, -0.372, -0.1931,
-0.0781, -0.1589, -0.216, 0.2585, -0.372, -0.1085,
-0.0781, -0.1589, 1.1665, 0.2585, -0.372, -0.1931,
-0.0781, -0.1589, -0.216, 0.2585, 0.8184, -0.1085,
-0.0781, -0.1589, -0.216, 0.2585, 0.8184, 0.0608,
-0.0781, -0.1589, -0.216, 1.0942, 0.8184, -0.0238,
-0.0781, 0.0643, 1.1665, 1.0942, -0.372, 0.2301,
-0.0781, 0.0643, -0.216, 1.0942, -1.5624, 0.3148,
-0.0781, 0.0643, -0.216, 1.0942, -0.9672, 0.1455,
-0.0781, 0.0643, 1.1665, 1.4284, 0.2232, 0.4841,
-0.0781, 0.1563, -0.216, 1.0942, -0.372, 0.5687,
2.4197, 0.3432, -0.216, 1.0942, -1.5624, 1.0766,
-0.0781, 0.3432, -0.216, 1.0942, -1.5624, 1.1613,
-0.0781, 0.3432, 1.1665, 1.0942, 0.2232, 0.738,
2.4197, 2.7145, -2.9811, 1.0942, -1.5624, 2.5156,
2.4197, 4.3884, -2.9811, 1.0942, -1.5624, 3.5314),
ncol=6)
colnames(ndata) <- c("x1", "x2", "x3", "x4", "x5", "y")
ndata <- as.data.frame(ndata)

set.seed(2020)
cat("Auto.linpreds=TRUE pmethod=\"none\":\n")
# trace==2 so we see "Fixed rank deficient bx by removing terms"
# TODO why are we getting the rank deficient message?
auto.linpreds.true.pmethod.none <- earth(y~., data=ndata, degree=2, nk=21, trace=2, pmethod="none")
print(summary(auto.linpreds.true.pmethod.none, decomp="none"))
cat("\nAuto.linpreds=FALSE pmethod=\"none\":\n")
auto.linpreds.false.pmethod.none <- earth(y~., data=ndata, degree=2, nk=21, trace=2, Auto.linpreds=FALSE, pmethod="none")
print(summary(auto.linpreds.false.pmethod.none, decomp="none"))
stopifnot(isTRUE(all.equal(predict(auto.linpreds.true.pmethod.none), predict(auto.linpreds.false.pmethod.none))))

set.seed(2020)
cat("\nAuto.linpreds=TRUE:\n")
auto.linpreds.true <- earth(y~., data=ndata, degree=2, nk=21, trace=2)
print(summary(auto.linpreds.true, decomp="none"))
cat("\nAuto.linpreds=FALSE:\n")
auto.linpreds.false <- earth(y~., data=ndata, degree=2, nk=21, trace=2, Auto.linpreds=FALSE)
print(summary(auto.linpreds.false, decomp="none"))
# following fails because of different pruning because of different term count
# stopifnot(isTRUE(all.equal(predict(auto.linpreds.true), predict(auto.linpreds.false))))

cat("\n#=== from test.weights.R ===========================================\n")
set.seed(2020)

noise <- .01 * c(1,2,3,2,1,3,5,2,0)
data <- data.frame(x1=c(1,2,3,4,5,6,7,8,9), x2=c(1,2,3,3,3,6,7,8,9), y=(1:9)+noise)
data[5,] <- c(5, 5, 6)
colnames(data) <- c("x1", "x2", "y")

a21.noweights <- earth(y~., data=data, # no weights for comparison
                       minspan=1, endspan=1, penalty=-1, thresh=1e-8, trace=-1)
print(summary(a21.noweights))
weights <- c(1, 1, 1, 1, .5, 1, 1, 1, 1)
a10  <- earth(y~., data=data, weights=weights,
              minspan=1, endspan=1, penalty=-1, thresh=1e-8, trace=-1)
print(summary(a10))

cat("\n#=== from test.glm.R ===========================================\n")

cat("a12: compare family=gaussian to standard earth model with two responses\n\n")
a12 <- earth(cbind(etitanic$sex, (as.integer(etitanic$age)^2)) ~ ., data=etitanic, degree=2, glm=list(family="gaussian"), trace=4)
cat("\nsummary(a12, details=TRUE)\n\n", sep="")
print(summary(a12, details=TRUE))

cat("\n#=== from test.plotmo.R ===========================================\n")

# check various types of predictors with grid.func and ndiscrete

varied.type.data <- data.frame(
    y    = 1:13,
    num  = c(1, 3, 2, 3, 4, 5, 6, 4, 5, 6.5, 3, 6, 5), # 7 unique values (but one is non integral)
    int  = c(1L, 1L, 3L, 3L, 4L, 4L, 3L, 5L, 3L, 6L, 7L, 8L, 10L), # 8 unique values
    bool = c(F, F, F, F, F, T, T, T, T, T, T, T, T),
    date = as.Date(
           c("2018-08-01", "2018-08-02", "2018-08-03",
             "2018-08-04", "2018-08-05", "2018-08-06",
             "2018-08-07", "2018-08-08", "2018-08-08",
             "2018-08-08", "2018-08-10", "2018-08-11", "2018-08-11")),
    ord  = ordered(c("ord3", "ord3", "ord3",
                     "ord1", "ord2", "ord3",
                     "ord1", "ord2", "ord3",
                     "ord1", "ord1", "ord1", "ord1"),
                   levels=c("ord1", "ord3", "ord2")),
    fac  = as.factor(c("fac1", "fac1", "fac1",
                       "fac2", "fac2", "fac2",
                       "fac3", "fac3", "fac3",
                       "fac1", "fac2", "fac3", "fac3")),
    str  = c("str1", "str1", "str1", # will be treated like a factor
             "str2", "str2", "str2",
             "str3", "str3", "str3",
             "str3", "str3", "str3", "str3"))

varied.type.earth <- earth(y ~ ., data = varied.type.data, thresh=0, penalty=-1, trace=1)
print(summary(varied.type.earth))

cat("\n#=== from test.plotmo.args.R ===========================================\n")
set.seed(2020)

oz2 <- ozone1[1:40,]
set.seed(2015)
a <- earth(O3~temp+wind, dat=oz2, deg=2, nk=21, ncr=3, nfo=3, varmod.me="lm")
print(summary(a))
plotmo(a, caption.col=3, caption.font=2, grid.col="pink",
       level=.8, SHOWCALL=TRUE)

cat("\n#=== from test.plotmo3.R ===========================================\n")
set.seed(2020)

# basic tests of plotmo on abbreviated titanic data

get.tita <- function()
{
    tita <- etitanic
    pclass <- as.character(tita$pclass)
    # change the order of the factors so not alphabetical
    pclass[pclass == "1st"] <- "first"
    pclass[pclass == "2nd"] <- "class2"
    pclass[pclass == "3rd"] <- "classthird"
    tita$pclass <- factor(pclass, levels=c("class2", "classthird", "first"))
    # log age is so we have a continuous predictor even when model is age~.
    set.seed(2015)
    tita$logage <- log(tita$age) + rnorm(nrow(tita))
    tita$parch <- NULL
    # by=12 gives us a small fast model with an additive and a interaction term
    tita[seq(1, nrow(etitanic), by=12), ]
}
tita <- get.tita()
# tita[,4] is age
set.seed(2020)
mod.earth.tita.age <- earth(tita[,-4], tita[,4], degree=2, nfold=3, ncross=3, trace=.5, varmod.method="lm")
cat("\nsummary(mod.earth.tita.age)\n")
print(summary(mod.earth.tita.age))
plotmo(mod.earth.tita.age, SHOWCALL=TRUE)

set.seed(2020)
mod.earth.sex <- earth(sex~., data=tita, degree=2, nfold=3, ncross=3, varmod.method="earth", glm=list(family=binomial), trace=.5)
cat("\nsummary(mod.earth.sex)\n")
print(summary(mod.earth.sex))
plotmo(mod.earth.sex, SHOWCALL=TRUE)

cat("\n#=== from test.unusual.vars.R ===========================================\n")
set.seed(2020)

vdata <- data.frame(
    resp = 1:13,
    bool = c(F, F, F, F, F, T, T, T, T, T, T, T, T),
    ord  = ordered(c("ORD1", "ORD1", "ORD1",
                     "ORD1", "ORD1", "ORD1",
                     "ORD3", "ORD3", "ORD3",
                     "ORD2", "ORD2", "ORD2", "ORD2"),
                   levels=c("ORD1", "ORD3", "ORD2")),
    fac  = as.factor(c("FAC1", "FAC1", "FAC1",
                       "FAC2", "FAC2", "FAC2",
                       "FAC3", "FAC3", "FAC3",
                       "FAC1", "FAC2", "FAC3", "FAC3")),
    str  = c("STR1", "STR1", "STR1", # WILL BE TREATED LIKE A FACTOR
             "STR2", "STR2", "STR2",
             "STR3", "STR3", "STR3",
             "STR3", "STR3", "STR3", "STR3"),
    num  = c(1, 3, 2, 3, 4, 5, 6, 4, 5, 6.5, 3, 6, 5), # 7 unique values (but one is non integral)
    sqrt_num  = sqrt(c(1, 3, 2, 3, 4, 5, 6, 4, 5, 6.5, 3, 6, 5)),
    int  = c(1L, 1L, 3L, 3L, 4L, 4L, 3L, 5L, 3L, 6L, 7L, 8L, 10L), # 8 unique values
    date = as.Date(
           c("2018-08-01", "2018-08-02", "2018-08-03",
             "2018-08-04", "2018-08-05", "2018-08-06",
             "2018-08-07", "2018-08-08", "2018-08-08",
             "2018-08-08", "2018-08-10", "2018-08-11", "2018-08-11")),
    date_num = as.numeric(as.Date(
           c("2018-08-01", "2018-08-02", "2018-08-03",
             "2018-08-04", "2018-08-05", "2018-08-06",
             "2018-08-07", "2018-08-08", "2018-08-08",
             "2018-08-08", "2018-08-10", "2018-08-11", "2018-08-11"))))

vdata$off <- (1:nrow(vdata)) / nrow(vdata)

resp2 <- 13:1

vweights <- rep(1, length.out=nrow(vdata))
vweights[1] <- 2

set.seed(2020)
lognum.bool.ord.off <- earth(resp ~ log(num) + bool + ord + offset(off), degree=2, weights=vweights,
           data=vdata, pmethod="none", varmod.method="lm",
           nfold=2, ncross=3,
           trace=1)
print(summary(lognum.bool.ord.off))

cat("\n#=== from test.caret.R ===========================================\n")
set.seed(2020)

library(caret)
set.seed(2015)
a.bag3 <- bagEarth(survived~., data=etitanic, degree=2, B=3, trace=1)
print(a.bag3)
plotmo(a.bag3, clip=F, caption="bagEarth, etitanic", trace=1, SHOWCALL=TRUE)
plotres(a.bag3, clip=F, trace=1, SHOWCALL=TRUE)

# Following commented out because too slow
#
# cat("\n#=== from test.parsnip.R ===========================================\n")
# set.seed(2020)
#
# cat("loading parsnip libraries\n") # these libraries take several seconds to load
# library(tidymodels)
# library(timetk)
# library(lubridate)
# cat("loaded parsnip libraries\n")
# cat("parsnip version:", as.character(packageVersion("parsnip")[[1]]), "\n")
#
# vdata <- data.frame(
#     resp    = 1:23,
#     bool = c(F, F, F, F, F, T, T, T, T, T, T, T, T, F, F, T, T, T, T, T, T, T, T),
#     ord  = ordered(c("ORD1", "ORD1", "ORD1",
#                      "ORD1", "ORD1", "ORD1",
#                      "ORD1", "ORD3", "ORD1",
#                      "ORD2", "ORD2", "ORD2", "ORD2",
#                      "ORD2", "ORD2", "ORD2",
#                      "ORD3", "ORD3", "ORD3",
#                      "ORD2", "ORD2", "ORD2", "ORD2"),
#                    levels=c("ORD1", "ORD3", "ORD2")),
#     fac  = as.factor(c("FAC1", "FAC1", "FAC1",
#                        "FAC2", "FAC2", "FAC2",
#                        "FAC3", "FAC1", "FAC1",
#                        "FAC1", "FAC2", "FAC2", "FAC2",
#                        "FAC2", "FAC2", "FAC2",
#                        "FAC3", "FAC3", "FAC3",
#                        "FAC1", "FAC3", "FAC3", "FAC3")),
#     str  = c("STR1", "STR1", "STR1", # WILL BE TREATED LIKE A FACTOR
#              "STR1", "STR1", "STR1",
#              "STR2", "STR2", "STR2",
#              "STR3", "STR3", "STR2", "STR3",
#              "STR2", "STR3", "STR2",
#              "STR3", "STR3", "STR3",
#              "STR3", "STR3", "STR3", "STR3"),
#     num  = c(1, 9, 2, 3, 14, 5, 6, 4, 5, 6.5, 3, 6, 5,
#              3, 4, 5, 6, 4, 5, 16.5, 3, 16, 15),
#     sqrt_num  = sqrt(
#            c(1, 9, 2, 3, 14, 5, 6, 4, 5, 6.5, 3, 6, 5,
#              3, 4, 5, 6, 4, 5, 16.5, 3, 16, 15)),
#     int  = c(1L, 1L, 3L, 3L, 4L, 4L, 3L, 5L, 3L, 6L, 7L, 8L, 10L,
#              13L, 14L, 3L, 13L, 5L, 13L, 16L, 17L, 18L, 11L),
#     date = as.Date(
#            c("2018-08-01", "2018-08-02", "2018-08-03",
#              "2018-08-04", "2018-08-05", "2018-08-06",
#              "2018-08-07", "2018-08-08", "2018-08-08",
#              "2018-08-10", "2018-08-10", "2018-08-11", "2018-08-11",
#              "2018-08-11", "2018-08-12", "2018-08-13",
#              "2018-08-10", "2018-08-15", "2018-08-17",
#              "2018-08-04", "2018-08-19", "2018-08-03", "2018-08-18")),
#     date_num = as.numeric(as.Date(
#            c("2018-08-01", "2018-08-02", "2018-08-03",
#              "2018-08-04", "2018-08-05", "2018-08-06",
#              "2018-08-07", "2018-08-08", "2018-08-08",
#              "2018-08-10", "2018-08-10", "2018-08-11", "2018-08-11",
#              "2018-08-11", "2018-08-12", "2018-08-13",
#              "2018-08-10", "2018-08-15", "2018-08-17",
#              "2018-08-04", "2018-08-19", "2018-08-03", "2018-08-18"))))
#
# set.seed(2020)
# splits <- initial_time_split(vdata, prop=.9)
#
# cat("===m750a first example===\n")
# set.seed(2020)
# m750a <- m4_monthly %>%
#     filter(id == "M750") %>%
#     select(-id)
# print(m750a) # a tibble
# set.seed(2020)
# splits_a <- initial_time_split(m750a, prop = 0.9)
# earth_m750a <- earth(log(value) ~ as.numeric(date) + month(date, label = TRUE), data = training(splits_a), degree=2)
# print(summary(earth_m750a))

cat("\n#=== from test.non.earth.R ===========================================\n")
set.seed(2020)

# Following gives different results on different systems (Oct 2020, earth 5.3.0).
# For example:
#   Win7 (Intel i7-4910MQ): Earth selected 7 of 19 terms, and 3 of 3 predictors, GRSq 0.20041 RSq 0.47214
#   Ubuntu (Intel P8600):   Earth selected 2 of 19 terms, and 1 of 3 predictors, GRSq 0.18687 RSq 0.23689

library(rpart) # for kyphosis data
data(kyphosis)
a <- earth(Kyphosis ~ ., data=kyphosis, degree=2, glm=list(family=binomial), trace=4)
print(summary(a))
par(mfrow=c(3,3))
plotmo(a, type2="image", do.par=FALSE,
        col.response=ifelse(kyphosis$Kyphosis=="present", "red", "lightblue"),
        clip=F)
plotmo(a, clip=F, degree1=0, do.par=FALSE)

source("test.epilog.R")
