# test.varmod.R

source("test.prolog.R")
library(earth)
options(warn=1) # print warnings as they occur
printh <- function(caption)
    cat("===", caption, "\n", sep="")

CAPTION <- NULL

multifigure <- function(caption, nrow=3, ncol=3)
{
    CAPTION <<- caption
    printh(caption)
    par(mfrow=c(nrow, ncol))
    par(cex = 0.8)
    par(mar = c(3, 3, 5, 0.5)) # small margins but space for right hand axis
    par(mgp = c(1.6, 0.6, 0))  # flatten axis elements
    oma <- par("oma") # make space for caption
    oma[3] <- 2
    par(oma=oma)
}
do.caption <- function() # must be called _after_ first plot on new page
    mtext(CAPTION, outer=TRUE, font=2, line=1, cex=1)

multifigure("test predict.earth with pints", 2, 2)

set.seed(2)
earth.trees <- earth(Volume~Girth, data=trees, nfold=3, ncross=3, varmod.method="earth")

old.environment <- attr(earth.trees$varmod, ".Environment")
stopifnot(is.environment(old.environment))
# following necessary else print.default prints a different default environment hex address each time
attr(earth.trees$varmod, ".Environment") <- NULL
printh("print.default(earth.trees$varmod)")
print.default(earth.trees$varmod)
attr(earth.trees$varmod, ".Environment") <- old.environment

printh("summary(earth.trees)")
print(summary(earth.trees))

# level arg not allowed with interval="se"
expect.err(try(predict(earth.trees, interval="se", level=.8)))

printh("predict(earth.trees, interval=\"se\")")
stderrs <- predict(earth.trees, interval="se")
print(stderrs)

# level arg not allowed with interval="abs.residual"
expect.err(try(predict(earth.trees, interval="abs.res", level=.8)))

printh("predict(earth.trees, interval=\"abs.residual\")")
stderrs <- predict(earth.trees, interval="abs.residual")
print(stderrs)

expect.err(try(predict(earth.trees, newdata=trees, interval="cint")))

printh("predict(earth.trees, interval=\"cint\")")
cints <- predict(earth.trees, interval="cint")
print(cints)

printh("predict(earth.trees, interval=\"pin\", level=.80)")
news <- predict(earth.trees, interval="pin", level=.80)
print(news)
expect.err(try(predict(earth.trees, interval="none", level=.80)), "predict.earth: level=0.8 was specified but interval=\"none\"")
expect.err(try(predict(earth.trees, interval="pin", type="class")), "predict.earth: the interval argument is not allowed with type=\"class\"")
expect.err(try(predict(earth.trees, interval="pin", type="cl")), "predict.earth: the interval argument is not allowed with type=\"class\"")
expect.err(try(predict(earth.trees, interval="pin", type="ter")), "predict.earth: the interval argument is not allowed with type=\"terms\"")

printh("print.default(earth.trees$varmod$residmod)")
# have to modify earth.trees because terms field stores the environment
# as a hex address which messes up the diffs
earth.trees$varmod$residmod$terms <- NULL
print.default(earth.trees$varmod$residmod)
# prevent mistakes later where we try to use a modified earth.trees
remove(earth.trees)

multifigure("test example for varmod help page", 2, 2)

data(ozone1)
set.seed(1) # optional, for cross validation reproducibility

a <- earth(O3~temp, data=ozone1, nfold=10, ncross=3, varmod.method="earth")

print(summary(a)) # note additional info on the variance model

old.mfrow <- par(mfrow=c(2,2))

# the variance model assumes residuals are symmetric, which is not
# quite true in this example, so the lower band is a bit too big
plotmo(a, do.par=FALSE, col.response=1, level=.95, main="earth model: O3~temp")

plot(a, which=1)            # model selection plot, same as ever
plot(a, which=3, level=.95) # residual plot: note 95% pred and darker conf intervals
plot(a, which=3, level=.95, standardize=TRUE) # standardize resids are approx homoscedastic

par(par=old.mfrow)

plot(a$varmod)              # plot the embedded variance model (this calls plot.varmod)

multifigure("test example for plot.varmod help page", 2, 2)

# multivariate example (for univariate, see the example on the varmod help page)

data(ozone1)
set.seed(1) # optional, for cross validation reproducibility

mod.temp.vh.doy <- earth(O3~temp+vh+vis+doy, data=ozone1, nfold=5, ncross=3, varmod.method="x.earth")

print(summary(mod.temp.vh.doy))  # note additional info on the variance model

plot(mod.temp.vh.doy, level=.95) # note 95% pred and darker conf intervals in resids plot

plot(mod.temp.vh.doy$varmod)     # plot the variance model (this calls plot.varmod)

plot(mod.temp.vh.doy, versus="", level=.9, caption="plot.earth versus=\"\"")
plot(mod.temp.vh.doy, versus="v", level=.9, caption="plot.earth versus=\"v\" and versus=\"temp\"", do.par=2)
plot(mod.temp.vh.doy, versus="temp", level=.9, caption="", main="temp on same page")

# plot.earth will silently not plots it cannot plot below, so 1:9 becomes c(3,5,6)
plot(mod.temp.vh.doy, which=1:9, versus="v", info=T, caption='which=c(3,5) versus="v" info=T')
par(org.par)

# versus="b:"
plot(mod.temp.vh.doy, versus="b:", level=.9,
     caption="plot.earth versus=\"b:\"")

# versus="b:" and versus=1:4 with info
plot(mod.temp.vh.doy, versus="b:", level=.8, info=TRUE,
     caption="plot.earth versus=\"b:\" with info")

multifigure("versus=1:4", 3, 3)

plot(mod.temp.vh.doy, versus=1, caption="", do.par=FALSE, which=3)
do.caption()
plot(mod.temp.vh.doy, versus=2, caption="", do.par=FALSE)
plot(mod.temp.vh.doy, versus=3, caption="", do.par=FALSE)

plot(mod.temp.vh.doy, versus=1, info=TRUE, caption="", do.par=FALSE, which=3)
plot(mod.temp.vh.doy, versus=2, info=TRUE, caption="", do.par=FALSE)
plot(mod.temp.vh.doy, versus=3, info=TRUE, caption="", do.par=FALSE)

plot(mod.temp.vh.doy, versus=1, info=TRUE, caption="", do.par=FALSE, level=.8, which=3)
plot(mod.temp.vh.doy, versus=2, info=TRUE, caption="", do.par=FALSE, level=.8)
plot(mod.temp.vh.doy, versus=3, info=TRUE, caption="", do.par=FALSE, level=.8)

expect.err(try(plot(mod.temp.vh.doy, versus=9)))
expect.err(try(plot(mod.temp.vh.doy, versus=1.2)))
expect.err(try(plot(mod.temp.vh.doy, versus=2:3)))

# versus="b:doy"

plot(mod.temp.vh.doy, versus="b:doy", level=.9, caption="plot.earth versus=\"b:doy\"")

# test warnings from plotres about which
plot(mod.temp.vh.doy, which=1, versus="b:doy")

multifigure("test example in (very old) earth vignette", 2, 2)

data(ozone1)
x <- ozone1$temp
y <- ozone1$O3

set.seed(1) # optional, for cross validation reproducibility
earth.mod <- earth(y~x, nfold=10, ncross=3, varmod.method="earth", trace=.1)
predict <- predict(earth.mod, interval="pint")
cat("\npredict(earth.mod, interval=\"pint\")\n")
print(head(predict))

order <- order(x)
x <- x[order]
y <- y[order]
predict <- predict[order,]

inconf <- y >= predict$lwr & y <= predict$upr

plot(x, y, pch=20, col=ifelse(inconf, 1, 2), main=sprint(
    "Prediction intervals\n%.0f%% of the points are in the estimated band",
    100 * sum(inconf) / length(y)))
do.caption()

lines(x, predict$fit)
lines(x, predict$lwr, lty=2)
lines(x, predict$upr, lty=2)

# Plot the Residuals vs Fitted graph
plot(earth.mod, which=3, level=.95)

# Plot the embedded residual model
plot(earth.mod$varmod, do.par=F, which=1:2)

cat('head(residuals(earth.mod))\n')
print(head(residuals(earth.mod)))
cat('head(residuals(earth.mod, type="standardize"))\n')
print(head(residuals(earth.mod, type="standardize")))

multifigure("plot.earth varmod options", 2, 2)

plot(earth.mod, which=3, level=.95, level.shade=0, main="plot.earth varmod options")
do.caption()
plot(earth.mod, which=3, level.shade="orange", level.shade2="darkgray", level=.99)
plot(earth.mod, which=3, level=.95, level.shade=0, level.shade2="mistyrose4")

multifigure("plot.earth delever and standardize", 2, 2)

set.seed(4)
earth.mod1 <- earth(O3~temp, data=ozone1, nfold=5, ncross=3, varmod.method="lm", keepxy=T, trace=.5)
plot(earth.mod1, which=3, ylim=c(-16,20), info=TRUE, level=.95)
do.caption()
plot(earth.mod1, which=3, ylim=c(-16,20), delever=TRUE, level=.95)
plot(earth.mod1, which=3, standardize=TRUE, info=TRUE,    level=.95)
# the standardize and delever arguments cannot both be set
expect.err(try(plot(earth.mod1, which=3, standardize=TRUE, delever=TRUE, level=.95)))

multifigure("plot.earth which=5 and which=6", 2, 3)
plot(earth.mod1, which=5, info=T,            main="which=5, info=T")
plot(earth.mod1, which=5, standardize=T, info=T, main="which=5, standardize=T, info=T")
plot(earth.mod1, which=5, standardize=T,         main="which=5, standardize=T")
do.caption()
plot(earth.mod1, which=6, info=T,            main="which=6, info=T")
plot(earth.mod1, which=6, standardize=T, info=T, main="which=6, standardize=T, info=T")
plot(earth.mod1, which=6, standardize=T,         main="which=6, standardize=T")

multifigure("plot.earth which=7", 2, 3)
plot(earth.mod1, which=7, info=T,            main="which=7, info=T")
plot(earth.mod1, which=7, standardize=T, info=T, main="which=7, standardize=T, info=T")
plot(earth.mod1, which=7, standardize=T,         main="which=7, standardize=T")
do.caption()

multifigure("plot.earth which=8 and which=9", 2, 3)
plot(earth.mod1, which=8, info=T,            main="which=8, info=T")
plot(earth.mod1, which=8, standardize=T, info=T, main="which=8, standardize=T, info=T")
plot(earth.mod1, which=8, standardize=T,         main="which=8, standardize=T")
do.caption()
plot(earth.mod1, which=9, info=T,            main="which=9, info=T")
plot(earth.mod1, which=9, standardize=T, info=T, main="which=9, standardize=T, info=T")
plot(earth.mod1, which=9, standardize=T,         main="which=9, standardize=T")

multifigure("plot.earth versus=4, which=3 and which=5", 2, 3)
plot(earth.mod1, versus=4, which=3,                    main="versus=4, which=3")
plot(earth.mod1, versus=4, which=3, standardize=T, info=T, main="versus=4, which=3, standardize=T, info=T")
plot(earth.mod1, versus=4, which=3, standardize=T,         main="versus=4, which=3, standardize=T")
do.caption()
plot(earth.mod1, versus=4, which=5,                    main="versus=4, which=5")
plot(earth.mod1, versus=4, which=5, standardize=T, info=T, main="versus=4, which=5, standardize=T, info=T")
plot(earth.mod1, versus=4, which=5, standardize=T,         main="versus=4, which=5, standardize=T")

cat("summary(earth.mod1, newdata=ozone1)\n")
print(summary(earth.mod1, newdata=ozone1))

cat("summary(earth.mod1, newdata=ozone1[1:100,]:)\n")
print(summary(earth.mod1, newdata=ozone1[1:100,]))

expect.err(try(summary(earth.mod1, newdata=c(1,2,3))),
           "plotmo_response: newdata must be a matrix or data.frame")
expect.err(try(summary(earth.mod1, newdata=ozone1[1:100,1:3])),
           "response with newdata object 'temp' not found")

# earth.default
O3 <- ozone1$O3
temper <- ozone1$temp
set.seed(4)
earth.default <- earth(temper, O3, nfold=5, ncross=3, varmod.method="lm")
cat("summary(earth.default)\n")
print(summary(earth.default))
expect.err(try(summary(earth.default, newdata=ozone1[1:100,])),
          "model.matrix.earth could not interpret the data")
newdata_temper <- matrix(c(O3[1:100], temper[1:100]), ncol=2)
expect.err(try(summary(earth.default, newdata=newdata_temper)),
          "cannot get response from newdata because newdata has no column names")
colnames(newdata_temper) <- c("O3", "temper")
cat("summary(earth.default, newdata=newdata_temper)\n")
print(summary(earth.default, newdata=newdata_temper))
plot(earth.default, level=.80, caption="earth.default")
options(warn=2) # treat warnings as errors
expect.err(try(plotmo(earth.default, level=.80, col.response=3)),
           "Cannot determine which variables to plot (use all1=TRUE?)")
plotmo(earth.default, all1=TRUE, level=.80, col.response=3, caption="earth.default\nlevel = .80")
options(warn=1) # print warnings as they occur

multifigure("plot(earth.mod2)", 2, 2)
set.seed(5)
earth.mod2 <- earth(y~x, nfold=10, ncross=5, varmod.method="earth")
plot(earth.mod2, caption="plot(earth.mod2)", level=.95)
do.caption()

multifigure("plot(earth.mod2) with standardize=TRUE", 2, 2)
plot(earth.mod2, standardize=TRUE, level=.95,
     caption="plot(earth.mod2, standardize=TRUE, level=.95)")
do.caption()

multifigure("plot.varmod by calling plot(earth.mod2$varmod)", 2, 2)
plot(earth.mod2$varmod)

multifigure("embedded earth model by calling plot(earth.mod2$varmod$residmod)", 2, 2)
plot(earth.mod2$varmod$residmod, caption="embedded earth model")
do.caption()

# test varmod.* args like varmod.conv

# cat("test varmod.exponent=.5\n")
# set.seed(1)
# (earth(Volume~Girth, data=trees, nfold=3, ncross=3, varmod.method="lm", trace=.3, varmod.exponent=.5))

# cat("test varmod.lambda=2/3\n")
# set.seed(1)
# (earth(Volume~Girth, data=trees, nfold=3, ncross=3, varmod.method="lm", trace=.3, varmod.lambda=2/3))

cat("test varmod.conv=50%\n")
set.seed(1)
(earth(Volume~Girth, data=trees, nfold=3, ncross=3, varmod.method="lm", trace=.3, varmod.conv=50))

cat("test varmod.conv=-5\n")
set.seed(1)
(earth(Volume~Girth, data=trees, nfold=3, ncross=3, varmod.method="lm", trace=.3, varmod.conv=-5))

cat("test varmod.clamp\n")
set.seed(1)
a.noclamp <- earth(Volume~Girth, data=trees, nfold=3, ncross=3, varmod.method="lm")
plot(a.noclamp$varmod, which=1:2, caption="a.noclamp and a.clamp", do.par=FALSE)
set.seed(1)
a.clamp <- earth(Volume~Girth, data=trees, nfold=3, ncross=3, varmod.method="lm", varmod.clamp=.6)
plot(a.clamp$varmod, which=1:2, caption="", do.par=FALSE)

cat("test varmod.minspan=-5\n")
set.seed(1)
a.varmod.minspan.minus5 <- earth(Volume~Girth, data=trees, nfold=3, ncross=3, varmod.method="earth", trace=.3, varmod.minspan=-5)
print(coef(a.varmod.minspan.minus5$varmod))
cat("test varmod.minspan=1\n")
set.seed(1)
a.varmod.minspan1 <- earth(Volume~Girth, data=trees, nfold=3, ncross=3, varmod.method="earth", trace=.3, varmod.minspan=1)
print(coef(a.varmod.minspan1$varmod))

# gam and y.gam are repeated below and on the repeat we will use the mgcv not gam package
use.mgcv.package <- FALSE

for(varmod.method in c(earth:::VARMOD.METHODS, "gam", "x.gam")) {

    multifigure(sprint("varmod.method=\"%s\"", varmod.method), 2, 3)
    par(mar = c(3, 3, 2, 3)) # space for right margin axis

    if(varmod.method %in% c("gam", "x.gam")) {
        if(use.mgcv.package) {
            # TODO with R 3.2.1 unload(gam) no longer works
            cat("skipping mgcv tests\n")
            next                                # NOTE next
            cat("library(mgcv)\n")
            library(mgcv)
        } else
            library(gam)
    }
    set.seed(2019)
    # may 2019: following added because gam version 1.16 R version 3.6.0 gives Warning: non-list contrasts argument ignored
    if(varmod.method %in% c("gam", "x.gam"))
        options(warn=1)
    earth.mod <- earth(Volume~Girth, data=trees, nfold=3, ncross=3,
                       varmod.method=varmod.method,
                       trace=if(varmod.method %in% c("const", "lm", "power")) .3 else 0)
    printh(sprint("varmod.method %s: summary(earth.mod)", varmod.method))
    printh("summary(earth.mod)")
    print(summary(earth.mod))

    if(use.mgcv.package && (varmod.method == "x.gam" || varmod.method == "gam")) {
        # summary(mgcv) prints environment as hex address which messes up the diffs
        printh("skipping summary(mgcv::gam) etc.\n")
    } else {
        printh("earth.mod$varmod")
        print(earth.mod$varmod, style="unit")

        printh("summary(earth.mod$varmod)")
        print(summary(earth.mod$varmod))

        printh("summary(earth.mod$varmod$residmod)")
        print(summary(earth.mod$varmod$residmod))
    }
    printh(sprint("varmod.method %s: predict(earth.mod, interval=\"pint\")", varmod.method))
    pints <- predict(earth.mod, interval="pint")
    print(pints)

    plotmo(earth.mod$varmod, do.par=FALSE, col.response=2, clip=FALSE,
           main="plotmo residual model",
           xlab="x", ylab="varmod residuals")

    plotmo(earth.mod, level=.90, do.par=FALSE, col.response=1, clip=FALSE,
           main="main model plotmo Girth")
    do.caption()

    plot(earth.mod, which=3, do.par=FALSE, level=.95)

    # plot.varmod
    plot(earth.mod$varmod, do.par=FALSE, which=1:3, info=(varmod.method=="earth"))

    # on second use of gam and y.gam we want to use the mgcv package
    if(varmod.method == "x.gam" && !use.mgcv.package) {
        use.mgcv.package <- TRUE
        cat("detach(\"package:gam\", unload=TRUE)\n")
        detach("package:gam", unload=TRUE)
    }
}
# test varmod.exponent
set.seed(6)
earth.exponent <- earth(Volume~Girth, data=trees, nfold=3, ncross=3,
                        varmod.method="lm", varmod.exponent=.5)
printh("summary(earth.exponent)")
print(summary(earth.exponent))

par(org.par)

source("test.epilog.R")
