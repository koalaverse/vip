# test.multresp.R: test multiple response models using the Formula interface
# Stephen Milborrow Mar 2019 Petaluma

source("test.prolog.R")
source("check.models.equal.R")
options(warn=1) # print warnings as they occur
library(earth)
show.earth.mod <- function(mod, modname, nresponses, caption, trace, ...)
{
    set.seed(2019)
    cat("\nsummary:", modname, "\n\n")
    print(summary(mod))
    cat("\nevimp:", modname, "\n\n")
    evimp <- evimp(mod)
    print(evimp)
    cat("\n")
    nrow <- 1 + max(1, ceiling(nresponses * nrow(evimp(mod)) / 2))
    par(mfrow=c(nrow, 2), mar=c(3, 3, 3, 1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 5, 0))
    if(nresponses == 1)
        plot(mod, which=c(1,3), do.par=0, caption=caption, trace=trace)
    else {
        plot(mod, nresponse=1, which=3, do.par=0,
             caption=caption, trace=trace,
             main="Response 1: Residuals vs Fitted")
        plot(mod, nresponse=max(nresponses), which=3, do.par=0,
             caption=caption, trace=trace,
             main=sprint("Response %d: Residuals vs Fitted", max(nresponses)))
    }
    cat("\nplotmo:", modname, "\n\n")
    if(nresponses == 1)
        plotmo(mod, do.par=0, pt.col="red", trace=trace)
    else for(iresponse in 1:nresponses)
        plotmo(mod, nresponse=iresponse, do.par=0, pt.col=iresponse+1, trace=trace)
    par(org.par)
    cat("-------------------------------------------------------------------------------\n\n")
}
show.earth.formula <- function(formula, data=trees, subset=NULL, nresponses=1,
                               show=TRUE, caption=modname, trace=0, ...)
{
    modname <- sprint("formula=%s (nresponses=%d)",
                    deparse(substitute(formula)), nresponses)
    printf("%s\n", modname)
    # use formula, not Formula
    mod <- earth(formula=formula, data=data, subset=subset, trace=1, keepxy=TRUE)
    global.mod <<- mod
    n <- if(is.null(subset)) nrow(data) else nrow(data[subset,])
    if(!(all(dim(mod$fitted.values) == c(n, nresponses)))) {
        cat("dim(mod$fitted.values)", dim(mod$fitted.values), "\n")
        stop("show.earth.formula: wrong response dimensions (see above)")
    }
    if(show)
        show.earth.mod(mod=mod, modname=modname, nresponses=nresponses,
                       caption=caption, trace=trace, ...)
    mod
}
show.earth.Formula <- function(formula, data=trees, subset=NULL, nresponses=1,
                               show=TRUE, caption=modname, trace=0, ...)
{
    modname <- sprint("Formula=%s (nresponses=%d)",
                    deparse(substitute(formula)), nresponses)
    printf("%s\n", modname)
    # use Formula, not formula
    # use trace=1 so so can that we can see trace message:
    # Using class "Formula" because lhs of formula has terms separated by "+"
    mod <- earth(formula=formula, data=data, subset=subset, trace=1, keepxy=TRUE)
    global.mod <<- mod
    if(!(all(dim(mod$fitted.values) == c(31, nresponses)))) {
        cat("dim(mod$fitted.values)", dim(mod$fitted.values), "\n")
        stop("show.earth.Formula: wrong response dimensions (see above)")
    }
    show.earth.mod(mod=mod, modname=modname, nresponses=nresponses,
                   caption=caption, trace=trace, ...)
    mod
}
VolNeg <- -sqrt(trees$Volume)
SinVol <- sin(pi * trees$Volume / max(trees$Volume))
global.mod <- NULL

# following use formula (not Formula)
show.earth.formula(Volume/VolNeg       ~.,  show=FALSE) # show=FALSE to save time
show.earth.formula(Volume/99            ~., show=FALSE)
show.earth.formula(Volume*99            ~., show=FALSE)
show.earth.formula(Volume-99            ~., show=FALSE)
show.earth.formula(Volume               ~., show=FALSE)
show.earth.formula(cbind(Volume+VolNeg)~.,  show=FALSE)
show.earth.formula((Volume+VolNeg)     ~.,  show=FALSE)
show.earth.formula(I(Volume+VolNeg)    ~.,  show=FALSE)
show.earth.formula(VolNeg~Girth+Height,     show=FALSE)

# use formula, but multiple response
show.earth.formula(cbind(VolNeg,    SinVol)~., nresponses=2, show=FALSE)
show.earth.formula(cbind(VolNeg,    SinVol)~., nresponses=2, show=FALSE)
show.earth.formula(cbind(VolNeg/33, SinVol)~., nresponses=2, show=FALSE)
show.earth.formula(cbind(VolNeg+33, SinVol)~., nresponses=2, show=FALSE)
show.earth.formula(cbind(VolNeg,    SinVol)~Girth,  nresponses=2, show=FALSE)
randx <- c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0)
show.earth.formula(VolNeg~randx, nresponses=1) # intercept only model
VolNeg.randx <- earth(VolNeg~randx, trace=1)   # intercept only model
plotmo(VolNeg.randx)
VolVolNeg <- show.earth.formula(cbind(Volume, VolNeg)~Girth+Height, nresponses=2, trace=0)
# Use a global variable for Volume
trees1 <- trees
Volume <- trees1$Volume # global Volume
trees1$Volume <- NULL # Volume no longer available in trees1
cbind.Volume.VolNeg <- cbind(Volume, VolNeg)
VolGlobalVolNeg <- show.earth.formula(cbind(Volume, VolNeg)~Girth+Height, data=trees1, nresponses=2, trace=0,
                               caption="VolGlobalVolNeg: This page should be the same as the previous page")
check.models.equal(VolVolNeg, VolGlobalVolNeg, msg="VolVolNeg, VolGlobalVolNeg", newdata=trees[3,])

# following use Formula (not formula)
VolVolNega <- show.earth.Formula(Volume+VolNeg~Girth+Height, nresponses=2,
                               caption="VolVolNega: This page should be the same as the previous page")
check.models.equal(VolVolNega, VolVolNeg, msg="VolVolNega, VolVolNeg", newdata=trees[3,])
Vol.VolNeg.dot <- show.earth.Formula(Volume+VolNeg~., nresponses=2, # use dot
                                caption="Vol.VolNeg.dot: This page should be the same as the previous page")
check.models.equal(Vol.VolNeg.dot, VolVolNega, msg="Vol.VolNeg.dot, VolVolNega", newdata=trees[3,])

trees1 <- trees
trees1$VolNeg <- VolNeg
VolVolNegc <- show.earth.Formula(Volume+VolNeg~., data=trees1, nresponses=2, # all variables on lhs in data argument (none global)
                   caption="Vol.VolNeg.trees1: This page should be the same as the previous page")
check.models.equal(VolVolNegc, VolVolNega, msg="VolVolNegc, VolVolNega", newdata=trees1[2:3,])

# check without using keepxy=TRUE (because show.earth.Formula uses keepxy=TRUE)
VolVolNega.nokeepxy <- earth(Volume+VolNeg~Girth+Height, data=trees, trace=1)
check.models.equal(VolVolNega.nokeepxy, VolVolNega, msg="VolVolNega.nokeepxy, VolVolNega", newdata=trees1[2:3,])
caption <- "VolVolNega.nokeepxy This page should be the same as the previous page"
par(mfrow=c(3, 2), mar=c(3, 3, 3, 1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 5, 0))
plot(VolVolNega.nokeepxy, nresponse=1, which=3, do.par=0,
     caption=caption, trace=0,
     main="Response 1: Residuals vs Fitted")
plot(VolVolNega.nokeepxy, nresponse=2, which=3, do.par=0,
     caption=caption, trace=0,
     main="Response 2: Residuals vs Fitted")
plotmo(VolVolNega.nokeepxy, nresponse=1, do.par=0, pt.col=2)
plotmo(VolVolNega.nokeepxy, nresponse=2, do.par=0, pt.col=3)
par(org.par)
plot(VolVolNega.nokeepxy) # Warning: Defaulting to nresponse=1, see above messages

# subset, single response
# TODO we don't use show.earth.formula here because there are plotmo problems
#      with subset and keepxy when called inside a function
subset2 <- seq(from=1, to=nrow(trees1), by=2)
Vol.formula.subset.nokeepxy <- earth(Volume~Girth+Height, data=trees1, subset=subset2, trace=1)
plot(Vol.formula.subset.nokeepxy, caption="Vol.formula.subset.nokeepxy")
plotmo(Vol.formula.subset.nokeepxy, nresponse=1, trace=1, pt.col=2, caption="Vol.formula.subset.nokeepxy")

Vol.formula.subset.keepxy <- earth(Volume~Girth+Height, data=trees1, subset=subset2, trace=1)
plotmo(Vol.formula.subset.keepxy, nresponse=1, trace=1, pt.col=2, caption="Vol.formula.subset.keepxy")

# subset, multiple response
VolVolNega.formula.subset.nokeepxy <- earth(cbind.Volume.VolNeg~Girth+Height, data=trees1, subset=subset2, trace=1)
VolVolNega.Formula.subset.nokeepxy <- earth(Volume+VolNeg      ~Girth+Height, data=trees1, subset=subset2, trace=1)
check.models.equal(VolVolNega.formula.subset.nokeepxy, VolVolNega.Formula.subset.nokeepxy, "VolVolNega.formula.subset.nokeepxy, VolVolNega.Formula.subset.nokeepxy", newdata=trees[3,])

caption <- "VolVolNega.formula.subset.nokeepxy"
par(mfrow=c(3, 2), mar=c(3, 3, 3, 1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 5, 0))
plot(VolVolNega.formula.subset.nokeepxy, nresponse=1, which=3, do.par=0,
     caption=caption, trace=1,
     main="Response 1: Residuals vs Fitted")
plot(VolVolNega.formula.subset.nokeepxy, nresponse=2, which=3, do.par=0,
     caption=caption, trace=1,
     main="Response 2: Residuals vs Fitted")
plotmo(VolVolNega.formula.subset.nokeepxy, nresponse=1, do.par=0, pt.col=2)
plotmo(VolVolNega.formula.subset.nokeepxy, nresponse=2, do.par=0, pt.col=3)
par(org.par)

caption <- "VolVolNega.Formula.subset.nokeepxy"
par(mfrow=c(3, 2), mar=c(3, 3, 3, 1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 5, 0))
plot(VolVolNega.Formula.subset.nokeepxy, nresponse=1, which=3, do.par=0,
     caption=caption, trace=1,
     main="Response 1: Residuals vs Fitted")
plot(VolVolNega.Formula.subset.nokeepxy, nresponse=2, which=3, do.par=0,
     caption=caption, trace=1,
     main="Response 2: Residuals vs Fitted")
plotmo(VolVolNega.Formula.subset.nokeepxy, nresponse=1, do.par=0, pt.col=2)
plotmo(VolVolNega.Formula.subset.nokeepxy, nresponse=2, do.par=0, pt.col=3)
par(org.par)

# subset, multiple response, keepxy
subset2 <- seq(from=1, to=nrow(trees1), by=2)
VolVolNega.formula.subset.keepxy <- earth(cbind.Volume.VolNeg~Girth+Height, data=trees1, subset=subset2, trace=1, keepxy=TRUE)
VolVolNega.Formula.subset.keepxy <- earth(Volume+VolNeg      ~Girth+Height, data=trees1, subset=subset2, trace=1, keepxy=TRUE)
check.models.equal(VolVolNega.formula.subset.nokeepxy, VolVolNega.formula.subset.keepxy, msg="VolVolNega.formula.subset.nokeepxy, VolVolNega.formula.subset.keepxy", newdata=trees1[2:3,])
check.models.equal(VolVolNega.Formula.subset.nokeepxy, VolVolNega.Formula.subset.keepxy, msg="VolVolNega.Formula.subset.nokeepxy, VolVolNega.Formula.subset.keepxy", newdata=trees1[2:3,])
check.models.equal(VolVolNega.formula.subset.keepxy, VolVolNega.Formula.subset.keepxy, msg="VolVolNega.formula.subset.keepxy, VolVolNega.Formula.subset.keepxy", newdata=trees1[2:3,])

caption <- "VolVolNega.formula.subset.keepxy"
par(mfrow=c(3, 2), mar=c(3, 3, 3, 1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 5, 0))
plot(VolVolNega.formula.subset.keepxy, nresponse=1, which=3, do.par=0,
     caption=caption, trace=1,
     main="Response 1: Residuals vs Fitted")
plot(VolVolNega.formula.subset.keepxy, nresponse=2, which=3, do.par=0,
     caption=caption, trace=1,
     main="Response 2: Residuals vs Fitted")
# TODO following fail: subset and keepxy
try(plotmo(VolVolNega.formula.subset.keepxy, nresponse=1, do.par=0, pt.col=2))
try(plotmo(VolVolNega.formula.subset.keepxy, nresponse=2, do.par=0, pt.col=3))
par(org.par)

caption <- "VolVolNega.Formula.subset.keepxy"
par(mfrow=c(3, 2), mar=c(3, 3, 3, 1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 5, 0))
plot(VolVolNega.Formula.subset.keepxy, nresponse=1, which=3, do.par=0,
     caption=caption, trace=1,
     main="Response 1: Residuals vs Fitted")
plot(VolVolNega.Formula.subset.keepxy, nresponse=2, which=3, do.par=0,
     caption=caption, trace=1,
     main="Response 2: Residuals vs Fitted")
# TODO following fail: subset and keepxy
try(plotmo(VolVolNega.Formula.subset.keepxy, nresponse=1, do.par=0, pt.col=2))
try(plotmo(VolVolNega.Formula.subset.keepxy, nresponse=2, do.par=0, pt.col=3))
par(org.par)

# subset, multiple response, weights
weights2 <- sqrt(1:nrow(trees1))
VolVolNega.formula.weights.subset.nokeepxy <- earth(cbind.Volume.VolNeg~Girth+Height, data=trees1, weights=weights2, subset=subset2, trace=1)
VolVolNega.Formula.weights.subset.nokeepxy <- earth(Volume+VolNeg      ~Girth+Height, data=trees1, weights=weights2, subset=subset2, trace=1)
check.models.equal(VolVolNega.formula.weights.subset.nokeepxy, VolVolNega.Formula.weights.subset.nokeepxy, "VolVolNega.formula.weights.subset.nokeepxy, VolVolNega.Formula.weights.subset.nokeepxy")

caption <- "VolVolNega.formula.weights.subset.nokeepxy"
par(mfrow=c(3, 2), mar=c(3, 3, 3, 1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 5, 0))
plot(VolVolNega.formula.weights.subset.nokeepxy, nresponse=1, which=3, do.par=0,
     caption=caption, trace=1,
     main="Response 1: Residuals vs Fitted")
plot(VolVolNega.formula.weights.subset.nokeepxy, nresponse=2, which=3, do.par=0,
     caption=caption, trace=1,
     main="Response 2: Residuals vs Fitted")
plotmo(VolVolNega.formula.weights.subset.nokeepxy, nresponse=1, do.par=0, pt.col=2)
plotmo(VolVolNega.formula.weights.subset.nokeepxy, nresponse=2, do.par=0, pt.col=3)
par(org.par)

caption <- "VolVolNega.Formula.weights.subset.nokeepxy"
par(mfrow=c(3, 2), mar=c(3, 3, 3, 1), mgp=c(1.5, 0.5, 0), oma=c(0, 0, 5, 0))
plot(VolVolNega.Formula.weights.subset.nokeepxy, nresponse=1, which=3, do.par=0,
     caption=caption, trace=1,
     main="Response 1: Residuals vs Fitted")
plot(VolVolNega.Formula.weights.subset.nokeepxy, nresponse=2, which=3, do.par=0,
     caption=caption, trace=1,
     main="Response 2: Residuals vs Fitted")
plotmo(VolVolNega.Formula.weights.subset.nokeepxy, nresponse=1, do.par=0, pt.col=2)
plotmo(VolVolNega.Formula.weights.subset.nokeepxy, nresponse=2, do.par=0, pt.col=3)
par(org.par)

# examples in earth vignette
data(ozone1)
mul1 <- earth(cbind(O3,wind) ~ ., data=ozone1) # formula interface
mul2 <- earth(O3 + wind ~ ., data=ozone1) # use + on left of formula
check.models.equal(mul2, mul1, "mul2, mul1", newdata=ozone1[1:3,])
mul3 <- earth(ozone1[,-c(1,3)], ozone1[,c(1,3)]) # x,y interface
check.models.equal(mul3, mul1,"mul3, mul", newdata=ozone1[1:3,])
mul4 <- earth(cbind(log.O3=log(O3),wind) ~ ., data=ozone1)
print(summary(mul4))
x1 <- ozone1$O3
x2 <- ozone1$win
x3 <- ozone1$O3
y1 <- ozone1$temp
y2 <- ozone1$doy
mul5 <- earth(x=data.frame(x1, x2, log.x3=log(x3)), y=data.frame(y1, y2), trace=1)
print(summary(mul5))
log.x3 <- log(x3)
mul6 <- earth(y1 + y2 ~ x1 + x2 + log.x3, trace=1)
stopifnot(all.equal(as.vector(mul5$coefficients), as.vector(mul6$coefficients)))
stopifnot(all.equal(as.vector(mul5$dirs), as.vector(mul6$dirs)))
mul7 <- earth(y1 + y2 ~ x1 + x2 + log(x3), trace=1)
stopifnot(all.equal(as.vector(mul5$coefficients), as.vector(mul7$coefficients)))
stopifnot(all.equal(as.vector(mul5$dirs), as.vector(mul7$dirs)))

# TODO Sep 2020: work around for model.matrix.Formula which incorrectly includes log(03) on lhs
expect.err(try(earth(log(O3 + wind) + ibt ~ temp, data=ozone1, trace=1)),
           "terms like 'log(O3 + wind)' are not allowed on the LHS of a multiple-response formula")

expect.err(try(show.earth.Formula(VolNeg+Volume~1, nresponses=2)), "'x' has no columns")
# use lhs on rhs TODO earth itself should give an error message, not just plotmo
expect.err(try(show.earth.Formula(VolNeg+Volume~Volume, nresponses=2)), "x is empty") # err from plotmo
# formula has better error handling than Formula (model.matrix.default gives warning)
options(warn=2)
expect.err(try(show.earth.formula(Volume~Volume)), "(converted from warning) the response appeared on the right-hand side and was dropped")
options(warn=1) # print warnings as they occur
show.earth.Formula(VolNeg+Volume~Girth, nresponses=2, subset=)
show.earth.Formula(Volume+VolNeg+SinVol~., nresponses=3)
show.earth.formula(VolNeg+SinVol~randx, nresponses=2)       # intercept only model
VolNeg.SinVol.randx <- earth(VolNeg+SinVol~randx, trace=1)  # intercept only model
plotmo(VolNeg.SinVol.randx, nresponse=2)

# TODO following should say "invalid formula: too many terms on the left hand side", but at least it gives an error message
expect.err(try(earth(Volume+VolNeg|99~Girth+Height, data=trees, trace=1)), "multiple parts on left side of formula (because \"|\" was used)")
expect.err(try(earth(Volume+VolNeg~Girth+Height|Volume, data=trees, trace=1)), "multiple parts on right side of formula (because \"|\" was used)")
a1 <- earth(Volume+VolNeg~Girth+(Height|Volume), data=trees, trace=1) # ok, because | is in () (and earth will use formula, not Formula)
stopifnot(NCOL(a1$coefficients) == 2)
a2 <- earth(Volume+VolNeg~Girth+I(Height|Volume), data=trees, trace=1) # ok, because | is in I()
stopifnot(NCOL(a2$coefficients) == 2)
a3 <- earth((Volume+VolNeg)~Girth+Height, data=trees, trace=1) # ok, earth will build a single response model
stopifnot(NCOL(a3$coefficients) == 1)
# TODO it's a pity the following don't work
expect.err(try(earth(Volume+VolNeg*999~., data=trees, trace=1)), "invalid model formula in ExtractVars")       # use Formula
expect.err(try(earth(Volume+VolNeg/99+SinVol~., data=trees, trace=1)), "invalid model formula in ExtractVars") # use Formula

library(earth)
data(ozone1)

# TODO Sep 2020: work around for model.matrix.Formula which incorrectly includes log(03)+wind on lhs
expect.err(try(earth(log(O3) + wind             ~ ., data=ozone1, trace=1)),
           "terms like 'log(O3)' are not allowed on the LHS of a multiple-response formula")

a1 <- earth(cbind(log.O3=log(O3),wind) ~ humidity+temp, data=ozone1)
options(warn=2)
expect.err(try(coef(a1)), "coef.earth: multiple response model: returning coefficients for just the first response")
options(warn=1)
a2 <- earth(cbind(log(O3),wind) ~ humidity+temp, data=ozone1)
stopifnot(all.equal(as.vector(a2$coefficients), as.vector(a1$coefficients)))
log.O3 <- log(ozone1$O3)
a3 <- earth(cbind(log.O3,wind) ~ humidity+temp, data=ozone1)
stopifnot(all.equal(as.vector(a3$coefficients), as.vector(a1$coefficients)))
a4 <- earth(log.O3+wind ~ humidity+temp, data=ozone1)
stopifnot(all.equal(as.vector(a4$coefficients), as.vector(a1$coefficients)))

# TODO Sep 2020: work around for model.matrix.Formula which incorrectly includes log(03) on lhs
expect.err(try(earth(log(O3)+wind ~ humidity+temp, data=ozone1)),
           "terms like 'log(O3)' are not allowed on the LHS of a multiple-response formula")

# multiple responses, mixed factors and numeric
data(etitanic)
pclass.age <- earth(pclass+age~sibsp, data=etitanic)
plot(pclass.age, nresponse=4)
par(mfrow=c(2,2))
cat("plotmo(pclass.age, nresponse=1):\n")
plotmo(pclass.age, nresponse=1, main="nresponse=1, pclass1st", do.par=FALSE)
cat("plotmo(pclass.age, nresponse=2):\n")
plotmo(pclass.age, nresponse=2, main="nresponse=2, pclass2nd", do.par=FALSE)
cat("plotmo(pclass.age, nresponse=3):\n")
plotmo(pclass.age, nresponse=3, main="nresponse=3, pclass3rd", do.par=FALSE)
cat("plotmo(pclass.age, nresponse=4):\n")
plotmo(pclass.age, nresponse=4, main="nresponse=4, age", do.par=FALSE)
cat("plotmo(pclass.age, nresponse=5):\n")
options(warn=2)
expect.err(try(plotmo(pclass.age, nresponse=5, main="nresponse=5", do.par=FALSE)), "nresponse is 5 but the number of columns is only 4")
options(warn=1)

age.pclass <- earth(age+pclass~sibsp, data=etitanic)
par(mfrow=c(2,2))
cat("plotmo(age.pclass, nresponse=1):\n")
plotmo(age.pclass, nresponse=1, main="nresponse=1, age", do.par=FALSE, trace=1)
cat("plotmo(age.pclass, nresponse=2):\n")
plotmo(age.pclass, nresponse=2, main="nresponse=2, pclass1st", do.par=FALSE)
cat("plotmo(age.pclass, nresponse=3):\n")
plotmo(age.pclass, nresponse=3, main="nresponse=3, pclass2nd", do.par=FALSE)
cat("plotmo(age.pclass, nresponse=4):\n")
plotmo(age.pclass, nresponse=4, main="nresponse=4, pclass3rd", do.par=FALSE)
cat("plotmo(age.pclass, nresponse=5):\n")
options(warn=2)
expect.err(try(plotmo(age.pclass, nresponse=5, main="nresponse=5", do.par=FALSE)), "nresponse is 5 but the number of columns is only 4")
options(warn=1)

pclass.sex <- earth(pclass+sex~sibsp, data=etitanic)
par(mfrow=c(2,2))
cat("plotmo(pclass.sex, nresponse=1):\n")
plotmo(pclass.sex, nresponse=1, main="nresponse=1, pclass1st", do.par=FALSE, trace=1)
cat("plotmo(pclass.sex, nresponse=2):\n")
plotmo(pclass.sex, nresponse=2, main="nresponse=2, pclass2nd", do.par=FALSE)
cat("plotmo(pclass.sex, nresponse=3):\n")
plotmo(pclass.sex, nresponse=3, main="nresponse=3, pclass3rd", do.par=FALSE)
cat("plotmo(pclass.sex, nresponse=4):\n")
plotmo(pclass.sex, nresponse=4, main="nresponse=4, age", do.par=FALSE)

cat("plotmo(pclass.sex, nresponse=5):\n")
options(warn=2)
expect.err(try(plotmo(pclass.sex, nresponse=5, main="nresponse=5", do.par=FALSE)), "nresponse is 5 but the number of columns is only 4")
options(warn=1)

# try to delete a varname (expose bug in model.matrix.Formula)
options(warn=2)
expect.err(try(earth(pclass+sex~.-survived, data=etitanic)), "'varlist' has changed (from nvar=4) to new 5 after EncodeVars() -- should no longer happen!")
options(warn=1)
expect.err(try(earth(pclass+sex~.-survived, data=etitanic)), "model frame and formula mismatch in model.matrix()")

# try to delete a varname not in data (expose bug in model.matrix.Formula)
options(warn=2)
expect.err(try(earth(pclass+sex~.-nonesuch, data=etitanic)), "'varlist' has changed (from nvar=5) to new 6 after EncodeVars() -- should no longer happen!")
options(warn=1)
expect.err(try(earth(pclass+sex~.-nonesuch, data=etitanic)), "model frame and formula mismatch in model.matrix()")

source("test.epilog.R")
