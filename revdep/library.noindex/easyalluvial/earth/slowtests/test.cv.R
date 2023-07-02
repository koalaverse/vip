# test.cv.R: test earth cross validation

source("test.prolog.R")
source("check.models.equal.R")
library(earth)
data(ozone1)
data(trees)
data(etitanic)
options(width=200)

printh <- function(x, expect.warning=FALSE, max.print=0) # like print but with a header
{
    cat("===", deparse(substitute(x)))
    if(expect.warning)
        cat(" expect warning -->")
    else if (NROW(x) > 1)
        cat("\n")
    if (max.print > 0)
        print(head(x, n=max.print))
    else
        print(x)
}

# print contents of earth.model, for sanity checking that all fields are present as usual
# but strip big fields to reduce amount of printing

print.stripped.earth.model <- function(earth.mod, model.name)
{
    earth.mod$bx <- NULL
    earth.mod$fitted.values <- NULL
    earth.mod$residuals <- NULL
    earth.mod$rss <- NULL
    cat("print.stripped.earth.model(", model.name, ")\n", sep="")
    print.default(earth.mod)
    cat("-------------------------------------------------------------------------------\n\n")
}

cat("a0: trees\n\n")

set.seed(23)
a0 <- earth(Volume ~ ., data = trees, trace=0.5, nfold=3)
printh(a0$cv.rsq.tab)
printh(a0)
printh(summary(a0))
print.stripped.earth.model(a0, "a0")

cat("a0a: trees with matrix interface\n\n")

set.seed(23)
a0a <- earth(trees[,-3], trees[,3], trace=0, nfold=3)
stopifnot(!identical(a0$cv.rsq.tab, a0a$cv.rsq.tab))
printh(a0a)
printh(summary(a0a))
print.stripped.earth.model(a0a, "a0a")

cat("a1: trees with trace enabled\n\n")

set.seed(1)
a1 <- earth(Volume ~ ., data = trees, trace=1, nfold=3)
stopifnot(!identical(a0$cv.rsq.tab, a1$cv.rsq.tab))
printh(a1)
printh(summary(a1))

# test correct operation of update

cat("a2 <- update(a0) # should do cv\n")
set.seed(2)
a2 <- update(a0)
cat("a3 <- update(a0) # should do cv\n")
set.seed(3)
a3 <- update(a0, formula=Volume~.-Height)
printh(a3$cv.rsq.tab)
printh(a3)
printh(summary(a3))
cat("a4 <- update(a0, nfold=0, trace=.5)  # should not do cv\n")
set.seed(4)
a4 <- update(a0, nfold=0, trace=.5)
cat("a5 <- update(a4, trace=.5)           # should not do cv\n")
set.seed(5)
a5 <- update(a4)
cat("a5a <- update(a4, nfold=2, trace=.5) # should do cv\n")
set.seed(2)
a5a <- update(a4, nfold=2, trace=.5)

cat("a6: titanic data, one logical response\n\n")
survived. <- as.logical(etitanic$survived)
set.seed(6)
a6 <- earth(survived. ~ ., data=etitanic[,-2], degree=2, glm=list(family="binomial"), trace=0.5, ncross=2, nfold=3)
printh(a6)
printh(summary(a6))
plotmo(a6)
printh(a6$cv.list[[2]])
printh(summary(a6$cv.list[[2]]))
print.stripped.earth.model(a6, "a6")

cat("a6a: stratify=FALSE\n\n")
set.seed(6)
a6a <- earth(survived. ~ ., data=etitanic[,-2], degree=2, glm=list(family="binomial"), trace=0.5, nfold=3, stratify=FALSE, keepxy=TRUE)
printh(a6a)
printh(summary(a6a))
plot(a6a, main="a6a (stratify=FALSE)", which=1, col.oof.labs=1)

cat("a7: titanic data, multiple responses (i.e. 3 level factor)\n\n")
set.seed(3)
# keepxy is needed for summary and plotmo of resmodels
a7 <- earth(pclass ~ ., data=etitanic, degree=2, glm=list(family="binomial"), trace=1, ncross=2, nfold=3, keepxy=TRUE)
printh(a7)
plot(a7, nresponse=1)
print.stripped.earth.model(a7, "a7")
printh(summary(a7))
plotmo(a7, nresponse=1)
printh(a7$cv.list[[3]])
printh(summary(a7$cv.list[[3]]))
plot(a7, main="a7 (multiple response model)", which=1, nresponse=1)

cat("a7.wp: as above but with wp parameter\n\n")
set.seed(3)
a7.wp <- earth(pclass ~ ., data=etitanic, degree=2, glm=list(family="binomial"), trace=0.5, nfold=3, wp=c(1,3,1))
printh(a7.wp)
printh(summary(a7.wp))
print.stripped.earth.model(a7.wp, "a7.wp")

# poisson models

counts <- c(18,  17,  15,  20,  10,  20,  25,  13,  12,
            18+2,17+2,15+2,20+2,10+2,20+2,25+2,13+2,12+2,
            18+3,17+3,15+3,20+3,10+3,20+3,25+3,13+3,12+3,
            18+4,17+4,15+4,20+4,10+4,20+4,25+4,13+4,12+4)
counts2 <- c(181,171,151,201,101,201,251,131,121,
             189,179,159,209,109,209,259,139,121,
             185,175,155,205,105,205,255,135,125,
             183,173,153,203,103,203,253,133,123)
outcome <- gl(3,1,4*9)
treatment <- gl(3,4*3)
d.AD <- data.frame(treatment, outcome, counts, counts2)

# one response poisson model
cat("a8p: one response poisson model\n\n")
set.seed(1236)
a8p <- earth(counts ~ outcome + treatment, glm=list(family=poisson()), trace=0.5, pmethod="none", nfold=3)
printh(a8p)
printh(summary(a8p))
print.stripped.earth.model(a8p, "a8p")
# two response poisson model
cat("a10: two response poisson model\n\n")
set.seed(1237)
a10 <- earth(cbind(counts, counts2) ~ outcome + treatment, glm=list(fam="po"), trace=0.5, pmethod="none", nfold=3)
printh(a10)
printh(summary(a10))
print.stripped.earth.model(a10, "a10")
# binomial pair model with keepxy
set.seed(2019)
bpair.mod <- earth(cbind(counts, counts2) ~ outcome + treatment,
                   glm=list(fam="quasib"), trace=1,
                   pmethod="none", nfold=3, keepxy=TRUE)
print(summary(bpair.mod))
plot(bpair.mod, which=1, main="bpair.mod")

bpair.data <- data.frame(counts, counts2, outcome, treatment)
set.seed(2019)
bpair.mod.Formula <- earth(counts+counts2 ~ outcome + treatment, data=bpair.data,
                   glm=list(fam="quasib"), trace=1,
                   pmethod="none", nfold=3, keepxy=TRUE)
print(summary(bpair.mod.Formula))
plot(bpair.mod.Formula, which=1, main="bpair.mod.Formula")
check.models.equal(bpair.mod, bpair.mod.Formula, "bpair.mod, bpair.mod.Formula", newdata=bpair.data[1:3,])

set.seed(427)
earth.mod.err <- earth(survived~., data=etitanic, degree=1, nfold=3, keepxy=FALSE)
expect.err(try(plot(earth.mod.err$cv.list[[1]])), "cannot get the original model response")

# test plot.earth with cross-validated models (example from help page)
set.seed(427)
earth.mod.help <- earth(survived~., data=etitanic, trace=1, degree=2, nfold=5, keepxy=TRUE)
print.stripped.earth.model(earth.mod.help, "earth.mod.help")
plot(earth.mod.help) # the full model

# test various options
par(mfrow=c(2,2), mar=c(4, 3.2, 3, 3), mgp=c(1.6, 0.6, 0), cex = 0.8)

plot(earth.mod.help, which=1, main="plot.model.selection\ncol.oof.rsq=c(\"red\", \"green\")",
     col.oof.rsq=c("red", "green"), do.par=F)

plot(earth.mod.help, which=1, col.oof.rsq=0, col.npreds="gray", lty.npreds=1,
     main="col.oof.rsq=0 col.npreds=gray", do.par=F)

plot(earth.mod.help, which=1, main="col.infold.rsq=lightblue",
     col.grsq = 0, col.rsq = NA, col.vline = 0, col.oof.vline = 0,
     col.mean.infold.rsq="blue", col.infold.rsq="lightblue",
     col.mean.oof.rsq="red", col.oof.rsq="pink",
     col.pch.max.oof.rsq="purple", col.pch.cv.rsq=1,
     do.par=F, legend.pos=c(5,0.32))

# expect Warning: cannot plot cross-validation data because keepxy not set in original call to earth
a0 <- earth(Volume ~ ., data = trees, nfold=2)
plot(a0, col.oof.rsq="pink", which=1, do.par=F)

par(org.par)

# test plot.earth.models with cross-validated models
set.seed(428)
earth.mod <- earth(survived ~ ., data=etitanic, nfold=3, keepxy=TRUE)
plot.earth.models(earth.mod$cv.list, main="plot.earth.models with cross validated models")

# test keepxy=2
expect.err(try(plot(earth.mod.help$cv.list[[3]])), "cannot get the original model response (use keepxy=2 in the call to earth)")
expect.err(try(plotmo(earth.mod.help$cv.list[[3]])), "cannot get the original model predictors (use keepxy=2 in the call to earth)")
set.seed(2019)
earth.mod.help.keepxy2 <- earth(survived~., data=etitanic, nfold=3, keepxy=2)
plot(earth.mod.help.keepxy2$cv.list[[3]])
plotmo(earth.mod.help.keepxy2$cv.list[[3]])

# example in earth vignette

library(bootstrap) # just for the "scor" data
set.seed(2) # for fold reproducibility, not strictly necessary
data(scor)
data <- data.frame(y=scor[,3], # didactic canonical data frame
                   x1=scor[,1], x2=scor[,2], x3=scor[,4], x4=scor[,5])

# Build an earth model with cross validation.  Note that keepxy=TRUE
# to retain the cross-validation data for further processing.

mod <- earth(y~., data=data, nfold=5, keepxy=TRUE)
plot(mod, which=1, col.rsq=0, caption="Cross Validated Models")
print(mod$cv.oof.rsq.tab, digits=2) # out-of-fold R-Squareds

# Use the cross-validation results to select the optimum number-of-terms.
# This is the number of terms that gave the max mean RSq on the out-of-fold
# data, as displayed by the vertical dotted red line in the graph.
# (This is criterion (ii) in the next section.  There are other approaches.)

mean.oof.rsq.per.subset <- mod$cv.oof.rsq.tab[nrow(mod$cv.oof.rsq.tab),]

nterms.selected.by.cv <- which.max(mean.oof.rsq.per.subset)

cat("\nnterms selected by GCV (standard earth model):", length(mod$selected.terms),
    "\nnterms selected by CV:                        ", nterms.selected.by.cv, "\n")

# Rebuild the earth model with the desired number of terms (and using
# all the data).
# The penalty=-1 tells earth to ignore the GCV (otherwise earth's usual
# selection-by-min-GCV may return a smaller model than the given nprune).

mod.cv <- earth(y~., data=data, nprune=nterms.selected.by.cv, penalty=-1)

# Test cross validation when calling earth from within a function

formula.global <- Volume ~ .
data.global <- trees
weights.global <- rep(1, length.out=nrow(trees))
weights.global[1] <- 2

lm.weights.local1 <- function() {
   weights.local <- rep(1, length.out=nrow(trees))
   weights.local[1] <- 2
   lm(formula=Volume ~ ., data=trees, weights=weights.local)
}
cat("\n--lm.weights.local1\n")
print(summary(lm.weights.local1()))

earth.weights.local2 <- function() {
   weights.local <- rep(1, length.out=nrow(trees))
   weights.local[1] <- 2
   earth(formula=Volume ~ ., data=trees, linpreds=TRUE, weights=weights.local)
}
cat("\n--earth.weights.local2\n")
print(summary(earth.weights.local2()))

lm.weights.local2 <- function(){
   weights.local <- rep(1, length.out=nrow(trees))
   weights.local[1] <- 2
   lm(formula=formula.global, data=data.global, weights=weights.local)
}
cat("\n--lm.weights.local2\n")
try(lm.weights.local2()) # fails: object 'weights.local' not found

earth.weights.local2 <- function(){
   weights.local <- rep(1, length.out=nrow(trees))
   weights.local[1] <- 2
   earth(formula=formula.global, data=data.global, linpreds=TRUE, weights=weights.local)
}
cat("\n--earth.weights.local2\n")
try(earth.weights.local2()) # fails: object 'weights.local' not found, so does lm (see lm.weights.local2 above)

#--- cross validation tests

earth.cv.1 <- function() {
    set.seed(2017)
    earth(formula=Volume ~ ., data=trees, weights=weights.global, linpreds=TRUE,
         nfold=3)
}
cat("\n--earth.cv.1\n")
print(earth.cv.1())
earth.cv.2 <- function() {
    weights.local <- rep(1, length.out=nrow(trees))
    weights.local[1] <- 2
    set.seed(2017)
    earth(formula=Volume ~ ., data=trees, weights=weights.local, linpreds=TRUE,
          nfold=3)
}
cat("\n--earth.cv.2\n")
print(earth.cv.2())
earth.cv.3 <- function(){
    set.seed(2017)
    earth(formula=formula.global, data=data.global, weights=weights.global, linpreds=TRUE,
          nfold=3)
}
cat("\n--earth.cv.3\n")
print(earth.cv.3())

# earth.cv.4 <- function(){ # fails: object 'weights.local' not found, cf earth.weights.local2 above for simpler example
#    weights.local <- rep(1, length.out=nrow(trees))
#    weights.local[1] <- 2
#    set.seed(2017)
#    earth(formula=formula.global, data=data.global, weights=weights.local, linpreds=TRUE,
#          nfold=3)
# }
# cat("\n--earth.cv.4\n")
# printt(earth.cv.4())

thresh.global <- .002
earth.cv.1 <- function() {
    set.seed(2017)
    earth(formula=Volume ~ ., data=trees, thresh=thresh.global,
         nfold=3)
}
cat("\n--earth.cv.1\n")
print(earth.cv.1())
earth.cv.2 <- function() {
    thresh.local <- .002
    set.seed(2017)
    earth(formula=Volume ~ ., data=trees, thresh=thresh.local,
          nfold=3)
}
cat("\n--earth.cv.2\n")
print(earth.cv.2())
earth.cv.3 <- function(){
    set.seed(2017)
    earth(formula=formula.global, data=data.global, thresh=thresh.global,
          nfold=3)
}
cat("\n--earth.cv.3\n")
print(earth.cv.3())
earth.cv.4 <- function(){
    thresh.local <- .002
    set.seed(2017)
    earth(formula=formula.global, data=data.global, thresh=thresh.local,
          nfold=3)
}
cat("\n--earth.cv.4\n")
print(earth.cv.4())
earth.cv.5 <- function(){
    thresh <- .002
    set.seed(2017)
    earth(formula=formula.global, data=data.global, thresh=thresh,
          nfold=3)
}
cat("\n--earth.cv.5\n")
print(earth.cv.5())

thresh.global <- .002
earth.cv.1 <- function() {
    set.seed(2017)
    earth(formula=Volume ~ ., data=trees, thresh=thresh.global,
          pmethod="cv", nfold=3)
}
cat("\n--earth.cv.1\n")
print(earth.cv.1())
earth.cv.2 <- function() { # fails
    thresh.local <- .002
    set.seed(2017)
    a <- earth(formula=Volume ~ ., data=trees, thresh=thresh.local,
          pmethod="cv", ncross=3, nfold=3, keepxy=TRUE)
    # plot(a, which=1, ylim=c(.7, 1))
    # print(a)
    a
}
cat("\n--earth.cv.2\n")
print(earth.cv.2())
a <- earth.cv.2()
earth.cv.3 <- function(){
    set.seed(2017)
    earth(formula=formula.global, data=data.global, thresh=thresh.global,
          pmethod="cv", nfold=3)
}
cat("\n--earth.cv.3\n")
print(earth.cv.3())
earth.cv.4 <- function(){ # fails
    thresh.local <- .002
    set.seed(2017)
    earth(formula=formula.global, data=data.global, thresh=thresh.local,
          pmethod="cv", nfold=3)
}
cat("\n--earth.cv.4\n")
print(earth.cv.4())
earth.cv.5 <- function(){ # fails
    thresh <- .002
    set.seed(2017)
    earth(formula=formula.global, data=data.global, thresh=thresh,
          pmethod="cv", nfold=3)
}
cat("\n--earth.cv.5\n")
a.cv.5 <- earth.cv.5()
print(a.cv.5)
cat("\n--summary(earth.cv.5)\n")
print(summary(a.cv.5))

source("test.epilog.R")
