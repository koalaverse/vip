# test.bpairs.R:

source("test.prolog.R")
source("check.models.equal.R")
source("check.earth.matches.glm.R")
library(earth)
data(ozone1)
data(trees)
data(etitanic)
options(warn=1) # print warnings as they occur

cat("\n===short and long data===\n")
x.short <- data.frame(x1=c(5,2,2,9,5), x2=c(20,20,30,20,20))
y.short <- data.frame(true=c(1,2,0,2,2), false=c(3,3,1,0,1))
short <- data.frame(x.short, y.short)
cat("short:\n")
print(short)
x.long <- data.frame(x1=c( 5, 5, 5, 5,  2, 2, 2, 2, 2,  2,  9, 9,   5,  5,  5),
                     x2=c(20,20,20,20, 20,20,20,20,20, 30, 20,20,  20, 20, 20))
y.long <- data.frame(true=c(1,0,0,0,  1, 1, 0, 0, 0,  0,  1, 1,   1,  1,  0))
long <- data.frame(x.long, y.long)
cat("long:\n")
print(long)
true.false <- cbind(true=short$true, false=short$false)
weights.long <- c(4, 4, 4, 4, 5, 5, 5, 5, 5, 1, 2, 2, 3, 3, 3)

elong <- earth(true~x1+x2, data=long, glm=list(family="binomial"),
               linpreds=TRUE, thresh=0, penalty=-1, trace=1)
glong <- glm(true~x1+x2, data=long, family="binomial")
check.earth.matches.glm(elong, glong)
par(mfrow=c(2,2))
plotres(elong, do.par=0, which=c(1,3), main="elong", legend.pos="topleft")
empty.plot()
plotres(glong, do.par=0, which=3, main="glong")
par(mfrow=c(2,2))
plotmo(elong, do.par=0)
plotmo(glong, do.par=0)
par(org.par)

eshort <- earth(true.false~x1+x2, data=short, glm=list(family="binomial"),
                linpreds=TRUE, thresh=0, penalty=-1, trace=2)
gshort <- glm(true.false~x1+x2, data=short, family="binomial")
OLD.EARTH <- FALSE # earth prior to version 5.0.0
MAX.ARG <- if(OLD.EARTH) 1e-6 else 1e-8
check.earth.matches.glm(eshort, gshort, max=MAX.ARG)
par(mfrow=c(2,2))
plotres(eshort, do.par=0, which=c(1,3), main="eshort", legend.pos="topleft")
empty.plot()
plotres(gshort, do.par=0, which=3, main="gshort")
par(mfrow=c(2,2))
plotmo(eshort, do.par=0)
plotmo(gshort, do.par=0)
par(org.par)

par(mfrow=c(2,2))
plot(elong, main="elong: Model Selection", which=c(1, 3), do.par=0, legend.pos="topleft")
plot(eshort, main="eshort: Model Selection", which=c(1, 3), do.par=0, legend.pos="topleft")
par(org.par)

cat("\n===long data with weights ===\n")
elong.weights <- earth(true~x1+x2, data=long, glm=list(family="binomial"),
                       weights=weights.long, trace=1,
                       linpreds=TRUE, thresh=0, penalty=-1)
print(summary(elong.weights))
glong.weights <- glm(true~x1+x2, data=long, family="binomial",
                    weights=weights.long)
# models match here but in general models with long and short data won't match
check.earth.matches.glm(elong.weights, glong.weights)
# compare "earth" part of earth-glm model to lm
lm.long.weights <- lm(true~x1+x2, data=long, weights=weights.long)
stopifnot(identical(sort(names(coef(elong.weights))), sort(names(coef(lm.long.weights)))))
stopifnot(identical(sort(coef(elong.weights, type="earth")), sort(coef(lm.long.weights))))

cat("\n===short data with weights ===\n")
# add an extra row to prevent singularities in glm with a zero weight
short6 <- rbind(short, list(x1=9, x2=10, true=1, false=1))
true.false6 <- rbind(true.false, c(1,1))
weights.short6 <- sqrt(1:6)
cat("weights.short6:\n")
print(weights.short6)
eshort.weights6 <- earth(true.false6~x1+x2, data=short6, glm=list(family="binomial"),
                        weights=weights.short6,
                        trace=1,
                        linpreds=TRUE, thresh=0, penalty=-1)
print(summary(eshort.weights6))
gshort.weights6 <- glm(true.false6~x1+x2, data=short6, family="binomial",
                      weights=weights.short6)
print(summary(gshort.weights6))
check.earth.matches.glm(eshort.weights6, gshort.weights6, max=1e-6, max.residuals=1e-10)

# unweighted (because all weights equal)
cat("weights.short6.reciprocal.of.rowsums:\n")
eshort.weights6.reciprocal.of.rowsums <- earth(true.false6~x1+x2, data=short6, glm=list(family="binomial"),
                        weights=1/rowSums(true.false6),
                        trace=1,
                        linpreds=TRUE, thresh=0, penalty=-1)
print(summary(eshort.weights6.reciprocal.of.rowsums))
gshort.weights6.reciprocal.of.rowsums <- glm(true.false6~x1+x2, data=short6, family="binomial",
                         weights=1/rowSums(true.false6))
print(summary(gshort.weights6.reciprocal.of.rowsums))
check.earth.matches.glm(eshort.weights6.reciprocal.of.rowsums, gshort.weights6.reciprocal.of.rowsums, max=1e-6, max.residuals=1e-10)

weights.short6zero <- sqrt(1:6)
weights.short6zero[3] <- 0
cat("weights.short6zero:\n")
print(weights.short6zero)
eshort.weights6zero <- earth(true.false6~x1+x2, data=short6, glm=list(family="binomial"),
                        weights=weights.short6zero,
                        trace=1,
                        linpreds=TRUE, thresh=0, penalty=-1)
print(summary(eshort.weights6zero))
gshort.weights6zero <- glm(true.false6~x1+x2, data=short6, family="binomial",
                      weights=weights.short6zero)
print(summary(gshort.weights6zero))
# max.residuals has to be big because of the way earth handles zero weights
check.earth.matches.glm(eshort.weights6zero, gshort.weights6zero)

cat("\n===short and long data with hinges===\n")
# test without linpreds=TRUE (to avoid int-only model, need thresh=0, penalty=-1)
elong.hinge <- earth(true~x1+x2, data=long, glm=list(family="binomial"),
                     thresh=0, penalty=-1)
print(summary(elong.hinge))
eshort.hinge <- earth(true.false~x1+x2, data=short, glm=list(family="binomial"),
                      thresh=0, penalty=-1)
print(summary(eshort.hinge))
eshort.hinge2 <- earth(true+false~x1+x2, data=short, glm=list(family="binomial"),
                      thresh=0, penalty=-1)
check.models.equal(eshort.hinge, eshort.hinge2, "eshort.hinge, eshort.hinge2, ", newdata=short[2:3,])
if(OLD.EARTH) {
    stopifnot(identical(eshort.hinge$dirs[order(rownames(eshort.hinge$dirs)),],
                        elong.hinge$dirs [order(rownames(elong.hinge$dirs)),]))
} else
    stopifnot(identical(eshort.hinge$dirs, elong.hinge$dirs))

par(mfrow=c(2,2))
plotres(elong.hinge, do.par=0, which=c(1,3), main="elong.hinge", legend.pos="topleft")
plotres(eshort.hinge, do.par=0, which=c(1,3), main="eshort.hinge", legend.pos="topleft")

par(mfrow=c(2,2))
plotmo(elong.hinge, do.par=0, ndiscrete=0)
plotmo(eshort.hinge, do.par=0, ndiscrete=0)
par(org.par)

# test with a y with a binomial pair row with both entries equal to 0
x.short.with.zeros <- data.frame(x1=c(5,2,2,9,5,9), x2=c(20,20,30,20,20,30))
y.short.with.zeros <- data.frame(true=c(1,2,0,2,2,0), false=c(3,3,1,0,1,0))
short.with.zeros <- data.frame(x.short.with.zeros, y.short.with.zeros)
true.false.with.zeros <- cbind(true=short.with.zeros$true, false=short.with.zeros$false)
eshort.with.zeros <- earth(true.false.with.zeros~x1+x2, data=short.with.zeros, glm=list(family="binomial"),
                           linpreds=TRUE, thresh=0, penalty=-1)
gshort.with.zeros <- glm(true.false.with.zeros~x1+x2, data=short.with.zeros, family="binomial")
check.earth.matches.glm(eshort.with.zeros, gshort.with.zeros)
par(mfrow=c(2,2))
plotres(eshort.with.zeros, do.par=0, which=c(1,3), main="eshort.with.zeros", legend.pos="topleft")
empty.plot()
plotres(gshort.with.zeros, do.par=0, which=3, main="gshort.with.zeros")
par(mfrow=c(2,2))
plotmo(eshort.with.zeros, do.par=0, ndiscrete=0)
plotmo(gshort.with.zeros, do.par=0, ndiscrete=0)
par(org.par)
eshort.with.zeros.plus <- earth(true+false~x1+x2, data=short.with.zeros, glm=list(family="binomial"),
                                linpreds=TRUE, thresh=0, penalty=-1, trace=1)
check.models.equal(eshort.with.zeros, eshort.with.zeros.plus, "eshort.with.zeros, eshort.with.zeros.plus", newdata=short.with.zeros[2:3,])

eshort.with.zeros.plus.quasibinomial <- earth(true+false~x1+x2, data=short.with.zeros, glm=list(family="quasibinomial"),
                                linpreds=TRUE, thresh=0, penalty=-1, trace=1)
check.models.equal(eshort.with.zeros.plus, eshort.with.zeros.plus.quasibinomial, "eshort.with.zeros.plus eshort.with.zeros.plus.quasibinomial", newdata=short.with.zeros[1:3,])
# print(summary(eshort.with.zeros.plus))
# print(summary(eshort.with.zeros.plus.quasibinomial))
# print(summary(eshort.with.zeros.plus$glm.list[[1]]))
# print(summary(eshort.with.zeros.plus.quasibinomial$glm.list[[1]]))

cat("\n===compare with model where yfrac is generated manually===\n")
bpairs.frac <- function(y)
{
    stopifnot(NCOL(y) == 2)         # binomial pairs y has two columns
    stopifnot(is.numeric(y[,1]) ||is.logical(y[,1]))
    stopifnot(is.numeric(y[,2]) ||is.logical(y[,2]))
    stopifnot(all(y >= 0))          # all y values non-negative
    stopifnot(round(y) == y)        # all y values integers
    weights <- y[,1] + y[,2]
    if(length(weights > 1) == 0)
        warning("no rows of y sum to greater than 1 (earth will not consider y to be a binomial pair")
    y[weights == 0, 2] <- 1         # so all-zero rows will be treated as fraction=0
    # we return y as a one column mat (not a vector) so we can give it a colname
    frac <- matrix(y[, 1] / (y[,1] + y[,2]), ncol=1) # fraction true
    colnames(frac) <- colnames(y)[1]
    nchar <- nchar(colnames(frac))
    if(length(nchar) == 0 || nchar == 0)
        colnames(frac) <- "frac"
    list(frac=frac, weights=weights)
}
ret <- bpairs.frac(cbind(short.with.zeros$true, short.with.zeros$false))
print(ret)
stopifnot(identical(colnames(ret$frac), "frac")) # column name added automatically
ret <- bpairs.frac(short.with.zeros[,c("true", "false")])
print(ret)
stopifnot(identical(colnames(ret$frac), "true"))
frac <- ret$frac
weights <- ret$weights
options(warn=2)
# expect warning: non-integer #successes in a binomial glm
expect.err(try(earth(frac~x1+x2, data=short.with.zeros, glm=list(family="binomial"),
               linpreds=TRUE, thresh=0, penalty=-1)), "non-integer #successes in a binomial glm")
# warning goes away if we use quasibinomial
eshort.with.zeros.frac.quasibinomial <- earth(frac~x1+x2, data=short.with.zeros, weights=weights, glm=list(family="quasibinomial"),
                                linpreds=TRUE, thresh=0, penalty=-1, trace=1)
options(warn=1)
check.models.equal(eshort.with.zeros.frac.quasibinomial, eshort.with.zeros.plus, "eshort.frac, eshort.with.zeros.plus", newdata=short.with.zeros[2:3,], allow.different.names=TRUE)
eshort.with.zeros.frac.binomial <- earth(frac~x1+x2, data=short.with.zeros, weights=weights, glm=list(family="binomial"),
                                         linpreds=TRUE, thresh=0, penalty=-1)
# # compare stats like deviance etc (all identical here except no AIC for quasibinomial,
# #                                  and standard deviations of glm submodels differ)
# cat("eshort.with.zeros.frac.binomial:\n")
# print(summary(eshort.with.zeros.frac.binomial))
# cat("eshort.with.zeros.frac.quasibinomial:\n")
# print(summary(eshort.with.zeros.frac.quasibinomial))
# cat("---------------------------------------------------\n")
# print(summary(eshort.with.zeros.frac.binomial$glm.list[[1]]))
# print(summary(eshort.with.zeros.frac.quasibinomial$glm.list[[1]]))

# lizard data used in McCullagh and Nelder GLM book (2nd ed)
# this has an entry with both responses equal to zero (similar to the above data):
#    site.shade diameter.wide height.tall  time grahami opalinus
# 11      FALSE          TRUE        TRUE   Mid       0        0

cat("\n===lizards===\n")

shade <- factor(x=c(
    "sun", "sun", "sun", "sun", "sun", "sun", "sun", "sun", "sun", "sun", "sun", "sun",
    "shade", "shade", "shade", "shade", "shade", "shade", "shade", "shade", "shade", "shade", "shade", "shade"),
    levels=c("sun", "shade"))
diameter.wide <- as.logical(c(
    0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,
    0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1))
height.tall <- as.logical(c(
    0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1,
    0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1))
time <- factor(x=c(
      "Early", "Mid", "Late",
      "Early", "Mid", "Late",
      "Early", "Mid", "Late",
      "Early", "Mid", "Late",
      "Early", "Mid", "Late",
      "Early", "Mid", "Late",
      "Early", "Mid", "Late",
      "Early", "Mid", "Late"),
    levels=c("Early", "Mid", "Late"), ordered=FALSE)
grahami <- c(
    20, 8, 4, 13, 8, 12, 8, 4, 5, 6, 0, 1,
    34, 69, 18, 31, 55, 13, 17, 60, 8, 12, 21, 4)
opalinus <- c(
    2, 1, 4, 0, 0, 0, 3, 1, 3, 0, 0, 1, 11,
    20, 10, 5, 4, 3, 15, 32, 8, 1, 5, 4)
lizards <- data.frame(
    shade=shade,
    wide=diameter.wide,
    tall=height.tall,
    time=time,
    grahami=grahami,
    opalinus=opalinus)

grahami.opalinus <- cbind(grahami=lizards$grahami, opalinus=lizards$opalinus)
eliz <- earth(grahami.opalinus~as.numeric(shade)+wide+tall*time,
              data=lizards, glm=list(family="binomial"),
              linpreds=TRUE, thresh=0, penalty=-1, trace=1)
print(summary(eliz))
eliz.Formula <- earth(grahami+opalinus~as.numeric(shade)+wide+tall*time,
              data=lizards, glm=list(family="binomial"),
              linpreds=TRUE, thresh=0, penalty=-1, trace=1)
print(summary(eliz.Formula))
gliz <- glm(grahami.opalinus~as.numeric(shade)+wide+tall*time,
            data=lizards, family="binomial")
print(summary(gliz))

check.earth.matches.glm(eliz, gliz, newdata=lizards[c(2:5),], max=1e-12)
check.earth.matches.glm(eliz.Formula, gliz, newdata=lizards[c(2:5),], max=1e-12)
print(evimp(eliz))
par(mfrow=c(3,2))
plotres(eliz,         do.par=0, which=c(1,3), main="eliz",         legend.pos="topleft")
plotres(eliz.Formula, do.par=0, which=c(1,3), main="eliz.Formula", legend.pos="topleft")
empty.plot()
plotres(gliz,         do.par=0, which=3, main="gliz")
par(org.par)
plotmo(eliz, ndiscrete=0, SHOWCALL=TRUE)
plotmo(eliz.Formula, ndiscrete=0, SHOWCALL=TRUE)
plotmo(gliz, ndiscrete=0, SHOWCALL=TRUE)

cat("\n===incorrect bpairs (error handling for bad data)===\n")
test.incorrect.bpairs <- function(msg, expect.err, trace, y.short)
{
    printf("\ntest.incorrect.bpairs: %s\n", msg)
    x.short <- data.frame(x1=as.double(1:5))
    short <- data.frame(x.short, y.short)
    true.false <- cbind(true=short$true, false=short$false)
    if(expect.err)
        expect.err(try(earth(true.false~x1, data=short, glm=list(family="binomial"), trace=trace)), "Binomial response (see above): all values should be between 0 and 1, or a binomial pair")
    else
        earth(true.false~x1, data=short, glm=list(family="binomial"), trace=trace)
}
test.incorrect.bpairs("non integral, greater than 1", expect.err=TRUE, trace=1,
                     data.frame(true=as.double(c(0,1,0,1,0)), false=as.double(c(1,0,1,0,1.1))))
test.incorrect.bpairs("non integral but in range 0...1", expect.err=FALSE, trace=1,
                     data.frame(true=as.double(c(0,1,0,1,0)), false=as.double(c(1,0,1,0,.1))))
test.incorrect.bpairs("non integral but in range 0...1", expect.err=FALSE, trace=0,
                     data.frame(true=as.double(c(0,1,0,1,0)), false=as.double(c(1,0,1,0,.1))))
test.incorrect.bpairs("negative value", expect.err=TRUE, trace=1,
                     data.frame(true=as.double(c(0,1,0,1,0)), false=as.double(c(1,0,1,0,-2))))
test.incorrect.bpairs("no rows sum to greater than 1", expect.err=FALSE, trace=1,
                     data.frame(true=as.double(c(0,1,0,1,0)), false=as.double(c(1,0,1,0,0))))
printf("\n")

#--------------------------------------------------------

ldose <- rep(0:5, 2) - 2 # Venables and Ripley 4th edition page 191
sex <- factor(rep(c("male", "female"), times=c(6,6)))
numdead <- c(1,4,9,13,18,20,0,2,6,10,12,16)
numalive = 20 - numdead
pair <- cbind(numalive, numdead)
# following uses formula not Formula
pairmod <- earth(pair ~ sex + ldose, trace=1, pmethod="none",
                  glm=list(family=binomial))
stopifnot(attr(terms(pairmod), "response") == 1)
stopifnot(is.null(attr(terms(pairmod), "Response")))
glm.weights <- 1 * c(.8,1,1,.5,1,1,1,1,1,1,1,1) # will change model slightly
pairmod.weights <- earth(pair ~ sex + ldose, weights=glm.weights,
                         trace=0, pmethod="none",
                         glm=list(family=binomial))
# build a model using a global variables
# following uses Formula not formula because of "+"
pairmod2 <- earth(numalive + numdead ~ sex + ldose, trace=1, pmethod="none",
                   glm=list(family=binomial))
stopifnot(attr(terms(pairmod2), "response") == 0)
stopifnot(attr(terms(pairmod2), "Response") == c(1,2))
check.models.equal(pairmod2, pairmod, "pairmod2, pairmod", newdata=data.frame(sex="male", ldose=3))
plot(pairmod2, info=TRUE, SHOWCALL=TRUE)
pairmod2.weights <- earth(numalive + numdead ~ sex + ldose, weights=glm.weights,
                         trace=0, pmethod="none",
                         glm=list(family=binomial))
plot(pairmod2.weights, info=TRUE, SHOWCALL=TRUE)
check.models.equal(pairmod2.weights, pairmod.weights, "pairmod2.weights, pairmod.weights", newdata=data.frame(sex="male", ldose=3))
plotmo(pairmod, SHOWCALL=TRUE)
plotmo(pairmod2, SHOWCALL=TRUE)

# build a model using a combo of global and data.frame data
df.except.numdead <- data.frame(ldose=ldose, numalive=numalive, sex=sex)
# change global data to invalid values so we can see if we use it by mistake
ldose <- rep(90:95, 2) - 2 # Venables and Ripley 4th edition page 191
sex <- factor(rep(c("a", "be"), times=c(6,6)))
numalive = NA
# following uses Formula not formula because of "+"
pairmod3 <- earth(numalive + numdead ~ sex + ldose, data=df.except.numdead, trace=0, pmethod="none",
                   glm=list(family=binomial))
check.models.equal(pairmod3, pairmod2, "pairmod3, pairmod2", newdata=df.except.numdead[3:4,])
plot(pairmod3, info=TRUE)
plotmo(pairmod3, SHOWCALL=TRUE)

# build a model using only data from a data.frame
df <- data.frame(df.except.numdead, numdead=numdead)
numdead <- 991:992 # invalidate the global data
# following uses Formula not formula because of "+"
pairmod_Formula <- earth(numalive + numdead ~ sex + ldose, data=df, trace=0, pmethod="none",
                   glm=list(family=binomial))
plot(pairmod_Formula, info=TRUE)
check.models.equal(pairmod_Formula, pairmod2, "pairmod_Formula, pairmod2", newdata=df[5:6,])

expect.err(try(earth(20-numdead+numdead ~ sex + ldose, data=df, glm=list(family=binomial))), "invalid model formula in ExtractVars")

# following uses formula not Formula
pairmod_formula <- earth(pair ~ sex + ldose, data=df, trace=0, pmethod="none",
                   glm=list(family=binomial))
stopifnot(attr(terms(pairmod_formula), "response") == 1)
stopifnot(is.null(attr(terms(pairmod_formula), "Response")))
check.models.equal(pairmod_formula, pairmod_Formula, "pairmod_Formula, pairmod2", newdata=df[1:3,])

# subset
# build a model using only data from a data.frame
# following uses Formula not formula because of "+"
subset.middle <- seq(from=2, to=nrow(df)-2)
pairmod_Formula_subset <- earth(numalive + numdead ~ sex + ldose, data=df, subset=subset.middle, trace=0, pmethod="none",
                   glm=list(family=binomial))
plot(pairmod_Formula_subset, info=TRUE)

# following uses formula not Formula
pairmod_formula_subset <- earth(pair ~ sex + ldose, data=df, subset=subset.middle, trace=0, pmethod="none",
                   glm=list(family=binomial))
stopifnot(attr(terms(pairmod_formula_subset), "response") == 1)
stopifnot(is.null(attr(terms(pairmod_formula_subset), "Response")))
check.models.equal(pairmod_formula_subset, pairmod_Formula_subset, "pairmod_Formula_subset, pairmod2", newdata=df[1:3,])
plot(pairmod_formula_subset, info=TRUE)
plotmo(pairmod_Formula_subset, SHOWCALL=TRUE)
plotmo(pairmod_formula_subset, SHOWCALL=TRUE)

# Terms on lhs like I(20-numdead) are not supported in multiple response Formulas
# (else `log(O3)` is included in model matrix if log(O3) is used on lhs of the Formula)
# Tested Sep 2020, problem in Formula package?
expect.err(try(earth(I(20-numdead) + numdead ~ sex + ldose, data=df, trace=1, pmethod="none",
               glm=list(family=binomial))),
           "terms like 'I(20 - numdead)' are not allowed on the LHS of a multiple-response formula")

pairmod6a <- earth(numalive + numdead ~ sex + ldose - sex, data=df, trace=1, pmethod="none")
pairmod6b <- earth(numalive + numdead ~ ldose, data=df, trace=1, pmethod="none")
print(summary(pairmod6a))
plot(pairmod6a, nresponse=1)
plotmo(pairmod6a, nresponse=1)
check.models.equal(pairmod6a, pairmod6b, "pairmod6a, pairmod6b", newdata=df[5:6,])

pairmod7 <- earth(numalive + numdead ~ sex * ldose, data=df, trace=1, pmethod="none")
print(summary(pairmod7))
plot(pairmod7, nresponse=1)
plotmo(pairmod7, nresponse=1)

pairmod8 <- earth(numalive + numdead ~ ., data=df, trace=1, pmethod="none",
                   glm=list(family=binomial))
print(summary(pairmod8))
plot(pairmod8, nresponse=1)
plotmo(pairmod8, nresponse=1)
# following fails because predictors are in a different order in dirs, ok
try(check.models.equal(pairmod8, pairmod2, "pairmod8, pairmod2", newdata=df[5:6,]))
stopifnot(all.equal(sort(coef(pairmod8)), sort(coef(pairmod2)))) # ok
set.seed(2019)
pairmod.cv <- earth(numalive + numdead ~ ., data=df, nfold=2, trace=1, pmethod="none",
                    keepxy=TRUE,
                    glm=list(family=binomial))
check.models.equal(pairmod.cv, pairmod8, "pairmod.cv, pairmod9", newdata=df[3:5,])

# TODO following fails, it shouldn't (the minus sign on the rhs messes things up), cf pairmod6a
try(earth(numalive + numdead ~ . - ldose, data=df))

newdata.dataframe <- df[1,,drop=FALSE] # data.frame
print(newdata.dataframe)
predict.pairmod  <- predict(pairmod, newdata.dataframe)
predict.pairmod2 <- predict(pairmod2, newdata.dataframe)
predict.pairmod_Formula <- predict(pairmod_Formula, newdata.dataframe)
# predict.pairmod5 <- predict(pairmod5, newdata.dataframe)
check.same(predict.pairmod, 2.372412, max=1e-4)
check.same(predict.pairmod2, predict.pairmod, "predict pairmod2,pairmod with newdata.dataframe")
check.same(predict.pairmod_Formula, predict.pairmod, "predict pairmod_Formula,pairmod2 with newdata.dataframe")
# check.same(predict.pairmod5, predict.pairmod, "predict pairmod5,pairmod2 with newdata.dataframe", allow.different.names=TRUE)

newdata.vector <- df[1,,drop=TRUE] # list
print(newdata.vector)
predict.pairmodv  <- predict(pairmod, newdata.vector)
predict.pairmod2v <- predict(pairmod2, newdata.vector)
predict.pairmod_Formulav <- predict(pairmod_Formula, newdata.vector)
# predict.pairmod5v <- predict(pairmod5, newdata.vector)
check.same(predict.pairmodv, 2.372412, max=1e-4)
check.same(predict.pairmod2v, predict.pairmodv, "predict pairmod2,pairmod with newdata.vector")
check.same(predict.pairmod_Formulav, predict.pairmodv, "predict pairmod_Formula,pairmod2 with newdata.vector")
# check.same(predict.pairmod5v, predict.pairmodv, "predict pairmod5,pairmod2 with newdata.vector", allow.different.names=TRUE)

plotmo(pairmod_Formula, SHOWCALL=TRUE)
# plotmo(pairmod5, SHOWCALL=TRUE)
expect.err(try(plotmo(pairmod2)), "cannot get the original model predictors") # because we deleted ldose, numalive, etc.
expect.err(try(plotmo(pairmod2.weights)), "cannot get the original model predictors") # because we deleted ldose, numalive, etc.

expect.err(try(earth(numalive + 20 - numdead ~ sex + ldose, data=df, glm=list(family=binomial))), "Binomial response (see above): all values should be between 0 and 1, or a binomial pair")

cat("\n===vignette short/long data example===\n")
ldose    <- rep(0:5, 2) - 2 # Venables and Ripley 4th edition page 191
sex      <- factor(rep(c("male", "female"), times=c(6,6)))
numdead  <- c(1,4,9,13,18,20,0,2,6,10,12,16)
numalive <- 20 - numdead

glm.short       <- glm(cbind(numalive,numdead) ~ ldose + sex, family=binomial)

earth.short     <- earth(cbind(numalive,numdead)  ~ ldose + sex,
                         glm=list(family=binomial))

earth.short.lin <- earth(cbind(numalive,numdead)  ~ ldose + sex,
                         glm=list(family=binomial),
                         # coerce earth to build a linear (no hinge) model with all vars
                         # (generated model matches the glm.short model above)
                         linpreds=TRUE, thresh=0, penalty=-1)

data.short <- data.frame(numalive, numdead, ldose, sex)

data.long <- expand.bpairs(data.short, c("numalive", "numdead"))
                         # data.long$num.alive will be a fraction 0...1
print(data.long)

glm.long       <- glm(numalive ~ ldose + sex, data=data.long, family=binomial)

earth.long     <- earth(numalive ~ ldose + sex, data=data.long,
                        glm=list(family=binomial))

earth.long.lin <- earth(numalive ~ ldose + sex, data=data.long,
                        glm=list(family=binomial),
                        linpreds=TRUE, thresh=0, penalty=-1)
print(summary(glm.short))
print(summary(earth.short))
print(summary(earth.short.lin))
print(summary(glm.long))
print(summary(earth.long))
print(summary(earth.long.lin))

print(coef(glm.short))
stopifnot(max(coef(earth.short.lin) - coef(glm.short)) < 1e-12)  # same
stopifnot(max(coef(glm.long) - coef(glm.short)) < 1e-12)         # same
stopifnot(max(coef(earth.long.lin) - coef(glm.short)) < 1e-12)   # same
coef(earth.short)     # different
coef(earth.long)      # different

cat("\n===cross validated binomial pair model===\n")
# use a big enough data set for cross validation without negative GRSqs
n2 <- 20
set.seed(2019)
good <- pmax(round(c((1:n2),(n2:1)) + rnorm(2*n2)), 0)
bad  <- pmax(n2 - good, 0)
data <- data.frame(good, bad, x=1:(2 * n2))
set.seed(2020)
earth.cv <- earth(good+bad~., data=data, glm=list(family=binomial), trace=1, nfold=2, keepxy=TRUE)
cat("cross validated model:\n")
print(summary(earth.cv))
cat("first fold model:\n")
print(summary(earth.cv$cv.list[[1]]))
par(mfrow = c(2, 2), mar = c(3, 3, 3, 1), mgp = c(1.5, 0.5, 0))
set.seed(2019)
plotmo(earth.cv, type="earth", pt.col=2, do.par=0)
empty.plot()
plot.earth.models(list(earth.cv, earth.cv$cv.list[[1]], earth.cv$cv.list[[2]]), which=1:2, do.par=0)

# try plotmo on one of the fold models
expect.err(try(plotmo(earth.cv$cv.list[[1]])), "cannot get the original model predictors (use keepxy=2 in the call to earth)")
# can plotmo on a fold model if we use keepxy=2 in call to earth
set.seed(2020)
earth.cv.keepxy2 <- earth(good+bad~., data=data, glm=list(family=binomial), trace=.5, nfold=2, keepxy=2)
plotmo(earth.cv.keepxy2$cv.list[[1]], type="earth", SHOWCALL=TRUE)

source("test.epilog.R")
