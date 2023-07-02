# test.glm.R: tests glm and factors added for earth release 2.0

source("test.prolog.R")
library(earth)
data(ozone1)
data(trees)
data(etitanic)
source("check.models.equal.R")

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

model.count <- 0

show.earth.models <- function(a, nresponse=NA, legend.pos=NULL, ...)
{
    model.name <- deparse(substitute(a))
    cat("\nPrint", model.name, "\n\n")
    print(a)
    cat("\nSummary", model.name, "\n\n")
    print(summary(a))
    model.count <<- model.count + 1
    if (model.count %% 2 == 0) {  # an attempt at trying different parameters without combin explosion.
        cat("\nSummary", model.name, "decomp=\"none\", digits=5, fixed.point=FALSE, details=TRUE\n\n")
        print(summary(a, decomp="none", digits=5, fixed.point=FALSE, details=TRUE))
    } else {
        cat("\nSummary", model.name, "digits=3, details=TRUE\n\n")
        print(summary(a, decomp="none", digits=3, details=TRUE))
    }
    cat("\nevimp", model.name, "\n\n")
    print(evimp(a))
    cat("\nevimp", model.name, "trim=FALSE\n\n")
    ev <- evimp(a, trim=FALSE)
    print(ev)
    plot(a, nresponse=nresponse, legend.pos=legend.pos,
         caption=if(is.na(nresponse)) model.name
                 else paste("Response ", nresponse, ": ", model.name, sep=""))
    plot(ev)
    if (!is.null(a$glm.list)) {
        control <- a$glm.list[[1]]$control
        family <- a$glm.list[[1]]$family
        cat("\nglm params: epsilon", control$epsilon,
            "maxit", control$maxit, "trace", control$trace,
            "family", family$family, "link", family$link, "\n")
    }
    cat("\nplotmo", model.name, "\n")
    if(is.na(nresponse))
        plotmo(a, clip=FALSE)
    else
        plotmo(a, nresponse=nresponse, clip=FALSE)
    cat("-------------------------------------------------------------------------------\n\n")
}

# print contents of earth.model, for sanity checking that all fields are present as usual
# but strip big fields to reduce amount of printing

print.stripped.earth.model <- function(a, model.name)
{
    a$bx <- NULL
    a$fitted.values <- NULL
    a$residuals <- NULL
    cat("print.stripped.earth.model(", model.name, ")\n", sep="")
    print.default(a)
    cat("-------------------------------------------------------------------------------\n\n")
}

# binomial models

ldose  <- rep(0:5, 2) - 2
# ldose1 <- c(0.1, 1.2, 2.3, 3.4, 4.5, 5.6, 0.3, 1.4, 2.5, 3.6, 4.7, 5.8)
ldose1 <- c(0.1, 1.2, 0.1, 1.2, 1.0, 0.1, 0.3, 1.4, 0.1, 1.2, 0.1, 0.9)
sex <- factor(rep(c("male", "female"), times=c(6,6)))
numdead <- c(1,4,9,13,18,20,0,2,6,10,12,16)
numalive=20 - numdead
SF <- cbind(numdead, numalive)
numdead2 <- c(2,3,10,13,19,20,0,3,7,11,13,17)
SF2 <- cbind(numdead2, numalive2=20 - numdead2)

PMETHOD <- "none" # avoid intercept only models
NK <- 6           # avoid infinite GCV models (since pmethod="none")

# single response glm model but with a binomial pair of y columns
cat("a1: single response glm model but with a binomial pair of y columns, with ldose1 data degree=2\n\n")
a1 <-  earth(SF ~ sex + ldose + ldose1, glm=list(family="binomial"), trace=4, pmethod=PMETHOD, nk=NK, degree=2)
show.earth.models(a1, legend.pos="topleft")
printh(evimp(a1, trim=FALSE, sqrt=FALSE))
printh(evimp(a1, trim=FALSE, sqrt=TRUE)) # this tests sqrt param with negative evimps
a1update <- update(a1, trace=0)
check.models.equal(a1, a1update, msg="a1update a1", newdata=data.frame(sex="female", ldose=10, ldose1=11))

# test some different but equivalent glm specs
a1a <-  earth(SF ~ sex + ldose + ldose1, glm=list(family="binomial"), trace=1, pmethod=PMETHOD, nk=NK, degree=2)
check.models.equal(a1a, a1, msg="a1 a1a", newdata=data.frame(sex="female", ldose=10, ldose1=11))
a1b <-  earth(SF ~ sex + ldose + ldose1, glm=list(family=binomial), trace=1, pmethod=PMETHOD, nk=NK, degree=2)
check.models.equal(a1, a1b, msg="a1 a1b", newdata=data.frame(sex="female", ldose=10, ldose1=11))
a1c <-  earth(SF ~ sex + ldose + ldose1, glm=list(family=binomial()), trace=1, pmethod=PMETHOD, nk=NK, degree=2)
check.models.equal(a1, a1c, msg="a1 a1c", newdata=data.frame(sex="female", ldose=10, ldose1=11))
a1d <-  earth(SF ~ sex + ldose + ldose1, glm=list(family=binomial(link="logit")), trace=1, pmethod=PMETHOD, nk=NK, degree=2)
check.models.equal(a1, a1d, msg="a1 a1d", newdata=data.frame(sex="female", ldose=10, ldose1=11))
expect.err(try(earth(SF ~ sex + ldose + ldose1, glm=list(family=binomial(link="logit"),offset=NULL), trace=1, pmethod=PMETHOD, nk=NK, degree=2)), "earth: 'offset' is not supported in glm argument to earth")
a1g <-  earth(SF ~ sex + ldose + ldose1, glm=list(family=binomial(link="logit"),control=glm.control()), trace=1, pmethod=PMETHOD, nk=NK, degree=2)
check.models.equal(a1, a1g, msg="a1 a1g", newdata=data.frame(sex="female", ldose=10, ldose1=11))
# following should cause a "did not converge warning" because maxit=2
a1h <-  earth(SF ~ sex + ldose + ldose1, glm=list(family=binomial(link="logit"),control=glm.control(epsilon=1e-8, maxit=2, trace=TRUE)), trace=1, pmethod=PMETHOD, nk=NK, degree=2)
show.earth.models(a1h, legend.pos="topleft") # show non convergence (and maxit)
check.models.equal(a1, a1g, msg="a1 a1h", newdata=data.frame(sex="female", ldose=10, ldose1=11)) # models should still be equal within numeric tolerance
stopifnot(a1h$glm.list[[1]]$control$maxit == 2)
# equivalent way of specifying maxit
a1h2 <-  earth(SF ~ sex + ldose + ldose1, glm=list(family=binomial(link="logit"),control=glm.control(epsilon=1e-8),maxit=2), pmethod=PMETHOD, nk=NK, degree=2)
check.models.equal(a1h, a1h2, msg="a1h a1h2", newdata=data.frame(sex="female", ldose=10, ldose1=11))
stopifnot(a1h2$glm.list[[1]]$control$maxit == 2)
expect.err(try(earth(SF ~ sex + ldose + ldose1, family=binomial)), "illegal 'family' argument to earth\nTry something like earth(y~x, glm=list(family=binomial))")
expect.err(try(earth(SF ~ sex + ldose + ldose1, glm=list(family=binomial(link="logit")), maxi=123)), "illegal 'maxit' argument to earth\nTry something like earth(y~x, glm=list(family=binomial, control=list(maxit=99)))")
expect.err(try(earth(SF ~ sex + ldose + ldose1, glm=list(family=binomial(link="logit")), eps=123)), "illegal 'epsilon' argument to earth\nTry something like earth(y~x, glm=list(family=binomial, control=list(epsilon=1e-9)))")
expect.err(try(earth(SF ~ sex + ldose + ldose1, glm=list(family=binomial(link="logit"), weights=1:nrow(SF)))), "earth: illegal 'weights' in 'glm' argument")
expect.err(try(earth(SF ~ sex + ldose + ldose1, glm=list(family=binomial(link="logit"), subset=1:nrow(SF)))), "earth: illegal 'subset' in 'glm' argument")
expect.err(try(earth(SF ~ sex + ldose + ldose1, glm=list(family=binomial(link="logit"), formula=SF~sex))), "earth: illegal 'formula' in 'glm' argument")

plotres(a1h,                  caption="a1h: default type", legend.pos="topleft")
plotres(a1h, type="response", caption="a1h: type=\"response\" (same as default type)", legend.pos="topleft")
plotres(a1h, type="earth",    caption="a1h: type=\"earth\"", legend.pos="topleft")

# check update, also check params are carried forward properly with update
a1h.update1 <- update(a1h, glm=list(family=binomial(link="probit"), maxit=8))
stopifnot(a1h.update1$glm.list[[1]]$control$maxit == 8)
show.earth.models(a1h.update1, legend.pos="topleft")
a1h.update2 <- update(a1h, glm=list(family=gaussian, maxit=9), degree=1)
stopifnot(a1h.update2$glm.list[[1]]$control$maxit == 9)
show.earth.models(a1h.update2, nresponse="numdea", legend.pos="topleft")

# basic check with an I in formula
a1i <-  earth(SF ~ sex + ldose + I(ldose1-3), glm=list(family="binomial"), trace=1, pmethod=PMETHOD, nk=NK, degree=2)
print(summary(a1i))

cat("a2: single response glm model but with a binomial pair of y columns, degree=1\n\n")
a2 <-  earth(SF ~ sex*ldose, glm=list(fa="b"), trace=3, pmethod=PMETHOD)
show.earth.models(a2, legend.pos="topleft")
# repeat with bpairs arg
a2a <- earth(SF ~ sex*ldose, glm=list(family="binomial", bpairs=c(TRUE,FALSE)), trace=3, pmethod=PMETHOD)
stopifnot(identical(a2$glm.list[[1]]$coefficients, a2a$glm.list[[1]]$coefficients))
stopifnot(isTRUE(all.equal(coef(a2), coefficients(a2))))
stopifnot(isTRUE(all.equal(coef(a2, type="glm"), coefficients(a2, type="glm"))))
stopifnot(isTRUE(all.equal(coef(a2, type="earth"), coefficients(a2, type="earth"))))
stopifnot(identical(names(coef(a2)), rownames(a2$coefficients)))
stopifnot(identical(names(coef(a2)), rownames(a2$glm.coefficients)))
stopifnot(identical(names(coef(a2, type="glm")), rownames(a2$glm.coefficients)))
stopifnot(max(abs(coef(a2) - a2$glm.coefficients)) == 0)
stopifnot(max(abs(coef(a2, type="earth") - a2$coefficients)) == 0)
stopifnot(max(abs(coef(a2) - a2$glm.list[[1]]$coefficients)) == 0)
a2b <- earth(numdead+numalive~sex*ldose, glm=list(family="binomial"), pmethod=PMETHOD)
predict.a2 <- predict(a2,newdata=data.frame(sex=sex[1],ldose=3))
predict.a2a <- predict(a2a,newdata=data.frame(sex=sex[1],ldose=3))
predict.a2b <- predict(a2b,newdata=data.frame(sex=sex[1],ldose=3))
stopifnot(identical(predict.a2a, predict.a2))
stopifnot(identical(predict.a2b, predict.a2))

a2c <- earth(SF ~ sex, glm=list(family="binomial"), trace=0, pmethod=PMETHOD)
a2update <- update(a2, SF ~ sex, trace=0)
check.models.equal(a2c, a2update, msg="a2c a2update", newdata=data.frame(sex="female", ldose=10, ldose1=11))

# build a standard GLM model for comparison
cat("a3: direct GLM a3:\n\n")
a3 <- glm(SF ~ sex * ldose, family="binomial")
print(summary(a3))
plotmo(a3, caption="a3 <- glm(SF ~ sex * ldose, family=\"binomial\")")
cat("-------------------------------------------------------------------------------\n\n")

# double response glm model with two binomial paired cols
SF.both <- cbind(SF, SF2)
cat("a4: double response glm model with two binomial paired cols\n\n")
expect.err(try(earth(SF.both ~ sex*ldose, linpreds=TRUE, glm=list(family="binomial"), trace=1)), "Binomial response (see above): all values should be between 0 and 1, or a binomial pair")

# titanic data, multiple responses (i.e. 3 level factor)
cat("a5: titanic data, multiple responses (i.e. 3 level factor)\n\n")
a5 <- earth(pclass ~ ., data=etitanic, degree=2, glm=list(family="binomial"), trace=0)
show.earth.models(a5, nresponse=1)
printh(a5$levels)
print.stripped.earth.model(a5, "a5")
# variance models are not supported for multiple response models
expect.err(try(earth(pclass ~ ., data=etitanic, ncross=3, nfold=3, varmod.method="lm")), "variance models are not supported for multiple response models")

a5d <- earth(pclass ~ .-age, data=etitanic, degree=2, glm=list(family="binomial"), trace=0)
a5update <- update(a5, form=pclass ~ .-age)
check.models.equal(a5update, a5d, msg="a5update a5d", newdata=etitanic[5,])

a5d <- earth(pclass ~ .-age, data=etitanic, degree=2, glm=list(family="binomial"), trace=0, keepxy=1)
a5update <- update(a5, form=pclass ~ .-age)
check.models.equal(a5update, a5d, msg="a5update a5d with keepxy", newdata=etitanic[5,])

# titanic data, one logical response
cat("a6: titanic data, one logical response\n\n")
pclass1 = (etitanic[,1] == "1st")
a6 <- earth(pclass1 ~ ., data=etitanic[,-1], degree=2, glm=list(family="binomial"), trace=1)
show.earth.models(a6)
printh(a6$levels) # expect levels to be NULL
print.stripped.earth.model(a6, "a6")

# titanic data, one response which is a two level factor
cat("a7: titanic data, one response which is a two level factor\n\n")
a7 <- earth(sex ~ ., data=etitanic, degree=2, glm=list(family="binomial"), trace=1)
show.earth.models(a7, nresponse=1)
printh(a7$levels)
print.stripped.earth.model(a7, "a7")

expect.err(try(earth(sex ~ ., data=etitanic, nfold=2,       # earth.formula
               subset=rep(TRUE, length.out=nrow(etitanic)))),
           "'subset' cannot be used with 'nfold' (implementation restriction)")

expect.err(try(earth(etitanic$age, etitanic$sex, nfold=2,   # earth.default
               subset=rep(TRUE, length.out=nrow(etitanic)))),
           "'subset' cannot be used with 'nfold' (implementation restriction)")

cat("glm.varmod: titanic data, one response which is a two level factor, with varmod and plotmo\n\n")
set.seed(2020)
glm.varmod <- earth(sex ~ pclass+age+sibsp, data=etitanic, glm=list(family="binomial"), trace=.5,
                  nfold=5, ncross=3, varmod.method="lm")
cat("\nprint(glm.varmod)\n")
print(glm.varmod)
cat("\nsummary(glm.varmod)\n")
print(summary(glm.varmod))
plotmo(glm.varmod, type="earth", level=.8, ylim=c(-1, 2), SHOWCALL=TRUE)
options(warn=2)
expect.err(try(plotmo(glm.varmod, leve=.8)),                 "predict.earth: with earth-glm models, use type=\"earth\" when using the interval argument")
expect.err(try(plotmo(glm.varmod, lev=.8, type="response")), "predict.earth: with earth-glm models, use type=\"earth\" when using the interval argument")
options(warn=1)

a7d <- earth(sex ~ .-pclass, data=etitanic, degree=2, glm=list(family="binomial"), trace=0)
a7dupdate <- update(a7, form=sex ~ .-pclass)
check.models.equal(a7dupdate, a7d, msg="a7update a7d", newdata=etitanic[5,])
printh(a7d$levels)

a7d1 <- earth(sex ~ .-pclass, data=etitanic, degree=2, glm=list(family="binomial"), trace=0, keepxy=1)
a7d1update <- update(a7, form=sex ~ .-pclass)
check.models.equal(a7d1update, a7d1, msg="a7update a7d1 with keepxy", newdata=etitanic[5,])

subset. <- rep(TRUE, nrow(etitanic))
subset.[1:10] <- FALSE
a7e <- earth(sex ~ ., subset=subset., data=etitanic, degree=2, glm=list(family="binomial"), trace=0)
a7eupdate <- update(a7, subset=subset.)
check.models.equal(a7eupdate, a7e, msg="a7update a7e", newdata=etitanic[5,])

subset. <- 1:nrow(etitanic) # another way of specifying a subset
subset.[1:10] <- 0
a7eeupdate <- update(a7, subset=subset.)
check.models.equal(a7eeupdate, a7e, msg="a7update a7e with alternative subset", newdata=etitanic[5,])

a7f <- earth(sex ~ ., data=etitanic, degree=2, glm=list(family=binomial(link="probit")), trace=0)
a7fupdate <- update(a7, glm=list(family=binomial(link="probit")))
check.models.equal(a7fupdate, a7f, msg="a7update a7f with link=probit", newdata=etitanic[5,])

a7 <- earth(sex ~ ., data=etitanic, degree=2, glm=list(family="binomial"), keepxy=1)
a7g <- earth(sex ~ ., data=etitanic, degree=2, glm=list(family=binomial(link="probit")), trace=0)
a7gupdate <- update(a7, glm=list(family=binomial(link="probit")), trace=1)
check.models.equal(a7gupdate, a7g, msg="a7update a7g with link=probit and keepxy", newdata=etitanic[5,])

a8 <- earth(sex ~ ., data=etitanic, degree=2, glm=list(family="binomial"), keepxy=1)
a8g <- earth(sex ~ ., data=etitanic[100:900,], degree=2, glm=list(family=binomial), trace=0)
a8gupdate <- update(a8, data=etitanic[100:900,], trace=1)
check.models.equal(a8gupdate, a8g, msg="a8update a8g with new data", newdata=etitanic[5,])

# poisson models

counts <- c(18,17,15,20,10,20,25,13,12)
counts2 <- c(181,171,151,201,101,201,251,131,121)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
d.AD <- data.frame(treatment, outcome, counts, counts2)

# one response poisson model
cat("a8p: one response poisson model\n\n")
a8p <- earth(counts ~ outcome + treatment, glm=list(family=poisson()), trace=3, pmethod=PMETHOD)
show.earth.models(a8p, legend.pos="topleft")
# build a standard GLM model for comparison
cat("a9: one response poisson model, standard GLM model for comparison\n\n")
a9 <- glm(counts ~ outcome + treatment, family="poisson")
cat("Direct GLM a9 summary:\n\n")
print(summary(a9))
plotmo(a9, grid.levels=list(outcome="2"), caption="a9 <- glm(counts ~ outcome + treatment, family=\"poisson\")")

# two response poisson model
cat("a10: two response poisson model\n\n")
a10 <- earth(cbind(counts, counts2) ~ outcome + treatment, glm=list(fam="po"), trace=1, pmethod=PMETHOD)
show.earth.models(a10, nresponse="counts")

# compare family=gaussian to standard earth model
cat("a11: compare family=gaussian to standard earth model\n\n")
a11 <- earth(etitanic$sex ~ ., data=etitanic, degree=2, glm=list(family="gaussian"), trace=4)
cat("\nsummary(a11, details=TRUE)\n\n", sep="")
print(summary(a11, details=TRUE))
stopifnot(identical(a11$coefficients, a11$glm.coefficients))
cat("-------------------------------------------------------------------------------\n\n")

cat("a12: compare family=gaussian to standard earth model with two responses\n\n")
a12 <- earth(cbind(etitanic$sex, (as.integer(etitanic$age)^2)) ~ ., data=etitanic, degree=2, glm=list(family="gaussian"), trace=4)
cat("\nsummary(a12, details=TRUE)\n\n", sep="")
print(summary(a12, details=TRUE))
stopifnot(identical(a12$coefficients, a12$glm.coefficients))

# test to see how standard model.matrix does column numbering with formula

my.x1 <- as.numeric(ToothGrowth[,2]) # supp was VC or OJ
my.x2 <- as.numeric(ToothGrowth[,3]) # dose
my.input.mat <- cbind(my.x1, my.x2)
my.response <- ToothGrowth[,1]
a13 <- earth(my.response~my.input.mat, trace=1)
print(summary(a13, details=TRUE))

stop.if.not.identical <- function(msg, a, b)
{
    if(!identical(a, b)) {
       cat(msg, "not identical\n")
       cat(deparse(substitute(a)), ":\n", sep="")
       print(a)
       cat(deparse(substitute(b)), ":\n", sep="")
       print(b)
       stop("test failed")
    }
    cat(msg, "identical\n")
}

# some matrix interface tests

# double response glm model with two binomial paired cols
SF.both <- cbind(SF, SF2)
df <- data.frame(sex, ldose)
expect.err(try(earth(SF.both ~ ., data=df, glm=list(family="binomial"), trace=1)), "Binomial response (see above): all values should be between 0 and 1, or a binomial pair")

# --- predict with factors ------------------------------------------------------

# there is a lot of redundancy in this routine, it doesn't really matter

test.predict.with.factors <- function(trace)
{
    cat("\n--- predict with single level factors and a single response, trace=", trace,
        " ---\n\n", sep="")

    cat("first do a quick test of predict.earth help page example\n")
    a <- earth(Volume ~ ., data = trees)
    if (trace) print(head(predict(a, trace=trace)))
    if (trace) print(predict(a, c(10,80), trace=trace))

    # test set A: prepare the data

    ldose  <- rep(0:5, 2) - 2
    # ldose1 <- c(0.1, 1.2, 2.3, 3.4, 4.5, 5.6, 0.3, 1.4, 2.5, 3.6, 4.7, 5.8)
    ldose1 <- c(0.1, 1.2, 0.1, 1.2, 1.0, 0.1, 0.3, 1.4, 0.1, 1.2, 0.1, 0.9)
    sex <- factor(rep(c("male", "female"), times=c(6,6)))
    numdead <- c(1,4,9,13,18,20,0,2,6,10,12,16)

    sexmale <- (sex == "male")
    cat("sexmale:\n"); print(sexmale)
    am <-  earth(cbind(sexmale, ldose, ldose1), numdead, trace=trace, pmethod=PMETHOD, nk=NK, degree=2)
    af <-  earth(numdead ~ sex + ldose + ldose1, trace=trace, pmethod=PMETHOD, nk=NK, degree=2)
    check.models.equal(am, af, "predict with single level factors and a single response")

    cat("A-20m head(predict(am, trace=", trace, ")\n", sep="")
    pm <- predict(am, trace=trace)
    if (trace) print(head(pm))

    cat("A-20f head(predict(af, trace=", trace, ")\n", sep="")
    pf <- predict(af, trace=trace)
    if (trace) print(head(pf))
    stop.if.not.identical("A-20", pm, pf)

    cat("A-21m predict(am, newdata=c(sex[1], -2, 0.1), trace=", trace, "))\n", sep="")
    pm <- predict(am, newdata=c(sex[1]=="male", -2, 0.1), trace=trace)
    pm.ref <- pm
    if (trace) print(pm)

    cat("A-21f predict(af, newdata=c(sex[1], -2, 0.1), trace=", trace, "))\n", sep="")
    pf <- predict(af, newdata=c(sex[1]=="male", -2, 0.1), trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-21", pm.ref, pf)

    cat("A-22m predict(am, newdata=c(1, -2, 0.1), trace=", trace, ")) use numeric instead of factor sex\n", sep="")
    pm <- predict(am, newdata=c(1, -2, 0.1), trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("A-22", pm.ref, pm)

    cat("A-22f predict(af, newdata=c(1, -2, 0.1), trace=", trace, ")) use numeric instead of factor sex\n", sep="")
    pf <- predict(af, newdata=c(1, -2, 0.1), trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-22", pm, pf)

    cat("A-23m predict(am, newdata=c(sex[1], sex[1], -2, -2, 0.1, 0.1), trace=", trace, ")) multiple rows as a vec\n", sep="")
    pm <- predict(am, newdata=c(sex[1], sex[1], -2, -2, 0.1, 0.1), trace=trace)
    if (trace) print(pm)

    cat("A-23f predict(af, newdata=c(sex[1], sex[1], -2, -2, 0.1, 0.1), trace=", trace, ")) multiple rows as a vec\n", sep="")
    pf <- predict(af, newdata=c(sex[1], sex[1], -2, -2, 0.1, 0.1), trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-23", pm, pf)

    cat("A-24m predict(am, newdata=c(sex[1], sex[1], -2, -1, 0.1, 0.1), trace=", trace, ")) more multiple rows as a vec\n", sep="")
    pm <- predict(am, newdata=c(sex[1], sex[1], -2, -1, 0.1, 0.1), trace=trace)
    if (trace) print(pm)
    pm2.ref <- pm

    cat("A-24f predict(af, newdata=c(sex[1], sex[1], -2, -1, 0.1, 0.1), trace=", trace, ")) more multiple rows as a vec\n", sep="")
    pf <- predict(af, newdata=c(sex[1], sex[1], -2, -1, 0.1, 0.1), trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-24", pm, pf)

    cat("A-25m predict(am, xpredict matrix trace=", trace, "\n", sep="")
    new.data <- matrix(c(sex[1], sex[1], -2, -1, 0.1, 0.1), nrow=2)
    pm <- predict(am, newdata=new.data, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("A-25", pm2.ref, pm)

    cat("A-25f predict(af, xpredict matrix trace=", trace, "\n", sep="")
    pf <- predict(af, newdata=new.data, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-25", pm, pf)

    cat("A-26m predict(am, new.data with col names) trace=", trace, "\n", sep="")
    new.data <- matrix(c(sex[1], sex[1], -2, -1, 0.1, 0.1), nrow=2)
    colnames(new.data) <- c("sex", "ldose", "ldose1")
    pm <- predict(am, newdata=new.data, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("A-26m", pm2.ref, pm)

    cat("A-26f predict(af, new.data with col names) trace=", trace, "\n", sep="")
    pf <- predict(af, newdata=new.data, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-26f", pm, pf)

    cat("A-27m predict(am, new.data with out of order col names) trace=", trace, "\n", sep="")
    new.data <- matrix(c(sex[1], sex[1], 0.1, 0.1, -2, -1), nrow=2)
    colnames(new.data) <- c("sex", "ldose1", "ldose")
    pm <- predict(am, newdata=new.data, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("A-27", pm2.ref, pm)

    cat("A-27f predict(af, new.data with out of order col names) trace=", trace, "\n", sep="")
    pf <- predict(af, newdata=new.data, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-27", pm, pf)

    cat("A-28m predict(am, xdata.frame without col names) trace=", trace, "\n", sep="")
    if (trace) print(pm)
    stop.if.not.identical("A-28m", pm2.ref, pm)

    # Jun 2021: with R 4.1.0 no longer works, probably ok (old version of R gave err/warn msgs)
    #           something to do with change in how ordered factors are handled in model frames
    #
    # cat("A-28f predict(af, xdata.frame without col names) trace=", trace, "\n", sep="")
    # xdata.frame <- data.frame(c(sex[1], sex[1]), c(-2, -1), c(0.1, 0.1))
    # pf <- predict(af, xdata.frame, trace=trace)
    # if (trace) print(pf)
    # stop.if.not.identical("A-28f", pm, pf)
    #
    # cat("A-29m predict(am, xdata.frame with col names) trace=", trace, "\n", sep="")
    # xdata.frame.29 <- data.frame(sex[1], -2, 0.1)
    # colnames(xdata.frame.29) <- c("sex", "ldose", "ldose1")
    # pm <- predict(am, xdata.frame.29, trace=trace)
    # if (trace) print(pm)
    # stop.if.not.identical("A-29", pm.ref, pm)
    #
    # cat("A-29f predict(af, xdata.frame with col names) trace=", trace, "\n", sep="")
    # pf <- predict(af, xdata.frame.29, trace=trace)
    # if (trace) print(pf)
    # stop.if.not.identical("A-29", pm, pf)
    #
    # cat("A2-29m predict(am, xdata.frame with col names) trace=", trace, "\n", sep="")
    # xdata.frame.29.2 <- data.frame(c(sex[1], sex[1]), c(-2, -1), c(0.1, 0.1))
    # colnames(xdata.frame.29.2) <- c("sex", "ldose", "ldose1")
    # pm <- predict(am, xdata.frame.29.2, trace=trace)
    # if (trace) print(pm)
    # stop.if.not.identical("A2-29m", pm2.ref, pm)
    #
    # cat("A2-29f predict(af, xdata.frame with col names) trace=", trace, "\n", sep="")
    # pf <- predict(af, xdata.frame.29.2, trace=trace)
    # if (trace) print(pf)
    # stop.if.not.identical("A2-29f", pm, pf)

    cat("A-31m predict(am, xdata.frame, trace=", trace, ") data frame with factors and wrong col names\n", sep="")
    xdata.frame <- data.frame(sex[1], -2, 0.1)
    pm <- predict(am, xdata.frame, trace=trace)
    stop.if.not.identical("A-31m", pm.ref, pm)
    if (trace) print(pm)

    cat("A-31f predict(af, xdata.frame, trace=", trace, ") data frame with factors and wrong col names\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-31f", pm, pf)

    cat("A-31bm predict(am, xdata.frame, trace=", trace, ") data frame col names\n", sep="")
    xdata.frame <- data.frame(sex=sex[1], ldose=-2, ldose1=0.1)
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("A-31bm", pm.ref, pm)

    cat("A-31bf predict(af, xdata.frame, trace=", trace, ") data frame col names\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-31bf", pm, pf)

    cat("A-32m predict(am, xdata.frame, trace=", trace, ") # data frame with names\n", sep="")
    xdata.frame <- data.frame(sex[1], -2, 0.1)
    colnames(xdata.frame) <- c("sex", "ldose", "ldose1")
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("A-32m1", pm, pf)
    stop.if.not.identical("A-32m2", pm.ref, pm)

    cat("A-32f predict(af, xdata.frame, trace=", trace, ") # data frame with names\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-32f", pm, pf)

    cat("A-42am predict(am, newdata=c(1, -2, 0.1), trace=", trace, ")) use numeric instead of factor sex\n", sep="")
    pm <- predict(am, newdata=c(1, -2, 0.1), trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("A-42a", pm.ref, pm)

    cat("A-42af predict(af, newdata=c(1, -2, 0.1), trace=", trace, ")) use numeric instead of factor sex\n", sep="")
    pf <- predict(af, newdata=c(1, -2, 0.1), trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-42a", pm, pf)

    cat("A-43am predict(af, newdata=c(sex[1], sex[1], -2, -2, 0.1, 0.1), trace=", trace, ")) multiple rows as a vec\n", sep="")
    pm <- predict(af, newdata=c(sex[1], sex[1], -2, -2, 0.1, 0.1), trace=trace)
    if (trace) print(pm)

    cat("A-43af predict(am, newdata=c(sex[1], sex[1], -2, -2, 0.1, 0.1), trace=", trace, ")) multiple rows as a vec\n", sep="")
    pf <- predict(am, newdata=c(sex[1], sex[1], -2, -2, 0.1, 0.1), trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-43a", pm, pf)

    cat("A-44am predict(af, newdata=c(sex[1], sex[1], -2, -1, 0.1, 0.1), trace=", trace, ")) more multiple rows as a vec\n", sep="")
    pm <- predict(af, newdata=c(sex[1], sex[1], -2, -1, 0.1, 0.1), trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("A-44a", pm2.ref, pm)

    cat("A-44fm predict(am, newdata=c(sex[1], sex[1], -2, -1, 0.1, 0.1), trace=", trace, ")) more multiple rows as a vec\n", sep="")
    pf <- predict(am, newdata=c(sex[1], sex[1], -2, -1, 0.1, 0.1), trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-44f", pm, pf)

    cat("A-53m predict(am, xdata.frame, trace=", trace, ") data frame with not enough columns, expect error message\n", sep="")
    xdata.frame <- data.frame(sex[1], -2)
    expect.err(try(predict(am, xdata.frame, trace=trace)),
"could not interpret newdata\n           model.matrix returned 2 columns: \"sex.1.\", \"X.2\"\n           need 3 columns: \"sexmale\", \"ldose\", \"ldose1\"")

    cat("A-53f predict(af, xdata.frame, trace=", trace, ") data frame with not enough columns, expect error message\n", sep="")
    xdata.frame <- data.frame(sex[1], -2)
    expect.err(try(predict(af, xdata.frame, trace=trace)),
"could not interpret newdata\n           model.matrix returned 2 columns: \"sex.1.\", \"X.2\"\n           need 3 columns: \"sex\", \"ldose\", \"ldose1\"")

    cat("A-54m predict(am, xdata.frame, trace=", trace, ") # data frame with cols in different order\n", sep="")
    xdata.frame <- data.frame(-2, sex[1], 0.1)
    colnames(xdata.frame) <- c("ldose", "sex", "ldose1")
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("A-54", pm.ref, pm)

    cat("A-54f predict(af, xdata.frame, trace=", trace, ") # data frame with cols in different order\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-54", pm, pf)

    cat("A-55m predict(am, xdata.frame, trace=", trace, ") data frame without col names\n", sep="")
    xdata.frame <- data.frame(sex[c(1,7)], c(-2,-1), c(0.1, 0.1))
    pm <- predict(am, xdata.frame, trace=trace)
    pm3.ref <- pm
    if (trace) print(pm)

    cat("A-55f predict(af, xdata.frame, trace=", trace, ") data frame without col names\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-55", pm, pf)

    cat("A-56m predict(am, xdata.frame, trace=", trace, ") # data frame with col names\n", sep="")
    xdata.frame <- data.frame(sex[c(1,7)], c(-2,-1), c(0.1, 0.1))
    colnames(xdata.frame) <- c("sex", "ldose", "ldose1")
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("A-56", pm3.ref, pm)

    cat("A-56f predict(af, xdata.frame, trace=", trace, ") # data frame with col names\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-56", pm, pf)

    cat("A-57m predict(am, xdata.frame, trace=", trace, ") data frame with not enough columns, expect error message\n", sep="")
    xdata.frame <- data.frame(sex[c(1,7)], c(-2,-1))
    expect.err(try(predict(am, xdata.frame, trace=trace)),
"could not interpret newdata\n           model.matrix returned 2 columns: \"sex.c.1..7..\", \"c..2...1.\"\n           need 3 columns: \"sexmale\", \"ldose\", \"ldose1\"")

    cat("A-57f predict(af, xdata.frame, trace=", trace, ") data frame with not enough columns, expect error message\n", sep="")
    expect.err(try(predict(af, xdata.frame, trace=trace)),
"could not interpret newdata\n           model.matrix returned 2 columns: \"sex.c.1..7..\", \"c..2...1.\"\n           need 3 columns: \"sex\", \"ldose\", \"ldose1\"")
    stop.if.not.identical("A-57", pm, pf)

    cat("A-58m predict(am, xdata.frame, trace=", trace, ") # data frame with cols in different order\n", sep="")
    xdata.frame <- data.frame(c(-2,-1), sex[c(1,7)], c(0.1, 0.1))
    colnames(xdata.frame) <- c("ldose", "sex", "ldose1")
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("A-58", pm3.ref, pm)

    cat("A-58f predict(af, xdata.frame, trace=", trace, ") # data frame with cols in different order\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-58", pm, pf)

    cat("A-59m predict(am, xdata.frame, trace=", trace, ") numeric where factor expected, expect forge on message\n", sep="")
    xdata.frame.39 <- data.frame(c(sex[1], sex[7]), c(-2,-1), c(0.1, 0.1))
    colnames(xdata.frame.39) <- c("sex", "ldose", "ldose1")
    pm <- predict(am, xdata.frame.39, trace=trace)
    if (trace) print(pm)
    # stop.if.not.identical("A-59", pm3.ref, pm) # TODO fails but "forge on" message is correctly issued

    cat("A-59f predict(af, xdata.frame, trace=", trace, ") numeric where factor expected, expect forge on message\n", sep="")
    pf <- predict(af, xdata.frame.39, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-59", pm, pf)

    cat("A-50m data frame without column names, trace=", trace, "\n", sep="")
    xdata.frame <- data.frame(sex[1], -2, 0.1)
    colnames(xdata.frame) <- NULL
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("A-34", pm.ref, pm)

    cat("A-60f data frame without column names, trace=", trace, "\n", sep="")
    xdata.frame <- data.frame(sex[1], -2, 0.1)
    colnames(xdata.frame) <- NULL
    pf <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-60", pm, pf)

    cat("A-61f data frame without extra columns, trace=", trace, "\n", sep="")
    xdata.frame <- data.frame(sex=sex[1], extra1=99, ldose=-2, extra2=99, ldose1=0.1, extra3=sex[7])
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-61", pm, pf)

    #-----------------------------------

    my.x1 <- as.numeric(ToothGrowth[,2]) # supp was VC or OJ
    my.x2 <- as.numeric(ToothGrowth[,3]) # dose
    my.input.mat <- cbind(my.x1, my.x2)
    my.response <- ToothGrowth[,1]

    cat("A-68 input matrix to formula interface trace=", trace, ", expect error \"model.matrix.earth could not interpret the data\"\n", sep="")
    a41 <- earth(my.response~my.input.mat, trace=trace)
    expect.err(try(predict(a41, c(2.1, 0.6), trace=trace)),
               "model.matrix.earth could not interpret the data")
    cat("A-69 above test but with properly named dataframe trace=", trace, "\n", sep="")
    df <- data.frame(growth=my.response, supp=my.x1, dose=my.x2)
    a42 <- earth(formula=growth~., data=df, trace=0)
    p <- predict(a42, c(2.1, 0.6), trace=0) # now gives the correct result
    if (trace) print(head(p))

    cat("Tests with not all predictors used in the model so can pass fewer columns\n")
    # No factor tests done, they probably won't work in this setup.

    # first for earth.default
    dummy <- rep(0, 12)
    am <-  earth(cbind(ldose, dummy, ldose1), numdead, trace=trace, pmethod=PMETHOD, nk=NK, degree=2)
    # prepare reference prediction, using all columns
    newdata <- matrix(c(-2, 0, 0.1), ncol=3, nrow=1)
    colnames(newdata) <- c("ldose", "dummy", "ldose1")
    pm.ref <- predict(am, newdata=newdata, trace=trace)
    if (trace) print(pm.ref)

    cat("A-72m predict(am, newdata=newdata[two columns], trace=trace)\n")
    newdata <- matrix(c(-2, 0.1), ncol=2, nrow=1)
    colnames(newdata) <- c("ldose", "ldose1")
    pm <- predict(am, newdata=newdata, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("A-72m", pm, pm.ref)

    # prepare reference prediction, using all columns
    newdata <- data.frame(cbind(ldose, dummy, ldose1))
    print(newdata)
    pm.ref <- predict(am, newdata=newdata, trace=trace)
    if (trace) print(pm.ref)

    cat("A-73m predict(am, newdata=newdata[two columns], trace=trace)\n")
    newdata <- newdata[, c(1,3)]
    pm <- predict(am, newdata=newdata, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("A-73m", pm, pm.ref)

    # now for earth.formula
    dummy <- rep(0, 12)
    af <-  earth(numdead ~ ldose + dummy + ldose1, trace=trace, pmethod=PMETHOD, nk=NK, degree=2)
    # prepare reference prediction, using all columns
    newdata <- matrix(c(-2, 0, 0.1), ncol=3, nrow=1)
    colnames(newdata) <- c("ldose", "dummy", "ldose1")
    newdata <- as.data.frame(newdata)
    pf.ref <- predict(af, newdata=newdata, trace=trace)
    if (trace) print(pf.ref)

    cat("A-72f predict(af, newdata=newdata[two columns], trace=trace)\n")
    newdata <- matrix(c(-2, 0.1), ncol=2, nrow=1)
    colnames(newdata) <- c("ldose", "ldose1")
    newdata <- as.data.frame(newdata)
    pf <- predict(af, newdata=newdata, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-72f", pf, pf.ref)

    # prepare reference prediction, using all columns
    newdata <- data.frame(cbind(ldose, dummy, ldose1))
    print(newdata)
    pf.ref <- predict(af, newdata=newdata, trace=trace)
    if (trace) print(pf.ref)

    cat("A-73f predict(af, newdata=newdata[two columns], trace=trace)\n")
    newdata <- newdata[, c(1,3)]
    pf <- predict(af, newdata=newdata, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("A-73f", pf, pf.ref)

    cat("\n--- B predict with multiple level factors and a multiple real response, trace=", trace,
        " ---\n\n", sep="")

    # note that we can no now longer get away with using numerics for
    # factors because factors have more than two levels

    # test set B: prepare the data

    ldose  <- rep(0:5, 2) - 2
    ldose1 <- c(0.1, 1.2, 2.3, 3.4, 4.5, 5.6, 0.3, 1.4, 2.5, 3.6, 4.7, 5.8)
    sex3 <- factor(rep(c("male", "female", "andro"), times=c(6,4,2)))
    fac3 <- factor(c("lev2", "lev2", "lev1", "lev1", "lev3", "lev3",
                     "lev2", "lev2", "lev1", "lev1", "lev3", "lev3"))
    numdead <- c(1,4,9,13,18,20,0,2,6,10,12,16)
    numdead2 <- c(2,3,10,13,19,20,0,3,7,11,13,17)
    numdeadboth <- cbind(numdead, numdead2)
    isex <- as.double(sex3) # sex3 as an index
    df <- data.frame(sex3, ldose, ldose1, fac3)
    am <-  earth(df, numdeadboth, trace=trace, pmethod=PMETHOD, nk=NK, degree=2)
    af <-  earth(numdeadboth ~ ., data=df, trace=trace, pmethod=PMETHOD, nk=NK, degree=2)
    check.models.equal(am, af, "B predict with multiple level factors and a multiple real response")
    cat("20m head(predict(am, trace=", trace, ")\n", sep="")
    pm <- predict(am, trace=trace)
    if (trace) print(head(pm))

    cat("B-21f head(predict(af, trace=", trace, ")\n", sep="")
    pf <- predict(af, trace=trace)
    if (trace) print(head(pf))
    stop.if.not.identical("B-20", pm, pf)

    cat("B-31m predict(am, xdata.frame, trace=", trace, ") data frame with factors and wrong col names\n", sep="")
    xdata.frame <- data.frame(sex3[1], -2, 0.1, fac3[1])
    pm <- predict(am, xdata.frame, trace=trace)
    pm.ref <- pm
    stop.if.not.identical("B-31", pm.ref, pm)
    if (trace) print(pm)

    cat("B-31f predict(af, xdata.frame, trace=", trace, ") data frame with factors and wrong col names\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("B-31", pm, pf)

    cat("B-31bm predict(am, xdata.frame, trace=", trace, ") data frame col names\n", sep="")
    xdata.frame <- data.frame(sex3=sex3[1], ldose=-2, ldose1=0.1, fac3=fac3[1])
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("B-31", pm.ref, pm)

    cat("B-31bf predict(af, xdata.frame, trace=", trace, ") data frame col names\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("B-31b", pm, pf)

    cat("B-32m predict(am, xdata.frame, trace=", trace, ") # data frame with names\n", sep="")
    xdata.frame <- data.frame(sex3[1], -2, 0.1, fac3[1])
    colnames(xdata.frame) <- c("sex3", "ldose", "ldose1", "fac3")
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("B-32", pm, pf)
    stop.if.not.identical("B-32", pm.ref, pm)

    cat("B-32f predict(af, xdata.frame, trace=", trace, ") # data frame with names\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("B-32", pm, pf)

    cat("B-53m predict(am, xdata.frame, trace=", trace, ") data frame with not enough columns, expect error message\n", sep="")
    xdata.frame <- data.frame(sex3[1], -2)
    expect.err(try(predict(am, xdata.frame, trace=trace)),
"could not interpret newdata\n           model.matrix returned 2 columns: \"sex3.1.\", \"X.2\"\n           need 4 columns: \"sex3\", \"ldose\", \"ldose1\", \"fac3\"")

    cat("B-53f predict(af, xdata.frame, trace=", trace, ") data frame with not enough columns, expect error message\n", sep="")
    expect.err(try(predict(af, xdata.frame, trace=trace)),
"could not interpret newdata\n           model.matrix returned 2 columns: \"sex3.1.\", \"X.2\"\n           need 4 columns: \"sex3\", \"ldose\", \"ldose1\", \"fac3\"")

    cat("B-54m predict(am, xdata.frame, trace=", trace, ") # data frame with cols in different order\n", sep="")
    xdata.frame <- data.frame(-2, sex3[1], 0.1, fac3[1])
    colnames(xdata.frame) <- c("ldose", "sex3", "ldose1", "fac3")
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("B-54", pm.ref, pm)

    cat("B-54f predict(af, xdata.frame, trace=", trace, ") # data frame with cols in different order\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("B-54", pm, pf)

    cat("B-55m predict(am, xdata.frame, trace=", trace, ") data frame without col names\n", sep="")
    xdata.frame <- data.frame(sex3[c(1,7)], c(-2,-1), c(0.1, 0.1), fac3[c(1,9)])
    pm <- predict(am, xdata.frame, trace=trace)
    pm3.ref <- pm
    if (trace) print(pm)
    cat("B-55m again, but with same x data for both reponses\n")
    xdata.frame <- data.frame(sex3[c(1,1)], c(-2,-2), c(0.1, 0.1), fac3[c(1,1)])
    pm <- predict(am, xdata.frame, trace=trace)
    print(pm)

    cat("B-55f predict(af, xdata.frame, trace=", trace, ") data frame without col names\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("B-55", pm, pf)

    cat("B2-55bm predict(am, xdata.frame, trace=", trace, ") data frame col names\n", sep="")
    xdata.frame <- data.frame(sex3=sex3[c(1,7)], ldose=c(-2,-1), ldose1=c(0.1,0.1), fac3=fac3[c(1,9)])
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("B2-55", pm3.ref, pm)

    cat("B2-55bf predict(af, xdata.frame, trace=", trace, ") data frame col names\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("B2-55b", pm, pf)

    cat("B-56m predict(am, xdata.frame, trace=", trace, ") # data frame with col names\n", sep="")
    xdata.frame <- data.frame(sex3[c(1,7)], c(-2,-1), c(0.1, 0.1), fac3[c(1,9)])
    colnames(xdata.frame) <- c("sex3", "ldose", "ldose1", "fac3")
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("B-56", pm3.ref, pm)

    cat("B-56f predict(af, xdata.frame, trace=", trace, ") # data frame with col names\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("B-56", pm, pf)

    cat("B-57m predict(am, xdata.frame, trace=", trace, ") data frame with not enough columns, expect error message\n", sep="")
    xdata.frame <- data.frame(sex3[c(1,7)], c(-2,-1))
    expect.err(try(predict(am, xdata.frame, trace=trace)),
"could not interpret newdata\n           model.matrix returned 2 columns: \"sex3.c.1..7..\", \"c..2...1.\"\n           need 4 columns: \"sex3\", \"ldose\", \"ldose1\", \"fac3\"")

    cat("B-57f predict(af, xdata.frame, trace=", trace, ") data frame with not enough columns, expect error message\n", sep="")
    expect.err(try(predict(af, xdata.frame, trace=trace)),
"could not interpret newdata\n           model.matrix returned 2 columns: \"sex3.c.1..7..\", \"c..2...1.\"\n           need 4 columns: \"sex3\", \"ldose\", \"ldose1\", \"fac3\"")
    stop.if.not.identical("B-57", pm, pf)

    cat("B-58m predict(am, xdata.frame, trace=", trace, ") # data frame with cols in different order\n", sep="")
    xdata.frame <- data.frame(c(-2,-1), sex3[c(1,7)], c(0.1, 0.1),  fac3[c(1,9)])
    colnames(xdata.frame) <- c("ldose", "sex3", "ldose1", "fac3")
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("B-58", pm3.ref, pm)

    cat("B-58f predict(af, xdata.frame, trace=", trace, ") # data frame with cols in different order\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("B-58", pm, pf)

    cat("B-50m data frame without column names, trace=", trace, "\n", sep="")
    xdata.frame <- data.frame(sex3[1], -2, 0.1, fac3[1])
    colnames(xdata.frame) <- NULL
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("B-34", pm.ref, pm)

    cat("B-60f data frame without column names, trace=", trace, "\n", sep="")
    xdata.frame <- data.frame(sex3[1], -2, 0.1, fac3[1])
    colnames(xdata.frame) <- NULL
    pf <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("B-60", pm, pf)

    cat("B-60f data frame without extra columns, trace=", trace, "\n", sep="")
    xdata.frame <- data.frame(sex3=sex3[1], extra1=99, ldose=-2, extra2=99,
                               ldose1=0.1, fac3=fac3[1], extra3=sex3[7])
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("B-60f", pm, pf)

    cat("\n--- C predict with multiple level factors and a 3 level factor response, trace=", trace,
        " ---\n\n", sep="")

    # test set C: prepare the data

    ldose  <- rep(0:5, 2) - 2
    ldose1 <- c(0.1, 1.2, 2.3, 3.4, 4.5, 5.6, 0.3, 1.4, 2.5, 3.6, 4.7, 5.8)
    sex3 <- factor(rep(c("male", "female", "andro"), times=c(6,4,2)))
    fac3 <- factor(c("lev2", "lev2", "lev1", "lev1", "lev3", "lev3",
                     "lev2", "lev2", "lev1", "lev1", "lev3", "lev3"))
    facdead <- factor(c("dead2", "dead2", "dead3", "dead1", "dead3", "dead3",
                        "dead1", "dead2", "dead1", "dead1", "dead3", "dead3"))

    isex <- as.double(sex3) # sex3 as an index
    df <- data.frame(sex3=sex3, ldose=ldose, ldose1=ldose1, fac3=fac3)
    am <-  earth(df, facdead, trace=trace, pmethod=PMETHOD, nk=NK, degree=2)
    df.with.response <- data.frame(sex3=sex3, ldose=ldose, ldose1=ldose1, facdead=facdead, fac3=fac3)
    af <-  earth(facdead ~ ., data=df.with.response, trace=trace, pmethod=PMETHOD, nk=NK, degree=2)
    check.models.equal(am, af, "C predict with multiple level factors and a multiple real response")
    cat("20m head(predict(am, trace=", trace, ")\n", sep="")
    pm <- predict(am, trace=trace)
    if (trace) print(head(pm))

    cat("C-21f head(predict(af, trace=", trace, ")\n", sep="")
    pf <- predict(af, trace=trace)
    if (trace) print(head(pf))
    stop.if.not.identical("C-20", pm, pf)

    cat("C-31m predict(am, xdata.frame, trace=", trace, ") data frame with factors and wrong col names\n", sep="")
    xdata.frame <- data.frame(sex3[1], -2, 0.1, fac3[1])
    pm <- predict(am, xdata.frame, trace=trace)
    pm.ref <- pm
    stop.if.not.identical("C-31", pm.ref, pm)
    if (trace) print(pm)

    cat("C-31f predict(af, xdata.frame, trace=", trace, ") data frame with factors and wrong col names\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("C-31", pm, pf)

    cat("C-31bm predict(am, xdata.frame, trace=", trace, ") data frame col names\n", sep="")
    xdata.frame <- data.frame(sex3=sex3[1], ldose=-2, ldose1=0.1, fac3=fac3[1])
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("C-31", pm.ref, pm)

    cat("C-31bf predict(af, xdata.frame, trace=", trace, ") data frame col names\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("C-31b", pm, pf)

    cat("C-32m predict(am, xdata.frame, trace=", trace, ") # data frame with names\n", sep="")
    xdata.frame <- data.frame(sex3[1], -2, 0.1, fac3[1])
    colnames(xdata.frame) <- c("sex3", "ldose", "ldose1", "fac3")
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("C-32", pm, pf)
    stop.if.not.identical("C-32", pm.ref, pm)

    cat("C-32f predict(af, xdata.frame, trace=", trace, ") # data frame with names\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("C-32", pm, pf)

    cat("C-53m predict(am, xdata.frame, trace=", trace, ") data frame with not enough columns, expect error message\n", sep="")
    xdata.frame <- data.frame(sex3[1], -2)
    expect.err(try(predict(am, xdata.frame, trace=trace)), "could not interpret newdata")

    cat("C-53f predict(af, xdata.frame, trace=", trace, ") data frame with not enough columns, expect error message\n", sep="")
    expect.err(try(predict(af, xdata.frame, trace=trace)), "could not interpret newdata")

    cat("C-54m predict(am, xdata.frame, trace=", trace, ") # data frame with cols in different order\n", sep="")
    xdata.frame <- data.frame(-2, sex3[1], 0.1, fac3[1])
    colnames(xdata.frame) <- c("ldose", "sex3", "ldose1", "fac3")
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("C-54", pm.ref, pm)

    cat("C-54f predict(af, xdata.frame, trace=", trace, ") # data frame with cols in different order\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("C-54", pm, pf)

    cat("C-55m predict(am, xdata.frame, trace=", trace, ") data frame without col names\n", sep="")
    xdata.frame <- data.frame(sex3[c(1,7)], c(-2,-1), c(0.1, 0.1), fac3[c(1,9)])
    pm <- predict(am, xdata.frame, trace=trace)
    pm3.ref <- pm
    if (trace) print(pm)

    cat("C-55f predict(af, xdata.frame, trace=", trace, ") data frame without col names\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("C-55", pm, pf)

    cat("C2-55bm predict(am, xdata.frame, trace=", trace, ") data frame col names\n", sep="")
    xdata.frame <- data.frame(sex3=sex3[c(1,7)], ldose=c(-2,-1), ldose1=c(0.1,0.1), fac3=fac3[c(1,9)])
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("C2-55", pm3.ref, pm)

    cat("C2-55bf predict(af, xdata.frame, trace=", trace, ") data frame col names\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("C2-55b", pm, pf)

    cat("C-56m predict(am, xdata.frame, trace=", trace, ") # data frame with col names\n", sep="")
    xdata.frame <- data.frame(sex3[c(1,7)], c(-2,-1), c(0.1, 0.1), fac3[c(1,9)])
    colnames(xdata.frame) <- c("sex3", "ldose", "ldose1", "fac3")
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("C-56", pm3.ref, pm)

    cat("C-56f predict(af, xdata.frame, trace=", trace, ") # data frame with col names\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("C-56", pm, pf)

    cat("C-57m predict(am, xdata.frame, trace=", trace, ") data frame with not enough columns, expect error message\n", sep="")
    xdata.frame <- data.frame(sex3[c(1,7)], c(-2,-1))
    expect.err(try(predict(am, xdata.frame, trace=trace)), "could not interpret newdata")

    cat("C-57f predict(af, xdata.frame, trace=", trace, ") data frame with not enough columns, expect error message\n", sep="")
    expect.err(try(predict(af, xdata.frame, trace=trace)),
"could not interpret newdata\n           model.matrix returned 2 columns: \"sex3.c.1..7..\", \"c..2...1.\"\n           need 4 columns: \"sex3\", \"ldose\", \"ldose1\", \"fac3\"")

    stop.if.not.identical("C-57", pm, pf)

    cat("C-58m predict(am, xdata.frame, trace=", trace, ") # data frame with cols in different order\n", sep="")
    xdata.frame <- data.frame(c(-2,-1), sex3[c(1,7)], c(0.1, 0.1),  fac3[c(1,9)])
    colnames(xdata.frame) <- c("ldose", "sex3", "ldose1", "fac3")
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("C-58", pm3.ref, pm)

    cat("C-58f predict(af, xdata.frame, trace=", trace, ") # data frame with cols in different order\n", sep="")
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("C-58", pm, pf)

    cat("C-50m data frame without column names, trace=", trace, "\n", sep="")
    xdata.frame <- data.frame(sex3[1], -2, 0.1, fac3[1])
    colnames(xdata.frame) <- NULL
    pm <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pm)
    stop.if.not.identical("C-34", pm.ref, pm)

    cat("C-60f data frame without column names, trace=", trace, "\n", sep="")
    xdata.frame <- data.frame(sex3[1], -2, 0.1, fac3[1])
    colnames(xdata.frame) <- NULL
    pf <- predict(am, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("C-60", pm, pf)

    cat("C-61f data frame without extra columns, trace=", trace, "\n", sep="")
    xdata.frame <- data.frame(sex3=sex3[1], extra1=99, ldose=-2, extra2=99,
                               ldose1=0.1, fac3=fac3[1], extra3=sex3[7])
    pf <- predict(af, xdata.frame, trace=trace)
    if (trace) print(pf)
    stop.if.not.identical("C-61", pm, pf)
}
test.predict.with.factors(trace=1)
test.predict.with.factors(trace=0)

cat("---test glm.predict---\n")

ldose  <- rep(0:5, 2) - 2
sex <- factor(rep(c("male", "female"), times=c(6,6)))
numdead <- c(1,4,9,13,18,20,0,2,6,10,12,16)
SF <- cbind(numdead, numalive=20 - numdead)

cat("c1a: single response glm model with a binomial pair of y columns, fitted values, keepxy=0\n")
c1a  <- earth(SF ~ sex + ldose, glm=list(family="binomial"), linpreds=TRUE, trace=1, pmethod=PMETHOD, nk=NK, degree=1, keepxy=0)
c1ag <- glm(SF ~ sex + ldose, family="binomial") # use this as a reference

c1a.predict <- predict(c1a, trace=1)
c1ag.predict <- predict(c1ag, trace=1)
check.almost.equal(c1a.predict, c1ag.predict, max=1e-10, msg="c1a fitted values, type=default link, keepxy=0", verbose=TRUE)
c1a.predict <- predict(c1a, type="link", trace=1)
c1ag.predict <- predict(c1ag, type="li", trace=1)
check.almost.equal(c1a.predict, c1ag.predict, max=1e-10, msg="c1a fitted values, type=link, keepxy=0", verbose=TRUE)
c1a.predict <- predict(c1a, type="response", trace=1)
c1ag.predict <- predict(c1ag, type="resp", trace=1)
check.almost.equal(c1a.predict, c1ag.predict, max=1e-10, msg="c1a fitted values, type=response, keepxy=0", verbose=TRUE)
c1a.predict <- predict(c1a, type="e", trace=1)
dead.frac <- numdead / (numdead + (20 - numdead))
c1ae <- earth(dead.frac ~ sex + ldose, trace=1, linpreds=TRUE, pmethod=PMETHOD, nk=NK, degree=1, keepxy=0)
c1ae.predict <- predict(c1ae, trace=1)
check.almost.equal(c1a.predict, c1ae.predict, max=1e-10, msg="c1a fitted values, type=earth, keepxy=0", verbose=TRUE)

cat("c1b: single response glm model with a binomial pair of y columns\n")
c1b <-  earth(SF ~ sex + ldose, glm=list(family="binomial"), linpreds=TRUE, trace=1, pmethod=PMETHOD, nk=NK, degree=1, keepxy=1)
c1be <-  earth(numdead ~ sex + ldose, linpreds=TRUE, trace=1, pmethod=PMETHOD, nk=NK, degree=1, keepxy=1)
c1bg <- glm(SF ~ sex + ldose, family="binomial") # use this as a reference

newdata <- data.frame(sex=sex[1], ldose=2)
c1b.predict <- predict(c1b, newdata, trace=1)
stopifnot(dim(c1b.predict) == c(1,1))
check.almost.equal(c1b.predict, predict(c1bg, newdata), max=1e-10, msg="c1b", verbose=TRUE)

c1b.link.predict <- predict(c1b, newdata, type="link", trace=1) # should be same as above because default is link
check.almost.equal(c1b.link.predict, c1b.predict, max=1e-10, msg="c1b link", verbose=TRUE)

c1b.predict <- predict(c1b, newdata, type="r")
stopifnot(dim(c1b.predict) == c(1,1))
check.almost.equal(c1b.predict, predict(c1bg, newdata, type="response"), max=1e-10, msg="c1b type=response", verbose=TRUE)

c1b.predict <- predict(c1b, newdata, type="earth")
stopifnot(dim(c1b.predict) == c(1,1))
print(c1b.predict)

newdata <- data.frame(sex=sex[c(1,3,7,9)], ldose=ldose[c(1,3,7,9)])
c1b.predict <- predict(c1b, newdata, trace=1)
stopifnot(dim(c1b.predict) == c(4,1))
check.almost.equal(c1b.predict, predict(c1bg, newdata), max=1e-10, msg="c1b multiple rows", verbose=TRUE)

c1b.predict <- predict(c1b, newdata, type="response", trace=1)
stopifnot(dim(c1b.predict) == c(4,1))
check.almost.equal(c1b.predict, predict(c1bg, newdata, type="response"), max=1e-10, msg="c1b multiple rows type=response", verbose=TRUE)

c1b.predict <- predict(c1b, newdata, type="terms", trace=0)
print(c1b.predict)
c1be.predict <- predict(c1be, newdata, type="terms")
print(c1be.predict)
c1bg.predict <- predict(c1bg, newdata, type="terms")
print(c1bg.predict)

# commented out because multiple binomial pairs are no longer supported
# cat("c2: double response glm model with two y binomial pairs\n")
# SF2 <- cbind(numdead, numalive=20 - numdead, numdead2=numdead, numalive2=20 - numdead)
# c2 <-  earth(SF2 ~ sex + ldose, glm=list(family="binomial"), linpreds=TRUE, trace=1, pmethod=PMETHOD, nk=NK, degree=1, keepxy=1)
# c2e <-  earth(data.frame(sex, ldose), data.frame(numdead,numdead), linpreds=TRUE, trace=1, pmethod=PMETHOD, nk=NK, degree=1, keepxy=1)
# c2g <- glm(SF ~ sex + ldose, family="binomial") # use this as a reference

# newdata <- data.frame(sex=sex[1], ldose=2)
# c2.predict <- predict(c2, newdata, trace=1)
# stopifnot(dim(c2.predict) == c(1,2))
# check.almost.equal(c2.predict[,1], predict(c2g, newdata), max=1e-10, msg="c2", verbose=TRUE)
#
# c2.link.predict <- predict(c2, newdata, type="link", trace=1) # should be same as above because default is link
# check.almost.equal(c2.link.predict, c2.predict, max=1e-10, msg="c2 link", verbose=TRUE)
#
# c2.predict <- predict(c2, newdata, type="response")
# stopifnot(dim(c2.predict) == c(1,2))
# check.almost.equal(c2.predict[,1], predict(c2g, newdata, type="response"), max=1e-10, msg="c2 multiple rows type=response", verbose=TRUE)
#
# newdata <- data.frame(sex=sex[c(1,3,7,9)], ldose=ldose[c(1,3,7,9)])
# c2.predict <- predict(c2, newdata)
# stopifnot(dim(c2.predict) == c(4,2))
# check.almost.equal(c2.predict[,1], predict(c2g, newdata), max=1e-10, msg="c2 column1", verbose=TRUE)
# check.almost.equal(c2.predict[,2], predict(c2g, newdata), max=1e-10, msg="c2 column2", verbose=TRUE)
#
# c2.predict <- predict(c2, newdata, type="response")
# stopifnot(dim(c2.predict) == c(4,2))
# check.almost.equal(c2.predict[,1], predict(c2g, newdata, type="response"), max=1e-10, msg="c2 column1 multiple rows type=response", verbose=TRUE)
# check.almost.equal(c2.predict[,2], predict(c2g, newdata, type="response"), max=1e-10, msg="c2 column2 multiple rows type=response", verbose=TRUE)
#
# c2.predict <- predict(c2, newdata, type="earth", trace=1)
# stopifnot(dim(c2.predict) == c(4,2))
# check.almost.equal(c2.predict[,1], predict(c2e, newdata, trace=1), max=1e-10, msg="c2 column1 multiple rows type=earth", verbose=TRUE)
# check.almost.equal(c2.predict[,2], predict(c2e, newdata, trace=1), max=1e-10, msg="c2 column2 multiple rows type=earth", verbose=TRUE)

cat("c3a: single response glm model with a boolean response, fitted values, keepxy=0\n")

mybool <- rep(c(FALSE, TRUE), times=c(6,6))
data1 <- data.frame(mybool, sex, ldose)
c3a <-  earth(mybool ~ sex + ldose, data=data1, glm=list(family="binomial"), linpreds=TRUE, trace=1, pmethod=PMETHOD, nk=NK, degree=1, keepxy=0)
c3ag <- glm(mybool ~ sex + ldose, family="binomial") # use this as a reference
c3ae <- earth(mybool ~ sex + ldose, data=data1, linpreds=TRUE, pmethod=PMETHOD, nk=NK, degree=1, keepxy=1)

c3a.predict <- predict(c3a, trace=1)
c3ag.predict <- predict(c3ag, trace=1)
# TODO why does max have to be big here?
check.almost.equal(c3a.predict, c3ag.predict, max=1e-7, msg="c3a fitted values, type=default link, keepxy=0", verbose=TRUE)
c3a.predict <- predict(c3a, type="link", trace=1)
c3ag.predict <- predict(c3ag, type="link", trace=1)
check.almost.equal(c3a.predict, c3ag.predict, max=1e-7, msg="c3a fitted values, type=link, keepxy=0", verbose=TRUE)
c3a.predict <- predict(c3a, type="response", trace=1)
c3ag.predict <- predict(c3ag, type="response", trace=1)
check.almost.equal(c3a.predict, c3ag.predict, max=1e-10, msg="c3a fitted values, type=response, keepxy=0", verbose=TRUE)
c3a.predict <- predict(c3a, type="earth", trace=1)
c3ae.predict <- predict(c3ae, trace=1)
check.almost.equal(c3a.predict, c3ae.predict, max=1e-10, msg="c3a fitted values, type=earth, keepxy=0", verbose=TRUE)

c3a.response.predict <- predict(c3a, type="response")
c3a.class.predict <- predict(c3a,type="class")
stopifnot(c3a.class.predict == (c3a.response.predict > .5))

cat("c3b: single response glm model with a boolean response, fitted values, keepxy=1\n")

c3b <-  earth(mybool ~ sex + ldose, glm=list(family="binomial"), linpreds=TRUE, trace=1, pmethod=PMETHOD, nk=NK, degree=1, keepxy=1)
c3bg <- glm(mybool ~ sex + ldose, family="binomial") # use this as a reference
c3be <- earth(mybool ~ sex + ldose, linpreds=TRUE, pmethod=PMETHOD, nk=NK, degree=1, keepxy=0)

c3b.predict <- predict(c3b, trace=1) # fitted values
c3bg.predict <- predict(c3bg, trace=1)
check.almost.equal(c3b.predict, c3bg.predict, max=1e-7, msg="c3b fitted values, type=default link, keepxy=0", verbose=TRUE)
c3b.predict <- predict(c3b, type="link", trace=1)
c3bg.predict <- predict(c3bg, type="link", trace=1)
check.almost.equal(c3b.predict, c3bg.predict, max=1e-7, msg="c3b fitted values, type=link, keepxy=0", verbose=TRUE)
c3b.predict <- predict(c3b, type="response", trace=1)
c3bg.predict <- predict(c3bg, type="response", trace=1)
check.almost.equal(c3b.predict, c3bg.predict, max=1e-10, msg="c3b fitted values, type=response, keepxy=0", verbose=TRUE)
c3b.predict <- predict(c3b, type="earth", trace=1)
c3be.predict <- predict(c3be, trace=1)
check.almost.equal(c3b.predict, c3be.predict, max=1e-10, msg="c3b fitted values, type=earth, keepxy=0", verbose=TRUE)

c3b.response.predict <- predict(c3b, type="response")
c3b.class.predict <- predict(c3b,type="cla")
stopifnot(c3b.class.predict == (c3b.response.predict > .5))

cat("c3c: single response glm model with a boolean response\n")

c3c <-  earth(mybool ~ sex + ldose, data=data1, linpreds=TRUE, glm=list(family="binomial"), trace=1, pmethod=PMETHOD, nk=NK, degree=1, keepxy=0)
c3cg <- glm(mybool ~ sex + ldose, data=data1, family="binomial") # use this as a reference
c3ce <- earth(mybool ~ sex + ldose, data=data1, linpreds=TRUE, pmethod=PMETHOD, nk=NK, degree=1, keepxy=0)

newdata <- data.frame(sex=sex[1], ldose=2)
c3c.predict <- predict(c3c, newdata)
stopifnot(dim(c3c.predict) == c(1,1))
check.almost.equal(c3c.predict, predict(c3cg, newdata), max=1e-10, msg="c3c", verbose=TRUE)

c3c.predict <- predict(c3c, newdata, type="response")
stopifnot(dim(c3c.predict) == c(1,1))
check.almost.equal(c3c.predict, predict(c3cg, newdata, type="response"), max=1e-10, msg="c3c type=response", verbose=TRUE)

newdata <- data.frame(sex=sex[c(1,3,7,9)], ldose=ldose[c(1,3,7,9)])
c3c.predict <- predict(c3c, newdata)
stopifnot(dim(c3c.predict) == c(4,1))
# TODO why does the max have to be bigger on this?
check.almost.equal(c3c.predict, predict(c3cg, newdata), max=1e-7, msg="c3c multiple rows", verbose=TRUE)

c3c.predict <- predict(c3c, newdata, type="response")
stopifnot(dim(c3c.predict) == c(4,1))
check.almost.equal(c3c.predict, predict(c3cg, newdata, type="response"), max=1e-10, msg="c3c multiple rows type=response", verbose=TRUE)

c3c.response.predict <- predict(c3c, type="response")
c3c.class.predict <- predict(c3c,type="cl")
stopifnot(c3c.class.predict == (c3c.response.predict > .5))

cat("c3d: single response glm model with a two level factor response\n")
cat("Expect \"did not converge warnings\", it doesn't matter for our purposes here\n")
myfac <- gl(2, 3, length=12, labels = c("Control", "Treat"))
c3d <-  earth(myfac ~ ldose + sex, data=data1, glm=list(family="binomial"), trace=0, pmethod=PMETHOD, nk=NK, degree=1)
c3d.class.predict <- predict(c3d,type="cl") # we also test here that the type can be abbreviated
stopifnot(c3d.class.predict == myfac)

cat("c4: multiple response glm model with a factor response\n")
fac3 <- factor(rep(c("A", "B", "C"), times=c(4,3,5)))
cat("Expect \"did not converge warnings\", it doesn't matter for our purposes here\n")
c4 <-  earth(fac3 ~ sex + ldose, linpreds=TRUE, glm=list(family="binomial"), trace=1, pmethod=PMETHOD, nk=NK, degree=1, keepxy=1)
c4g <- glm(fac3 ~ sex + ldose, family="binomial") # use this as a reference
c4.notrace <-  earth(fac3 ~ sex + ldose, linpreds=TRUE, glm=list(family="binomial"), trace=0, pmethod=PMETHOD)

newdata <- data.frame(sex=sex[1], ldose=2)
c4.predict <- predict(c4, newdata)
stopifnot(dim(c4.predict) == c(1,3))
# minus needed on predict because of different handling of factors
check.almost.equal(c4.predict[1,1], -predict(c4g, newdata), max=1e-8, msg="c4", verbose=TRUE)

newdata <- data.frame(sex=sex[c(1,3,7,9)], ldose=ldose[c(1,3,7,9)])
c4.predict <- predict(c4, newdata)
stopifnot(dim(c4.predict) == c(4,3))
check.almost.equal(c4.predict[,1], -predict(c4g, newdata), max=1e-8, msg="c4 multiple rows", verbose=TRUE)

c4.predict <- predict(c4, newdata, type="response")
stopifnot(dim(c4.predict) == c(4,3))
check.almost.equal(1-c4.predict[,1], predict(c4g, newdata, type="response"), max=1e-10, msg="c4 multiple rows type=response", verbose=TRUE)

cat("c5: multiple response glm model with two multi level factor responses\n")

fac3 <- factor(rep(c("A", "B", "C"), times=c(4,3,5)))
fac4 <- factor(rep(c("P", "Q", "R", "S"), times=c(3,3,3,3)))
big.dataframe <- data.frame(fac3, fac4)
c5 <-  earth(data.frame(sex, ldose), big.dataframe, trace=1, pmethod=PMETHOD, nk=NK, degree=1, keepxy=1)
stopifnot(colnames(c5$coef) == c("fac3A", "fac3B", "fac3C",
                                 "fac4P", "fac4Q", "fac4R", "fac4S"))
stopifnot(is.null(c5$glm.bpairs))

cat("c6: multiple response earth model with mixed responses\n")

big.dataframe2 <- data.frame(SF, fac3, fac4, SF+1, sex, fac4, SF+3)
c6 <-  earth(data.frame(sex, ldose), big.dataframe2, trace=1, pmethod=PMETHOD, nk=NK, degree=1, keepxy=1)
stopifnot(colnames(c6$coef) == c("numdead", "numalive", "fac3A", "fac3B", "fac3C",
                                  "fac4P", "fac4Q", "fac4R", "fac4S", "numdead.1", "numalive.1",
                                  "sex", "fac4.1P", "fac4.1Q", "fac4.1R", "fac4.1S",
                                  "numdead.2", "numalive.2"))
stopifnot(is.null(c6$glm.bpairs))

# residuals

a <- earth(pclass ~ ., data=etitanic)
printh(residuals(a), max.print=3)
a <- earth(pclass ~ ., data=etitanic, glm=list(family="b"))
printh(residuals(a), expect.warning=TRUE, max.print=3)
printh(residuals(a, warn=FALSE), max.print=3)
printh(resid(a, type="earth"), max.print=3)
printh(resid(a, type="deviance"), max.print=3)
printh(resid(a, type="glm.pearson"), max.print=3)
printh(resid(a, type="glm.working"), max.print=3)
printh(resid(a, type="glm.response"), max.print=3)
printh(resid(a, type="glm.partial"), max.print=3)
expect.err(try(printh(resid(a, type="nonesuch"), max.print=3)), "Choose one of")
expect.err(try(printh(resid(a, type="g"), max.print=3)), "ambiguous") # type="g" is ambiguous
expect.err(try(printh(resid(a, type="standardize"), max.print=3)), "model was not built with varmod.method") # model was not built with varmod.method

# tests based on Gavin Simpson's bug report
# fit a MARS model allowing one-way interactions
mod.Gamma <- earth(O3 ~ . - doy, data = ozone1, degree = 2, glm = list(family = Gamma))
cat("summary(mod.Gamma):\n")
print(summary(mod.Gamma))
for(type in c("earth", "deviance", "glm.pearson", "glm.working", "glm.response", "glm.partial"))
{
    cat("residuals.earth Gamma type=", type, ":\n", sep="")
    print(head(resid(mod.Gamma, type = type), n=2))
    print(tail(resid(mod.Gamma, type = type), n=2))
}
mod.binomial <- earth(survived ~ ., data = etitanic, degree = 2, glm = list(family = binomial))
cat("summary(mod.binomial):\n")
print(summary(mod.binomial))
for(type in c("earth", "deviance", "glm.pearson", "glm.working", "glm.response", "glm.partial"))
{
    cat("residuals.earth binomial type=", type, ":\n", sep="")
    print(head(residuals(mod.binomial, type = type), n=2))
    print(tail(residuals(mod.binomial, type = type), n=2))
}
# intercept only model

cat("a.intercept.only: intercept only logistic model\n\n")
# This seed chosen so call to earth below has one predictor model in 1st
# cv fold and intercept-only in 2nd cv fold, that way we test both.
set.seed(3)
df <- data.frame(aaa = round(runif(18)), bbb = runif(18), ccc = rnorm(18))
a.intercept.only <- earth(aaa ~ bbb + ccc, data = df, glm=list(family=binomial), trace=1, nfold=2)
show.earth.models(a.intercept.only)
cat("\nsummary(a.intercept.only, details=TRUE)\n\n", sep="")
print(summary(a.intercept.only, details=TRUE))
printh(predict(a.intercept.only))
printh(predict(a.intercept.only, type="link"))
printh(predict(a.intercept.only, type="response"))
printh(predict(a.intercept.only, type="earth"))
g <- a.intercept.only$glm.list[[1]]
printh(predict(g, type="link"))
printh(predict(g, type="response"))

new.df <- df[3:5, ]
printh(predict(a.intercept.only, type="response"))
printh(predict(a.intercept.only, newdata=new.df, trace=1, type="link"))
printh(predict(a.intercept.only, newdata=new.df, trace=1, type="response"))
printh(predict(a.intercept.only, newdata=new.df, type="earth"))
printh(predict(a.intercept.only, newdata=new.df, type="class"))
# cat("Expect Warning: predict.earth: returning the earth (not glm) terms\n")
printh(predict(a.intercept.only, newdata=new.df, type="terms"))

set.seed(1235)
a <- earth(survived ~ ., data=etitanic, glm=list(family=binomial), nfold=2)
plot.earth.models(list(a.intercept.only, a), main="plot.earth.models\nlist(a.intercept.only, a)")
plot.earth.models(list(a, a.intercept.only), main="plot.earth.models\nlist(a, a.intercept.only)", legend.pos="topleft", jitter=.01)
# nothing will plot for the next call
plot.earth.models(list(a.intercept.only, a.intercept.only), main="plot.earth.models\nlist(a.intercept.only, a.intercept.only)")

# test position of legend and "intercep-only model" message when only one term in model
a.intercept.pruned <- update(a.intercept.only, nprune=1, nfold=1)
show.earth.models(a.intercept.pruned)

# misc tests

cat("---misc 1---\n")
sex1 <- factor(rep(c("male", "female"), times=c(6,6)))
sex2 <- factor(rep(c("male", "female"), times=c(6,6)))
expect.err(try(earth(numdead, cbind(sex1, sex2, sex1), trace=1)), # one duplicate name
               "Duplicate colname in cbind(sex1, sex2, sex1) (colnames are \"sex1\", \"sex2\", \"sex1\"")
sex1 <- factor(rep(c("male", "female"), times=c(6,6)))
sex2 <- factor(rep(c("male", "female"), times=c(6,6)))
expect.err(try(earth(numdead, cbind(sex1, sex2, sex1, sex1), trace=1)), # two duplicate names
               "Duplicate colname in cbind(sex1, sex2, sex1, sex1) (colnames are \"sex1\", \"sex2\", \"sex1\", \"sex1\"")

# test column expansion when y is a data frame in earth.default

cat("---misc 2---\n")
ldose  <- rep(0:5, 2) - 2
ldose1 <- c(0.1, 1.2, 2.3, 3.4, 4.5, 5.6, 0.3, 1.4, 2.5, 3.6, 4.7, 5.8)
sex <- factor(rep(c("male", "female"), times=c(6,6)))
sex2 <- sex
sex3 <- factor(rep(c("male", "female", "andro"), times=c(6,4,2)))
fac3 <- factor(c("lev2", "lev2", "lev1", "lev1", "lev3", "lev3",
                 "lev2", "lev2", "lev1", "lev1", "lev3", "lev3"))
facdead <- factor(c("dead2", "dead2", "dead3", "dead1", "dead3", "dead3",
                    "dead1", "dead2", "dead1", "dead1", "dead3", "dead3"))

isex <- as.double(sex3) # sex3 as an index
df1 <- data.frame(sex2, d_=facdead, sex, sex, isex)
af <-  earth(data.frame(sex3,ldose,fac3,isex), df1, trace=1, pmethod=PMETHOD, nk=NK, degree=2)

cat("---misc 3---\n")

# strings in input matrices, get converted to factors and a warning issued
# TODO would like to improve the error message (says 'x' even when 'y')
# TODO Apr 2013 warning no longer issued (R changed), is that ok?

ldose  <- rep(0:5, 2) - 2
ldose1 <- c(0.1, 1.2, 2.3, 3.4, 4.5, 5.6, 0.3, 1.4, 2.5, 3.6, 4.7, 5.8)
sex2 <- rep(c("male", "female"), times=c(6,6))
# y cannot be a character variable
expect.err(try(earth(sex2, sex2, trace=1)), "y is a character variable: ")
expect.err(try(earth(sex2~ldose1, trace=1)), "y is a character variable: ")
# but note that this is ok
earth(sex2, data.frame(sex2=sex2), trace=1)

earth(sex2, data.frame(sex2=sex2, stringsAsFactors=TRUE), trace=1) # R 4.0.0 may 2020

# test update.earth with bpairs argument (for now always do forward pass if bpairs)

cat("---misc 4---\n")
volumei <- as.integer(trees$Volume)
x1 <- trees$Height
a <- earth(x1, cbind(volumei, 100-volumei), glm=list(family=binomial))
update(a, trace=1, glm=list(family=binomial))
a <- earth(x1, cbind(volumei, 100-volumei), glm=list(family=binomial))
update(a, trace=1, glm=list(family=binomial))

source("test.epilog.R")
