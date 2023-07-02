# test.allowedfunc.R

source("test.prolog.R")
source("check.models.equal.R")
library(earth)
data(trees)

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

example.nopred2  <- function(degree, pred, parents)
{
    pred != 2  # disallow predictor 2, which is "Height"
}
a.nopred2 <- earth(Volume ~ ., data = trees, allowed = example.nopred2)
printh(summary(a.nopred2))

example.noHeight  <- function(degree, pred, parents, namesx)
{
    namesx[pred] != "Height"  # disallow "Height"
}
a.noHeight <- earth(Volume ~ ., data = trees, allowed = example.noHeight)
newdata.global <- trees[seq(from=nrow(trees), to=1, by=-5),]
check.models.equal(a.nopred2, a.noHeight, msg="\"allowed\" function a.nopred2 a.noHeight", newdata=newdata.global)

# we explicitly set minspan and endspan here because they are calculated differently if number of predictors is different
aGirthOnly <- earth(Volume ~ Girth, data = trees, trace=4, minspan=1, endspan=1)
printh(summary(aGirthOnly))
a1c <- earth(Volume ~ ., data = trees, allowed = example.noHeight, trace=4, minspan=1, endspan=1)
# can't use check.models.equal because e.g. dirs for a1c has two columns but aGirthOnly has only one column
stopifnot(identical(predict(aGirthOnly), predict(a1c)))

iheight <- 0
example.noHeight.first  <- function(degree, pred, parents, namesx, first)
{
    if (first) {
        iheight <<- which(namesx == "Height") # note use of <<- not <-
        if (length(iheight) != 1)
            stop("could not find Height in ", paste(namesx, collapse=" "))
    }
    pred != iheight
}
a.noHeight.first <- earth(Volume ~ ., data = trees, allowed = example.noHeight.first)
check.models.equal(a.nopred2, a.noHeight, msg="\"allowed\" function a.nopred2 a.noHeight.first", newdata=newdata.global)

example.noHumidityInDegree2 <- function(degree, pred, parents)
{
    # disallow humidity in terms of degree > 1
    # 3 is the "humidity" column in the input matrix
    if (degree > 1 && (pred == 3 || parents[3]))
        return(FALSE)
    TRUE
}
a.noHumidityInDegree2 <- earth(O3 ~ ., data = ozone1, degree = 2, allowed = example.noHumidityInDegree2)
printh(summary(a.noHumidityInDegree2))
example.Degree2OnlyHumidityAndTemp <- function(degree, pred, parents)
{
    # allow only humidity and temp in terms of degree > 1
    # 3 and 4 are the "humidity" and "temp" columns
    allowed.set = c(3,4)
    if (degree > 1 && (all(allowed.set != pred) || any(parents[-allowed.set])))
        return(FALSE)
    TRUE
}
a.Degree2OnlyHumidityAndTemp <- earth(O3 ~ ., data = ozone1, degree = 2, allowed = example.Degree2OnlyHumidityAndTemp)
printh(summary(a.Degree2OnlyHumidityAndTemp))

ihumidity.global <- NA
itemp.global <- NA
example.Degree2OnlyHumidityAndTemp.First  <- function(degree, pred, parents, namesx, first)
{
    if (first) {
        ihumidity.global <<- which(namesx == "humidity") # note use of <<- not <-
        if (length(ihumidity.global) != 1)
            stop("could not find humidity in ", paste(namesx, collapse=" "))
        itemp.global <<- which(namesx == "temp") # note use of <<- not <-
        if (length(itemp.global) != 1)
            stop("could not find temp in ", paste(namesx, collapse=" "))
    }
    # allow only humidity and temp in terms of degree > 1
    allowed.set = c(ihumidity.global, itemp.global)
    if (degree > 1 &&
           (all(allowed.set != pred) || any(parents[-allowed.set])))
        return(FALSE)
    TRUE
}
a.Degree2OnlyHumidityAndTemp.First <- earth(O3 ~ ., data = ozone1, degree = 2, allowed = example.Degree2OnlyHumidityAndTemp)
check.models.equal(a.Degree2OnlyHumidityAndTemp, a.Degree2OnlyHumidityAndTemp.First, msg="\"allowed\" function a.Degree2OnlyHumidityAndTemp a.Degree2OnlyHumidityAndTemp.First", newdata=newdata.global)

#--- no predictor in PREDICTORS is allowed to interact with any predictor in PARENTS
#--- but all other interactions are allowed

PREDICTORS <- c("age")
PARENTS <- c("survived", "parch")

example4 <- function(degree, pred, parents, namesx)
{
    if (degree > 1) {
        predictor <- namesx[pred]
        parents   <- namesx[parents != 0]
        if((any(predictor %in% PREDICTORS) && any(parents %in% PARENTS)) ||
           (any(predictor %in% PARENTS)    && any(parents %in% PREDICTORS))) {
            return(FALSE)
        }
    }
    TRUE
}
a4.allowed <- earth(sex~., data=etitanic, degree=2, allowed=example4)
printh(summary(a4.allowed))
plotmo(a4.allowed, caption="a4.allowed")

#--- predictors in PREDICTORS are allowed to interact with predictors in PARENTS
#--- but no other interactions are allowed

PREDICTORS <- c("age")
PARENTS    <- c("survived", "parch")

example5 <- function(degree, pred, parents, namesx)
{
    if (degree <= 1)
        return(TRUE)
    predictor <- namesx[pred]
    parents   <- namesx[parents != 0]
    if((any(predictor %in% PREDICTORS) && any(parents %in% PARENTS)) ||
       (any(predictor %in% PARENTS)    && any(parents %in% PREDICTORS))) {
        return(TRUE)
    }
    FALSE
}
a5.allowed <- earth(sex~., data=etitanic, degree=2, allowed=example5)
printh(summary(a5.allowed))
plotmo(a5.allowed, caption="a5.allowed")

# "allowed" function checks, these check error handling by forcing an error

expect.err(try(earth(Volume ~ ., data = trees, allowed = 99)), "argument is not a function")

example7  <- function(degree, pred) pred!=2
expect.err(try(earth(Volume ~ ., data = trees, allowed = example7)), "function does not have the correct number of arguments")

expect.err(try(earth(Volume ~ ., data = trees, allowed = earth)), "your 'allowed' function does not have the correct number of arguments")

example8  <- function(degree, pred, parents99) pred!=2
expect.err(try(earth(Volume ~ ., data = trees, allowed = example8)), "function needs the following arguments")

example9  <- function(degree, pred, parents, namesx99) pred!=2
expect.err(try(earth(Volume ~ ., data = trees, allowed = example9)), "function needs the following arguments")

source("test.epilog.R")
