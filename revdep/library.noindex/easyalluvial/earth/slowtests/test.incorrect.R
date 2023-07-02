# test.incorrect.R: example incorrect model built by earth
# Stephen Milborrow May 2015 Berea

source("test.prolog.R")
library(earth)
options(digits=4)

printf <- function(format, ...) cat(sprint(format, ...)) # like c printf

sos <- function(x) sum(as.vector(x^2)) # sum of squares

func <- function(x) # bivariate with interaction
{
    x[,1] + x[,2] + (x[,1] * x[,2]) + .3 * rnorm(nrow(x))
}

n <- 30
set.seed(n)
n <- 11
seed <- 17
set.seed(100 + seed)
x1 <- sort(runif(n, -(n-1), n+1))
x2 <- runif(n, -(n-1), n+1)
x <- data.frame(x1=x1, x2=x2)
set.seed(101 + seed)

x1test <- runif(10000, -n, n)
x2test <- runif(10000, -n, n)
xtest <- data.frame(x1=x1test, x2=x2test)
colnames(x) <- colnames(xtest) <- c("x1", "x2")
set.seed(103 + seed)
ytest <- func(xtest)

par(mfrow = c(2, 2), mar = c(3, 3, 3, 1), mgp = c(1.5, 0.5, 0))
correct.mod <- earth(xtest, ytest, degree=2, trace=0, minspan=-1, Force.weights=TRUE)
plotmo(correct.mod, degree1=0, do.par=FALSE, main="correct model\nx1 + x2 + x1*x2")
plotmo(correct.mod, degree1=0, do.par=FALSE, main="correct model", type2="im")

set.seed(102 + seed)
y <- func(x)
incorrect.mod <- earth(x, y, degree=2, trace=2, minspan=-1)
print(incorrect.mod)
test.rsq <- 1 - sos(ytest - predict(incorrect.mod, newdata=xtest)) / sos(ytest - mean(ytest))
plotmo(incorrect.mod, degree1=0, do.par=FALSE, main="incorrect model")
plotmo(incorrect.mod, degree1=0, do.par=FALSE, main="incorrect model", pt.col=2, type2="im")
points(xtest[,1], xtest[,2], col=3, pch=20, cex=.05)

source("test.epilog.R")
