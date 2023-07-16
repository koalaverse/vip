pkgname <- "gbm"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('gbm')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("calibrate.plot")
### * calibrate.plot

flush(stderr()); flush(stdout())

### Name: calibrate.plot
### Title: Calibration plot
### Aliases: calibrate.plot
### Keywords: hplot

### ** Examples

# Don't want R CMD check to think there is a dependency on rpart
# so comment out the example
#library(rpart)
#data(kyphosis)
#y <- as.numeric(kyphosis$Kyphosis)-1
#x <- kyphosis$Age
#glm1 <- glm(y~poly(x,2),family=binomial)
#p <- predict(glm1,type="response")
#calibrate.plot(y, p, xlim=c(0,0.6), ylim=c(0,0.6))



cleanEx()
nameEx("gbm")
### * gbm

flush(stderr()); flush(stdout())

### Name: gbm
### Title: Generalized Boosted Regression Modeling (GBM)
### Aliases: gbm

### ** Examples

#
# A least squares regression example 
#

# Simulate data
set.seed(101)  # for reproducibility
N <- 1000
X1 <- runif(N)
X2 <- 2 * runif(N)
X3 <- ordered(sample(letters[1:4], N, replace = TRUE), levels = letters[4:1])
X4 <- factor(sample(letters[1:6], N, replace = TRUE))
X5 <- factor(sample(letters[1:3], N, replace = TRUE))
X6 <- 3 * runif(N) 
mu <- c(-1, 0, 1, 2)[as.numeric(X3)]
SNR <- 10  # signal-to-noise ratio
Y <- X1 ^ 1.5 + 2 * (X2 ^ 0.5) + mu
sigma <- sqrt(var(Y) / SNR)
Y <- Y + rnorm(N, 0, sigma)
X1[sample(1:N,size=500)] <- NA  # introduce some missing values
X4[sample(1:N,size=300)] <- NA  # introduce some missing values
data <- data.frame(Y, X1, X2, X3, X4, X5, X6)

# Fit a GBM
set.seed(102)  # for reproducibility
gbm1 <- gbm(Y ~ ., data = data, var.monotone = c(0, 0, 0, 0, 0, 0),
            distribution = "gaussian", n.trees = 100, shrinkage = 0.1,             
            interaction.depth = 3, bag.fraction = 0.5, train.fraction = 0.5,  
            n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE, 
            verbose = FALSE, n.cores = 1)  

# Check performance using the out-of-bag (OOB) error; the OOB error typically
# underestimates the optimal number of iterations
best.iter <- gbm.perf(gbm1, method = "OOB")
print(best.iter)

# Check performance using the 50% heldout test set
best.iter <- gbm.perf(gbm1, method = "test")
print(best.iter)

# Check performance using 5-fold cross-validation
best.iter <- gbm.perf(gbm1, method = "cv")
print(best.iter)

# Plot relative influence of each variable
par(mfrow = c(1, 2))
summary(gbm1, n.trees = 1)          # using first tree
summary(gbm1, n.trees = best.iter)  # using estimated best number of trees

# Compactly print the first and last trees for curiosity
print(pretty.gbm.tree(gbm1, i.tree = 1))
print(pretty.gbm.tree(gbm1, i.tree = gbm1$n.trees))

# Simulate new data
set.seed(103)  # for reproducibility
N <- 1000
X1 <- runif(N)
X2 <- 2 * runif(N)
X3 <- ordered(sample(letters[1:4], N, replace = TRUE))
X4 <- factor(sample(letters[1:6], N, replace = TRUE))
X5 <- factor(sample(letters[1:3], N, replace = TRUE))
X6 <- 3 * runif(N) 
mu <- c(-1, 0, 1, 2)[as.numeric(X3)]
Y <- X1 ^ 1.5 + 2 * (X2 ^ 0.5) + mu + rnorm(N, 0, sigma)
data2 <- data.frame(Y, X1, X2, X3, X4, X5, X6)

# Predict on the new data using the "best" number of trees; by default,
# predictions will be on the link scale
Yhat <- predict(gbm1, newdata = data2, n.trees = best.iter, type = "link")

# least squares error
print(sum((data2$Y - Yhat)^2))

# Construct univariate partial dependence plots
plot(gbm1, i.var = 1, n.trees = best.iter)
plot(gbm1, i.var = 2, n.trees = best.iter)
plot(gbm1, i.var = "X3", n.trees = best.iter)  # can use index or name

# Construct bivariate partial dependence plots
plot(gbm1, i.var = 1:2, n.trees = best.iter)
plot(gbm1, i.var = c("X2", "X3"), n.trees = best.iter)
plot(gbm1, i.var = 3:4, n.trees = best.iter)

# Construct trivariate partial dependence plots
plot(gbm1, i.var = c(1, 2, 6), n.trees = best.iter, 
     continuous.resolution = 20)
plot(gbm1, i.var = 1:3, n.trees = best.iter)
plot(gbm1, i.var = 2:4, n.trees = best.iter)
plot(gbm1, i.var = 3:5, n.trees = best.iter)

# Add more (i.e., 100) boosting iterations to the ensemble
gbm2 <- gbm.more(gbm1, n.new.trees = 100, verbose = FALSE)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("gbm.more")
### * gbm.more

flush(stderr()); flush(stdout())

### Name: gbm.more
### Title: Generalized Boosted Regression Modeling (GBM)
### Aliases: gbm.more

### ** Examples

#
# A least squares regression example 
#

# Simulate data
set.seed(101)  # for reproducibility
N <- 1000
X1 <- runif(N)
X2 <- 2 * runif(N)
X3 <- ordered(sample(letters[1:4], N, replace = TRUE), levels = letters[4:1])
X4 <- factor(sample(letters[1:6], N, replace = TRUE))
X5 <- factor(sample(letters[1:3], N, replace = TRUE))
X6 <- 3 * runif(N) 
mu <- c(-1, 0, 1, 2)[as.numeric(X3)]
SNR <- 10  # signal-to-noise ratio
Y <- X1 ^ 1.5 + 2 * (X2 ^ 0.5) + mu
sigma <- sqrt(var(Y) / SNR)
Y <- Y + rnorm(N, 0, sigma)
X1[sample(1:N,size=500)] <- NA  # introduce some missing values
X4[sample(1:N,size=300)] <- NA  # introduce some missing values
data <- data.frame(Y, X1, X2, X3, X4, X5, X6)

# Fit a GBM
set.seed(102)  # for reproducibility
gbm1 <- gbm(Y ~ ., data = data, var.monotone = c(0, 0, 0, 0, 0, 0),
            distribution = "gaussian", n.trees = 100, shrinkage = 0.1,             
            interaction.depth = 3, bag.fraction = 0.5, train.fraction = 0.5,  
            n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE, 
            verbose = FALSE, n.cores = 1)  

# Check performance using the out-of-bag (OOB) error; the OOB error typically
# underestimates the optimal number of iterations
best.iter <- gbm.perf(gbm1, method = "OOB")
print(best.iter)

# Check performance using the 50% heldout test set
best.iter <- gbm.perf(gbm1, method = "test")
print(best.iter)

# Check performance using 5-fold cross-validation
best.iter <- gbm.perf(gbm1, method = "cv")
print(best.iter)

# Plot relative influence of each variable
par(mfrow = c(1, 2))
summary(gbm1, n.trees = 1)          # using first tree
summary(gbm1, n.trees = best.iter)  # using estimated best number of trees

# Compactly print the first and last trees for curiosity
print(pretty.gbm.tree(gbm1, i.tree = 1))
print(pretty.gbm.tree(gbm1, i.tree = gbm1$n.trees))

# Simulate new data
set.seed(103)  # for reproducibility
N <- 1000
X1 <- runif(N)
X2 <- 2 * runif(N)
X3 <- ordered(sample(letters[1:4], N, replace = TRUE))
X4 <- factor(sample(letters[1:6], N, replace = TRUE))
X5 <- factor(sample(letters[1:3], N, replace = TRUE))
X6 <- 3 * runif(N) 
mu <- c(-1, 0, 1, 2)[as.numeric(X3)]
Y <- X1 ^ 1.5 + 2 * (X2 ^ 0.5) + mu + rnorm(N, 0, sigma)
data2 <- data.frame(Y, X1, X2, X3, X4, X5, X6)

# Predict on the new data using the "best" number of trees; by default,
# predictions will be on the link scale
Yhat <- predict(gbm1, newdata = data2, n.trees = best.iter, type = "link")

# least squares error
print(sum((data2$Y - Yhat)^2))

# Construct univariate partial dependence plots
plot(gbm1, i.var = 1, n.trees = best.iter)
plot(gbm1, i.var = 2, n.trees = best.iter)
plot(gbm1, i.var = "X3", n.trees = best.iter)  # can use index or name

# Construct bivariate partial dependence plots
plot(gbm1, i.var = 1:2, n.trees = best.iter)
plot(gbm1, i.var = c("X2", "X3"), n.trees = best.iter)
plot(gbm1, i.var = 3:4, n.trees = best.iter)

# Construct trivariate partial dependence plots
plot(gbm1, i.var = c(1, 2, 6), n.trees = best.iter, 
     continuous.resolution = 20)
plot(gbm1, i.var = 1:3, n.trees = best.iter)
plot(gbm1, i.var = 2:4, n.trees = best.iter)
plot(gbm1, i.var = 3:5, n.trees = best.iter)

# Add more (i.e., 100) boosting iterations to the ensemble
gbm2 <- gbm.more(gbm1, n.new.trees = 100, verbose = FALSE)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("print.gbm")
### * print.gbm

flush(stderr()); flush(stdout())

### Name: print.gbm
### Title: Print model summary
### Aliases: print.gbm show.gbm
### Keywords: models nonlinear nonparametric survival

### ** Examples


data(iris)
iris.mod <- gbm(Species ~ ., distribution="multinomial", data=iris,
                 n.trees=2000, shrinkage=0.01, cv.folds=5,
                 verbose=FALSE, n.cores=1)
iris.mod
#data(lung)
#lung.mod <- gbm(Surv(time, status) ~ ., distribution="coxph", data=lung,
#                 n.trees=2000, shrinkage=0.01, cv.folds=5,verbose =FALSE)
#lung.mod



cleanEx()
nameEx("quantile.rug")
### * quantile.rug

flush(stderr()); flush(stdout())

### Name: quantile.rug
### Title: Quantile rug plot
### Aliases: quantile.rug
### Keywords: aplot

### ** Examples

x <- rnorm(100)
y <- rnorm(100)
plot(x, y)
quantile.rug(x)



cleanEx()
nameEx("test.gbm")
### * test.gbm

flush(stderr()); flush(stdout())

### Name: test.gbm
### Title: Test the 'gbm' package.
### Aliases: test.gbm validate.gbm test.relative.influence
### Keywords: models

### ** Examples


# Uncomment the following lines to run - commented out to make CRAN happy
#library(RUnit)
#val <- validate.texmex()
#printHTMLProtocol(val, "texmexReport.html")



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
