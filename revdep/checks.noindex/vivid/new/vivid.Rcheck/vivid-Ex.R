pkgname <- "vivid"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('vivid')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("as.data.frame.vivid")
### * as.data.frame.vivid

flush(stderr()); flush(stdout())

### Name: as.data.frame.vivid
### Title: as.data.frame.vivid
### Aliases: as.data.frame.vivid

### ** Examples




cleanEx()
nameEx("pdpPairs")
### * pdpPairs

flush(stderr()); flush(stdout())

### Name: pdpPairs
### Title: pdpPairs
### Aliases: pdpPairs

### ** Examples

# Load in the data:
aq <- na.omit(airquality)
f <- lm(Ozone ~ ., data = aq)
pdpPairs(aq, f, "Ozone")



cleanEx()
nameEx("pdpVars")
### * pdpVars

flush(stderr()); flush(stdout())

### Name: pdpVars
### Title: pdpVars
### Aliases: pdpVars

### ** Examples




cleanEx()
nameEx("pdpZen")
### * pdpZen

flush(stderr()); flush(stdout())

### Name: pdpZen
### Title: Create a zenplot displaying partial dependence values.
### Aliases: pdpZen

### ** Examples

## Not run: 
##D # To use this function, install zenplots and graph from Bioconductor.
##D if (!requireNamespace("graph", quietly = TRUE)) {
##D   install.packages("BiocManager")
##D   BiocManager::install("graph")
##D }
##D install.packages("zenplots")
##D 
##D library(MASS)
##D library(ranger)
##D Boston1 <- Boston
##D Boston1$chas <- factor(Boston1$chas)
##D rf <- ranger(medv ~ ., data = Boston1)
##D pdpZen(Boston1[1:30, ], rf, response = "medv", zpath = names(Boston1)[1:4], comboImage = T)
##D # Find the top variables in rf
##D set.seed(123)
##D viv <- vivi(Boston1, rf, "medv", nmax = 30) # use 30 rows, for speed
##D pdpZen(Boston1, rf, response = "medv", zpath = rownames(viv)[1:4], comboImage = T)
##D zpath <- zPath(viv, cutoff = .2) # find plots whose interaction score exceeds .2
##D pdpZen(Boston1, rf, response = "medv", zpath = zpath, comboImage = T)
## End(Not run)



cleanEx()
nameEx("vip2vivid")
### * vip2vivid

flush(stderr()); flush(stdout())

### Name: vip2vivid
### Title: vip2vivid
### Aliases: vip2vivid

### ** Examples

## Not run: 
##D library(ranger)
##D library(vip)
##D aq <- na.omit(airquality) # get data
##D nameAq <- names(aq[-1]) # get feature names
##D 
##D rF <- ranger(Ozone ~ ., data = aq, importance = "permutation") # create ranger random forest fit
##D vImp <- vi(rF) # vip importance
##D vInt <- vint(rF, feature_names = nameAq) # vip interaction
##D 
##D vip2vivid(vImp, vInt)
## End(Not run)



cleanEx()
nameEx("vivi")
### * vivi

flush(stderr()); flush(stdout())

### Name: vivi
### Title: vivi
### Aliases: vivi

### ** Examples


aq <- na.omit(airquality)
f <- lm(Ozone ~ ., data = aq)
m <- vivi(fit = f, data = aq, response = "Ozone") # as expected all interactions are zero
viviHeatmap(m)

# Select importance metric
library(randomForest)
rf1 <- randomForest(Ozone~., data = aq, importance = TRUE)
m2 <- vivi(fit = rf1, data = aq, response = 'Ozone',
           importanceType = '%IncMSE') # select %IncMSE as the importance measure
viviHeatmap(m2)




cleanEx()
nameEx("viviHeatmap")
### * viviHeatmap

flush(stderr()); flush(stdout())

### Name: viviHeatmap
### Title: viviHeatmap
### Aliases: viviHeatmap

### ** Examples




cleanEx()
nameEx("viviNetwork")
### * viviNetwork

flush(stderr()); flush(stdout())

### Name: viviNetwork
### Title: viviNetwork
### Aliases: viviNetwork

### ** Examples




cleanEx()
nameEx("viviUpdate")
### * viviUpdate

flush(stderr()); flush(stdout())

### Name: viviUpdate
### Title: viviUpdate
### Aliases: viviUpdate

### ** Examples

f <- lm(Sepal.Length ~ ., data = iris[, -5])
m <- vivi(iris[, -5], f, "Sepal.Length")
corimp <- abs(cor(iris[, -5])[1, -1])
viviUpdate(m, corimp) # use correlation as updated importance



cleanEx()
nameEx("vividReorder")
### * vividReorder

flush(stderr()); flush(stdout())

### Name: vividReorder
### Title: vividReorder
### Aliases: vividReorder

### ** Examples

f <- lm(Sepal.Length ~ ., data = iris[, -5])
m <- vivi(fit = f, data = iris[, -5], response = "Sepal.Length")
corimp <- abs(cor(iris[, -5])[1, -1])
viviUpdate(m, corimp) # use correlation as importance and reorder




cleanEx()
nameEx("zPath")
### * zPath

flush(stderr()); flush(stdout())

### Name: zPath
### Title: zPath
### Aliases: zPath

### ** Examples

## Not run: 
##D # To use this function, install zenplots and graph from Bioconductor.
##D if (!requireNamespace("graph", quietly = TRUE)) {
##D   install.packages("BiocManager")
##D   BiocManager::install("graph")
##D }
##D install.packages("zenplots")
##D 
##D aq <- na.omit(airquality) * 1.0
##D 
##D # Run an mlr3 ranger model:
##D library(mlr3)
##D library(mlr3learners)
##D library(ranger)
##D ozonet <- TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
##D ozonel <- lrn("regr.ranger", importance = "permutation")
##D ozonef <- ozonel$train(ozonet)
##D 
##D viv <- vivi(aq, ozonef, "Ozone")
##D 
##D # Calculate Zpath:
##D zpath <- zPath(viv, .8)
##D zpath
## End(Not run)



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
