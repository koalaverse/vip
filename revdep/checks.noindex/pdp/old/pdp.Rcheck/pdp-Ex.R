pkgname <- "pdp"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('pdp')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("autoplot.partial")
### * autoplot.partial

flush(stderr()); flush(stdout())

### Name: autoplot.partial
### Title: Plotting Partial Dependence Functions
### Aliases: autoplot.partial autoplot.ice autoplot.cice

### ** Examples

## Not run: 
##D #
##D # Regression example (requires randomForest package to run)
##D #
##D 
##D # Load required packages
##D library(ggplot2)  # for autoplot() generic
##D library(gridExtra)  # for `grid.arrange()`
##D library(magrittr)  # for forward pipe operator `%>%`
##D library(randomForest)
##D 
##D # Fit a random forest to the Boston housing data
##D data (boston)  # load the boston housing data
##D set.seed(101)  # for reproducibility
##D boston.rf <- randomForest(cmedv ~ ., data = boston)
##D 
##D # Partial dependence of cmedv on lstat
##D boston.rf %>%
##D   partial(pred.var = "lstat") %>%
##D   autoplot(rug = TRUE, train = boston) + theme_bw()
##D 
##D # Partial dependence of cmedv on lstat and rm
##D boston.rf %>%
##D   partial(pred.var = c("lstat", "rm"), chull = TRUE, progress = TRUE) %>%
##D   autoplot(contour = TRUE, legend.title = "cmedv",
##D            option = "B", direction = -1) + theme_bw()
##D 
##D # ICE curves and c-ICE curves
##D age.ice <- partial(boston.rf, pred.var = "lstat", ice = TRUE)
##D grid.arrange(
##D   autoplot(age.ice, alpha = 0.1),                 # ICE curves
##D   autoplot(age.ice, center = TRUE, alpha = 0.1),  # c-ICE curves
##D   ncol = 2
##D )
## End(Not run)



cleanEx()
nameEx("boston")
### * boston

flush(stderr()); flush(stdout())

### Name: boston
### Title: Boston Housing Data
### Aliases: boston
### Keywords: datasets

### ** Examples

head(boston)




cleanEx()
nameEx("exemplar")
### * exemplar

flush(stderr()); flush(stdout())

### Name: exemplar
### Title: Exemplar observation
### Aliases: exemplar exemplar.data.frame exemplar.matrix
###   exemplar.dgCMatrix

### ** Examples

set.seed(1554)  # for reproducibility
train <- data.frame(
  x = rnorm(100),
  y = sample(letters[1L:3L], size = 100, replace = TRUE,
             prob = c(0.1, 0.1, 0.8))
)
exemplar(train)



cleanEx()
nameEx("partial")
### * partial

flush(stderr()); flush(stdout())

### Name: partial
### Title: Partial Dependence Functions
### Aliases: partial partial.default partial.model_fit

### ** Examples

## Not run: 
##D #
##D # Regression example (requires randomForest package to run)
##D #
##D 
##D # Fit a random forest to the boston housing data
##D library(randomForest)
##D data (boston)  # load the boston housing data
##D set.seed(101)  # for reproducibility
##D boston.rf <- randomForest(cmedv ~ ., data = boston)
##D 
##D # Using randomForest's partialPlot function
##D partialPlot(boston.rf, pred.data = boston, x.var = "lstat")
##D 
##D # Using pdp's partial function
##D head(partial(boston.rf, pred.var = "lstat"))  # returns a data frame
##D partial(boston.rf, pred.var = "lstat", plot = TRUE, rug = TRUE)
##D 
##D # The partial function allows for multiple predictors
##D partial(boston.rf, pred.var = c("lstat", "rm"), grid.resolution = 40,
##D         plot = TRUE, chull = TRUE, progress = TRUE)
##D 
##D # The plotPartial function offers more flexible plotting
##D pd <- partial(boston.rf, pred.var = c("lstat", "rm"), grid.resolution = 40)
##D plotPartial(pd, levelplot = FALSE, zlab = "cmedv", drape = TRUE,
##D             colorkey = FALSE, screen = list(z = -20, x = -60))
##D 
##D # The autplot function can be used to produce graphics based on ggplot2
##D library(ggplot2)
##D autoplot(pd, contour = TRUE, legend.title = "Partial\ndependence")
##D 
##D #
##D # Individual conditional expectation (ICE) curves
##D #
##D 
##D # Use partial to obtain ICE/c-ICE curves
##D rm.ice <- partial(boston.rf, pred.var = "rm", ice = TRUE)
##D plotPartial(rm.ice, rug = TRUE, train = boston, alpha = 0.2)
##D autoplot(rm.ice, center = TRUE, alpha = 0.2, rug = TRUE, train = boston)
##D 
##D #
##D # Classification example (requires randomForest package to run)
##D #
##D 
##D # Fit a random forest to the Pima Indians diabetes data
##D data (pima)  # load the boston housing data
##D set.seed(102)  # for reproducibility
##D pima.rf <- randomForest(diabetes ~ ., data = pima, na.action = na.omit)
##D 
##D # Partial dependence of positive test result on glucose (default logit scale)
##D partial(pima.rf, pred.var = "glucose", plot = TRUE, chull = TRUE,
##D         progress = TRUE)
##D 
##D # Partial dependence of positive test result on glucose (probability scale)
##D partial(pima.rf, pred.var = "glucose", prob = TRUE, plot = TRUE,
##D         chull = TRUE, progress = TRUE)
## End(Not run)



cleanEx()
nameEx("pima")
### * pima

flush(stderr()); flush(stdout())

### Name: pima
### Title: Pima Indians Diabetes Data
### Aliases: pima
### Keywords: datasets

### ** Examples

head(pima)




cleanEx()
nameEx("plotPartial")
### * plotPartial

flush(stderr()); flush(stdout())

### Name: plotPartial
### Title: Plotting Partial Dependence Functions
### Aliases: plotPartial plotPartial.ice plotPartial.cice
###   plotPartial.partial

### ** Examples

## Not run: 
##D #
##D # Regression example (requires randomForest package to run)
##D #
##D 
##D # Load required packages
##D library(gridExtra)  # for `grid.arrange()`
##D library(magrittr)  # for forward pipe operator `%>%`
##D library(randomForest)
##D 
##D # Fit a random forest to the Boston housing data
##D data (boston)  # load the boston housing data
##D set.seed(101)  # for reproducibility
##D boston.rf <- randomForest(cmedv ~ ., data = boston)
##D 
##D # Partial dependence of cmedv on lstat
##D boston.rf %>%
##D   partial(pred.var = "lstat") %>%
##D   plotPartial(rug = TRUE, train = boston)
##D 
##D # Partial dependence of cmedv on lstat and rm
##D boston.rf %>%
##D   partial(pred.var = c("lstat", "rm"), chull = TRUE, progress = TRUE) %>%
##D   plotPartial(contour = TRUE, legend.title = "rm")
##D 
##D # ICE curves and c-ICE curves
##D age.ice <- partial(boston.rf, pred.var = "lstat", ice = TRUE)
##D p1 <- plotPartial(age.ice, alpha = 0.1)
##D p2 <- plotPartial(age.ice, center = TRUE, alpha = 0.1)
##D grid.arrange(p1, p2, ncol = 2)
## End(Not run)



cleanEx()
nameEx("topPredictors")
### * topPredictors

flush(stderr()); flush(stdout())

### Name: topPredictors
### Title: Extract Most "Important" Predictors (Experimental)
### Aliases: topPredictors topPredictors.default topPredictors.train

### ** Examples

## Not run: 
##D #
##D # Regression example (requires randomForest package to run)
##D #
##D 
##D Load required packages
##D library(ggplot2)
##D library(randomForest)
##D 
##D # Fit a random forest to the mtcars dataset
##D data(mtcars, package = "datasets")
##D set.seed(101)
##D mtcars.rf <- randomForest(mpg ~ ., data = mtcars, mtry = 5, importance = TRUE)
##D 
##D # Topfour predictors
##D top4 <- topPredictors(mtcars.rf, n = 4)
##D 
##D # Construct partial dependence functions for top four predictors
##D pd <- NULL
##D for (i in top4) {
##D   tmp <- partial(mtcars.rf, pred.var = i)
##D   names(tmp) <- c("x", "y")
##D   pd <- rbind(pd,  cbind(tmp, predictor = i))
##D }
##D 
##D # Display partial dependence functions
##D ggplot(pd, aes(x, y)) +
##D   geom_line() +
##D   facet_wrap(~ predictor, scales = "free") +
##D   theme_bw() +
##D   ylab("mpg")
##D 
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
