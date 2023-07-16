pkgname <- "moreparty"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('moreparty')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("BivariateAssoc")
### * BivariateAssoc

flush(stderr()); flush(stdout())

### Name: BivariateAssoc
### Title: Bivariate association measures for supervised learning tasks.
### Aliases: BivariateAssoc

### ** Examples

  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  BivariateAssoc(iris2$Species,iris2[,1:4])



cleanEx()
nameEx("EasyTreeVarImp")
### * EasyTreeVarImp

flush(stderr()); flush(stdout())

### Name: EasyTreeVarImp
### Title: Variable importance for conditional inference trees.
### Aliases: EasyTreeVarImp
### Keywords: tree

### ** Examples

  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.ct = partykit::ctree(Species ~ ., data = iris2)
  EasyTreeVarImp(iris.ct, nsim = 1)



cleanEx()
nameEx("FeatureSelection")
### * FeatureSelection

flush(stderr()); flush(stdout())

### Name: FeatureSelection
### Title: Feature selection for conditional random forests.
### Aliases: FeatureSelection

### ** Examples

  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  featsel <- FeatureSelection(iris2$Species, iris2[,1:4], measure='ACC', ntree=200)
  featsel$selection.0se
  featsel$selection.1se



cleanEx()
nameEx("GetAleData")
### * GetAleData

flush(stderr()); flush(stdout())

### Name: GetAleData
### Title: Accumulated Local Effects for a conditional random forest.
### Aliases: GetAleData
### Keywords: tree

### ** Examples

  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2, 
              controls = party::cforest_unbiased(mtry=2, ntree=50))
  GetAleData(iris.cf)



cleanEx()
nameEx("GetCtree")
### * GetCtree

flush(stderr()); flush(stdout())

### Name: GetCtree
### Title: Gets a tree from a conditional random forest
### Aliases: GetCtree
### Keywords: tree

### ** Examples

  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2,
            control = party::cforest_unbiased(mtry = 2, ntree = 50))
  plot(GetCtree(iris.cf))



cleanEx()
nameEx("GetInteractionStrength")
### * GetInteractionStrength

flush(stderr()); flush(stdout())

### Name: GetInteractionStrength
### Title: Strength of interactions
### Aliases: GetInteractionStrength
### Keywords: tree

### ** Examples

  ## Not run: 
##D   data(iris)
##D   iris2 = iris
##D   iris2$Species = factor(iris$Species == "versicolor")
##D   iris.cf = party::cforest(Species ~ ., data = iris2, 
##D               controls = party::cforest_unbiased(mtry=2, ntree=50))
##D   GetInteractionStrength(iris.cf)
##D   
## End(Not run)



cleanEx()
nameEx("GetPartialData")
### * GetPartialData

flush(stderr()); flush(stdout())

### Name: GetPartialData
### Title: Partial dependence for a conditional random forest.
### Aliases: GetPartialData
### Keywords: tree

### ** Examples

  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2, 
              controls = party::cforest_unbiased(mtry=2, ntree=50))
  GetPartialData(iris.cf)



cleanEx()
nameEx("GetSplitStats")
### * GetSplitStats

flush(stderr()); flush(stdout())

### Name: GetSplitStats
### Title: Permutation tests results for each split in a conditional tree.
### Aliases: GetSplitStats
### Keywords: tree

### ** Examples

  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.ct = partykit::ctree(Species ~ ., data = iris2)
  GetSplitStats(iris.ct)



cleanEx()
nameEx("NiceTreePlot")
### * NiceTreePlot

flush(stderr()); flush(stdout())

### Name: NiceTreePlot
### Title: Plots conditional inference trees.
### Aliases: NiceTreePlot
### Keywords: tree

### ** Examples

  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.ct = partykit::ctree(Species ~ ., data = iris2)
  NiceTreePlot(iris.ct, inner_plots = TRUE)



cleanEx()
nameEx("Outliers")
### * Outliers

flush(stderr()); flush(stdout())

### Name: Outliers
### Title: Computes outliers
### Aliases: Outliers
### Keywords: classif

### ** Examples

  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2,
            control = party::cforest_unbiased(mtry = 2, ntree = 50))
  prox=proximity(iris.cf)
  Outliers(prox, iris2$Species, iris2[,1:4])



cleanEx()
nameEx("Prototypes")
### * Prototypes

flush(stderr()); flush(stdout())

### Name: Prototypes
### Title: Prototypes of groups
### Aliases: Prototypes
### Keywords: classif

### ** Examples

  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2,
            control = party::cforest_unbiased(mtry = 2, ntree = 50))
  prox=proximity(iris.cf)
  Prototypes(iris2$Species,iris2[,1:4],prox)



cleanEx()
nameEx("SurrogateTree")
### * SurrogateTree

flush(stderr()); flush(stdout())

### Name: SurrogateTree
### Title: Surrogate tree for conditional inference random forests
### Aliases: SurrogateTree

### ** Examples

  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2,
            control = party::cforest_unbiased(mtry = 2, ntree = 50))
  surro <- SurrogateTree(iris.cf)
  surro$r.squared
  plot(surro$tree)



cleanEx()
nameEx("ctree-module")
### * ctree-module

flush(stderr()); flush(stdout())

### Name: ctree-module
### Title: Shiny module to build and analyse conditional inference trees
### Aliases: ctree-module ctreeUI ctreeServer
### Keywords: tree

### ** Examples


library(shiny)
library(moreparty)

data(titanic)

ui <- fluidPage(
  titlePanel("Conditional inference trees"),
  ctreeUI(id = "ctree_app")
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    data = titanic,
    name = deparse(substitute(titanic))
  )
  ctreeServer(id = "ctree_app", reactive(rv$data), reactive(rv$name))
}

if (interactive())
  shinyApp(ui, server)



cleanEx()
nameEx("fastcforest")
### * fastcforest

flush(stderr()); flush(stdout())

### Name: fastcforest
### Title: Parallelized conditional inference random forest
### Aliases: fastcforest
### Keywords: tree

### ** Examples

  ## classification
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species=="versicolor")
  iris.cf = fastcforest(Species~., data=iris2, parallel=FALSE)



cleanEx()
nameEx("fastvarImp")
### * fastvarImp

flush(stderr()); flush(stdout())

### Name: fastvarImp
### Title: Variable importance for conditional inference random forests
### Aliases: fastvarImp

### ** Examples

  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2,
            control = party::cforest_unbiased(mtry = 2, ntree = 50))
  fastvarImp(object = iris.cf, measure='ACC', parallel=FALSE)          



cleanEx()
nameEx("fastvarImpAUC")
### * fastvarImpAUC

flush(stderr()); flush(stdout())

### Name: fastvarImpAUC
### Title: Variable importance (with AUC performance measure) for
###   conditional inference random forests
### Aliases: fastvarImpAUC

### ** Examples

  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2,
            control = party::cforest_unbiased(mtry = 2, ntree = 50))
  fastvarImpAUC(object = iris.cf, parallel = FALSE)



cleanEx()
nameEx("ggForestEffects")
### * ggForestEffects

flush(stderr()); flush(stdout())

### Name: ggForestEffects
### Title: Dot plot of covariates effects
### Aliases: ggForestEffects
### Keywords: aplot tree

### ** Examples

  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2, controls = cforest_unbiased(mtry=2))
  ale <- GetAleData(iris.cf)
  ale$cat <- paste(ale$var,ale$cat,sep='_')  # to avoid duplicated categories
  ggForestEffects(ale)



cleanEx()
nameEx("ggVarImp")
### * ggVarImp

flush(stderr()); flush(stdout())

### Name: ggVarImp
### Title: Dot plot of variable importance
### Aliases: ggVarImp
### Keywords: aplot tree

### ** Examples

  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2,
            control = party::cforest_unbiased(mtry = 2, ntree = 50))
  imp <- fastvarImpAUC(object = iris.cf, parallel = FALSE)
  ggVarImp(imp)



cleanEx()
nameEx("ictree")
### * ictree

flush(stderr()); flush(stdout())

### Name: ictree
### Title: An interactive app for conditional inference trees
### Aliases: ictree
### Keywords: tree

### ** Examples

if (interactive()) {
ictree(iris)
}



cleanEx()
nameEx("titanic")
### * titanic

flush(stderr()); flush(stdout())

### Name: titanic
### Title: Titanic dataset
### Aliases: titanic
### Keywords: datasets

### ** Examples

data(titanic)
str(titanic)



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
