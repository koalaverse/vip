pkgname <- "ENMTools"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ENMTools')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("assert.extras")
### * assert.extras

flush(stderr()); flush(stdout())

### Name: assert.extras
### Title: Assert that the extra packages needed for an ENMTools function
###   are installed and available
### Aliases: assert.extras

### ** Examples

if(check.extras("enmtools.gam")) {
  assert.extras("enmtools.gam")
}



cleanEx()
nameEx("background.raster.buffer")
### * background.raster.buffer

flush(stderr()); flush(stdout())

### Name: background.raster.buffer
### Title: Takes a set of points, a buffer radius, and a mask and returns a
###   raster based on that buffer radius. Code modified from Elith and
###   Hijmans SDM with R tutorial
### Aliases: background.raster.buffer

### ** Examples

library(ENMTools)
background.raster.buffer(iberolacerta.clade$species$cyreni$presence.points, 100000, euro.worldclim)



cleanEx()
nameEx("background.test")
### * background.test

flush(stderr()); flush(stdout())

### Name: background.test
### Title: background.test Conduct a background test (also called a
###   similarity test), as described in Warren et al. 2008. This test can
###   either be run as an asymmetric test (species.1 vs background of
###   species.2) or as a symmetric test (background of species.1 vs
###   background of species.2). For GLM, Bioclim, and Domain models the
###   replicates will be constructed from the background points supplied
###   for each species.  For Maxent, the replicates will be constructed
###   from the range rasters stored in the enmtools.species objects.
### Aliases: background.test
### Keywords: background enmtools equivalency hypothesis testing

### ** Examples




cleanEx()
nameEx("calc.B1")
### * calc.B1

flush(stderr()); flush(stdout())

### Name: calc.B1
### Title: calc.B1, Calculates standardized version of Levins (1968) B1
###   measure of niche breadth given a vector of suitabilities
### Aliases: calc.B1
### Keywords: breadth enm niche sdm

### ** Examples

calc.B1(c(1, .001, .001))



cleanEx()
nameEx("calc.B2")
### * calc.B2

flush(stderr()); flush(stdout())

### Name: calc.B2
### Title: calc.B2, Calculates standardized version of Levins (1968) B2
###   measure of niche breadth given a vector of suitabilities
### Aliases: calc.B2
### Keywords: breadth enm niche sdm

### ** Examples

calc.B2(c(1, .001, .001))



cleanEx()
nameEx("check.clade")
### * check.clade

flush(stderr()); flush(stdout())

### Name: check.clade
### Title: Checking for complians of an enmtools.clade object
### Aliases: check.clade

### ** Examples

check.clade(iberolacerta.clade)



cleanEx()
nameEx("check.env")
### * check.env

flush(stderr()); flush(stdout())

### Name: check.env
### Title: Automating some basic tasks for using a raster stack for
###   modeling. Checks rasters for same extent and resolution, and sets
###   values in each layer to NA if there is an NA in any other layer.
### Aliases: check.env

### ** Examples

check.env(euro.worldclim)



cleanEx()
nameEx("check.extras")
### * check.extras

flush(stderr()); flush(stdout())

### Name: check.extras
### Title: Check that the extra packages needed for an ENMTools function
###   are installed and available
### Aliases: check.extras

### ** Examples

check.extras("enmtools.gam")



cleanEx()
nameEx("check.species")
### * check.species

flush(stderr()); flush(stdout())

### Name: check.species
### Title: Checking compliance for an object of class enmtools.species.
### Aliases: check.species

### ** Examples

check.species(iberolacerta.clade$species$monticola)



cleanEx()
nameEx("clamp.env")
### * clamp.env

flush(stderr()); flush(stdout())

### Name: clamp.env
### Title: Takes an emtools.model object and a set of environment layers
###   and clamps the environment layers so that no variable falls outside
###   of the range available in the training data.
### Aliases: clamp.env

### ** Examples




cleanEx()
nameEx("combine.species")
### * combine.species

flush(stderr()); flush(stdout())

### Name: combine.species
### Title: Takes a list of enmtools.species objects and combines them into
###   a single enmtools.species object
### Aliases: combine.species

### ** Examples

combine.species(iberolacerta.clade$species)



cleanEx()
nameEx("drop.species")
### * drop.species

flush(stderr()); flush(stdout())

### Name: drop.species
### Title: Takes a an ENMTools clade object and a vector of species names.
###   Drops the species from the tree and removes data from the clade
###   object.
### Aliases: drop.species

### ** Examples




cleanEx()
nameEx("enmtools.bc")
### * enmtools.bc

flush(stderr()); flush(stdout())

### Name: enmtools.bc
### Title: Takes an emtools.species object with presence and background
###   points, and builds a Bioclim model
### Aliases: enmtools.bc

### ** Examples




cleanEx()
nameEx("enmtools.calibrate")
### * enmtools.calibrate

flush(stderr()); flush(stdout())

### Name: enmtools.calibrate
### Title: Takes an emtools.model object, and reformats it to run through
###   the CalibratR package, calculates Continuous Boyce Index, and runs a
###   Hosmer-Lemeshow goodness-of-fit test.  Can either do a full CalibratR
###   run or just return ECE/MCE statistics and plots.
### Aliases: enmtools.calibrate

### ** Examples




cleanEx()
nameEx("enmtools.dm")
### * enmtools.dm

flush(stderr()); flush(stdout())

### Name: enmtools.dm
### Title: Takes an emtools.species object with presence and background
###   points, and builds a Domain model
### Aliases: enmtools.dm

### ** Examples




cleanEx()
nameEx("enmtools.ecospat.bg")
### * enmtools.ecospat.bg

flush(stderr()); flush(stdout())

### Name: enmtools.ecospat.bg
### Title: enmtools.ecospat.bg, Runs an ecospat background/similarity test
###   using enmtool.species objects.
### Aliases: enmtools.ecospat.bg
### Keywords: enm niche plot sdm

### ** Examples




cleanEx()
nameEx("enmtools.ecospat.id")
### * enmtools.ecospat.id

flush(stderr()); flush(stdout())

### Name: enmtools.ecospat.id
### Title: enmtools.ecospat.id, Runs an ecospat identity test using
###   enmtool.species objects.
### Aliases: enmtools.ecospat.id
### Keywords: enm niche plot sdm

### ** Examples




cleanEx()
nameEx("enmtools.gam")
### * enmtools.gam

flush(stderr()); flush(stdout())

### Name: enmtools.gam
### Title: Takes an emtools.species object with presence and background
###   points, and builds a gam
### Aliases: enmtools.gam

### ** Examples




cleanEx()
nameEx("enmtools.glm")
### * enmtools.glm

flush(stderr()); flush(stdout())

### Name: enmtools.glm
### Title: Takes an enmtools.species object with presence and background
###   points, and builds a GLM
### Aliases: enmtools.glm

### ** Examples

enmtools.glm(iberolacerta.clade$species$monticola, env = euro.worldclim, f = pres ~ bio1 + bio9)



cleanEx()
nameEx("enmtools.hypervolume")
### * enmtools.hypervolume

flush(stderr()); flush(stdout())

### Name: enmtools.hypervolume
### Title: THIS FUNCTION IS CURRENTLY DISABLED.  Takes an emtools.species
###   object and environmental layers, and constructs a hypervolume using
###   the R package hypervolume
### Aliases: enmtools.hypervolume

### ** Examples




cleanEx()
nameEx("enmtools.maxent")
### * enmtools.maxent

flush(stderr()); flush(stdout())

### Name: enmtools.maxent
### Title: Takes an emtools.species object with presence and background
###   points, and builds a maxent model
### Aliases: enmtools.maxent

### ** Examples

if(check.extras("enmtools.maxent")) {
    ## maxent is not working on some platforms so use try()
    try(enmtools.maxent(iberolacerta.clade$species$monticola, env = euro.worldclim))
}



cleanEx()
nameEx("enmtools.rf")
### * enmtools.rf

flush(stderr()); flush(stdout())

### Name: enmtools.rf
### Title: Takes an emtools.species object with presence and background
###   points, and builds a random forest model
### Aliases: enmtools.rf

### ** Examples




cleanEx()
nameEx("enmtools.rf.ranger")
### * enmtools.rf.ranger

flush(stderr()); flush(stdout())

### Name: enmtools.rf.ranger
### Title: Takes an emtools.species object with presence and background
###   points, and builds a random forest model using the 'probability mode'
###   in package 'ranger'
### Aliases: enmtools.rf.ranger

### ** Examples




cleanEx()
nameEx("enmtools.vip")
### * enmtools.vip

flush(stderr()); flush(stdout())

### Name: enmtools.vip
### Title: Takes an enmtools.model object, and performs variable importance
###   analyses on it using methods from the vip package
### Aliases: enmtools.vip

### ** Examples




cleanEx()
nameEx("env.breadth")
### * env.breadth

flush(stderr()); flush(stdout())

### Name: env.breadth
### Title: Calculates breadth of a model in environment space using latin
###   hypercube sampling
### Aliases: env.breadth

### ** Examples




cleanEx()
nameEx("env.evaluate")
### * env.evaluate

flush(stderr()); flush(stdout())

### Name: env.evaluate
### Title: Calculates evaluation metrics (AUC, etc.) using latin hypercube
###   sampling in environment space
### Aliases: env.evaluate

### ** Examples




cleanEx()
nameEx("env.overlap")
### * env.overlap

flush(stderr()); flush(stdout())

### Name: env.overlap
### Title: Calculates overlap between models in environment space using
###   latin hypercube sampling
### Aliases: env.overlap

### ** Examples




cleanEx()
nameEx("find.extras")
### * find.extras

flush(stderr()); flush(stdout())

### Name: find.extras
### Title: Find the extra packages needed for an ENMTools function
### Aliases: find.extras

### ** Examples

find.extras("enmtools.calibrate")



cleanEx()
nameEx("find.extras.missing")
### * find.extras.missing

flush(stderr()); flush(stdout())

### Name: find.extras.missing
### Title: Find the extra packages needed for an ENMTools function that are
###   currently missing (not available)
### Aliases: find.extras.missing

### ** Examples

find.extras.missing("enmtools.calibrate")



cleanEx()
nameEx("geog.range.overlap")
### * geog.range.overlap

flush(stderr()); flush(stdout())

### Name: geog.range.overlap
### Title: Takes two emtools.species objects with range rasters, calculates
###   overlap between them as in Fitzpatrick and Turelli 2006.  This metric
###   divides the area of overlap between two species ranges by the smaller
###   of the two areas of the species' individual ranges.  It therefore
###   ranges from 0 (no overlap) to 1 (ranges are the same or the smaller
###   species' range is contained entirely within the larger).
### Aliases: geog.range.overlap

### ** Examples

cyreni <- iberolacerta.clade$species$cyreni
monticola <- iberolacerta.clade$species$monticola
cyreni$range <- background.raster.buffer(cyreni$presence.points, 100000, euro.worldclim)
monticola$range <- background.raster.buffer(monticola$presence.points, 100000, euro.worldclim)
geog.range.overlap(cyreni, monticola)



cleanEx()
nameEx("hypervolume.overlap")
### * hypervolume.overlap

flush(stderr()); flush(stdout())

### Name: hypervolume.overlap
### Title: Takes an emtools.species object and environmental layers, and
###   constructs a hypervolume using the R package hypervolume
### Aliases: hypervolume.overlap

### ** Examples




cleanEx()
nameEx("identity.test")
### * identity.test

flush(stderr()); flush(stdout())

### Name: identity.test
### Title: identity.test Conduct a niche identity/equivalency test as
###   described in Warren et al. 2008.
### Aliases: identity.test
### Keywords: enmtools equivalency hypothesis-testing identity

### ** Examples




cleanEx()
nameEx("install.extras")
### * install.extras

flush(stderr()); flush(stdout())

### Name: install.extras
### Title: install.extras
### Aliases: install.extras

### ** Examples




cleanEx()
nameEx("marginal.plots")
### * marginal.plots

flush(stderr()); flush(stdout())

### Name: marginal.plots
### Title: marginal.plots Plots the marginal response of a model to an
###   environmental variable with all other variables held at their mean in
###   env
### Aliases: marginal.plots
### Keywords: enm plot response sdm

### ** Examples

cyreni.glm <- enmtools.glm(iberolacerta.clade$species$cyreni,
f = pres ~ bio1 + bio12, euro.worldclim)
marginal.plots(cyreni.glm, euro.worldclim, "bio1")



cleanEx()
nameEx("multistack.pca")
### * multistack.pca

flush(stderr()); flush(stdout())

### Name: multistack.pca
### Title: multistack.pca, simultaneous PCA on more than one stack of
###   environmental rasters
### Aliases: multistack.pca
### Keywords: environment pca raster

### ** Examples

test1 <- terra::crop(euro.worldclim, terra::ext(-10, -5, 40, 43))
test2 <- terra::crop(euro.worldclim, terra::ext(-5, 5, 40, 48))
test3 <- terra::crop(euro.worldclim, terra::ext(5, 15, 44, 48))
multistack.pca(test1, test2, test3)



cleanEx()
nameEx("point.overlap")
### * point.overlap

flush(stderr()); flush(stdout())

### Name: point.overlap
### Title: Takes two emtools.species objects with range rasters, calculates
###   overlap between them as in Cardillo and Warren 2016
### Aliases: point.overlap

### ** Examples




cleanEx()
nameEx("rangebreak.blob")
### * rangebreak.blob

flush(stderr()); flush(stdout())

### Name: rangebreak.blob
### Title: rangebreak.blob Conduct a blob rangebreak test as described in
###   Glor and Warren 2011.
### Aliases: rangebreak.blob
### Keywords: barrier biogeography enmtools hypothesis-testing rangebreak

### ** Examples




cleanEx()
nameEx("rangebreak.linear")
### * rangebreak.linear

flush(stderr()); flush(stdout())

### Name: rangebreak.linear
### Title: rangebreak.linear Conduct a linear rangebreak test as described
###   in Glor and Warren 2011.
### Aliases: rangebreak.linear
### Keywords: barrier biogeography enmtools hypothesis-testing rangebreak

### ** Examples




cleanEx()
nameEx("rangebreak.ribbon")
### * rangebreak.ribbon

flush(stderr()); flush(stdout())

### Name: rangebreak.ribbon
### Title: rangebreak.ribbon Conduct a ribbon rangebreak test as described
###   in Glor and Warren 2011.
### Aliases: rangebreak.ribbon
### Keywords: barrier biogeography enmtools hypothesis-testing rangebreak

### ** Examples




cleanEx()
nameEx("raster.breadth")
### * raster.breadth

flush(stderr()); flush(stdout())

### Name: raster.breadth
### Title: raster.breadth, applies measures of niche breadth to an ENM
### Aliases: raster.breadth
### Keywords: keywords

### ** Examples

aurelioi.glm <- enmtools.glm(iberolacerta.clade$species$aurelioi, euro.worldclim,
f = pres ~ bio1 + bio12)
raster.breadth(aurelioi.glm)



cleanEx()
nameEx("raster.cor")
### * raster.cor

flush(stderr()); flush(stdout())

### Name: raster.cor
### Title: Calculates the correlation coefficient between two rasters.
### Aliases: raster.cor

### ** Examples

data(euro.worldclim)
raster.cor(euro.worldclim[[1]], euro.worldclim[[2]])



cleanEx()
nameEx("raster.cor.matrix")
### * raster.cor.matrix

flush(stderr()); flush(stdout())

### Name: raster.cor.matrix
### Title: Takes a raster stack and returns a data frame containing Pearson
###   correlation coefficients between the included rasters
### Aliases: raster.cor.matrix

### ** Examples




cleanEx()
nameEx("raster.cor.plot")
### * raster.cor.plot

flush(stderr()); flush(stdout())

### Name: raster.cor.plot
### Title: Takes a raster stack and returns a data frame containing Pearson
###   correlation coefficients between the included rasters
### Aliases: raster.cor.plot

### ** Examples

data(euro.worldclim)
raster.cor.plot(euro.worldclim)



cleanEx()
nameEx("raster.overlap")
### * raster.overlap

flush(stderr()); flush(stdout())

### Name: raster.overlap
### Title: raster.overlap, measures overlap between two ENMs
### Aliases: raster.overlap
### Keywords: keywords

### ** Examples

aurelioi.glm <- enmtools.glm(iberolacerta.clade$species$aurelioi,
euro.worldclim, f = pres ~ bio1 + bio12)
aranica.glm <- enmtools.glm(iberolacerta.clade$species$aranica,
euro.worldclim, f = pres ~ bio1 + bio12)
raster.overlap(aurelioi.glm, aranica.glm)



cleanEx()
nameEx("raster.pca")
### * raster.pca

flush(stderr()); flush(stdout())

### Name: raster.pca
### Title: raster.pca, PCA on a set of environmental rasters
### Aliases: raster.pca
### Keywords: environment pca raster

### ** Examples

env.pca <- raster.pca(euro.worldclim, 2)



cleanEx()
nameEx("raster.resid")
### * raster.resid

flush(stderr()); flush(stdout())

### Name: raster.resid
### Title: raster.resid Measure standardized residuals from a linear
###   regression between two rasters.
### Aliases: raster.resid
### Keywords: correlation raster residuals

### ** Examples

data(euro.worldclim)
raster.resid(euro.worldclim[[1]], euro.worldclim[[2]])



cleanEx()
nameEx("raster.standardize")
### * raster.standardize

flush(stderr()); flush(stdout())

### Name: raster.standardize
### Title: raster.standardize, standardizes all values in a raster file
### Aliases: raster.standardize
### Keywords: keywords

### ** Examples

raster.standardize(euro.worldclim[[1]])



cleanEx()
nameEx("threespace.plot")
### * threespace.plot

flush(stderr()); flush(stdout())

### Name: threespace.plot
### Title: threespace.plot, A plot that compares the environmental
###   distribution of presence points, background points, and the set of
###   supplied environmental layers.
### Aliases: threespace.plot
### Keywords: background comparison environment extrapolation pca presence

### ** Examples




cleanEx()
nameEx("trimdupes.by.raster")
### * trimdupes.by.raster

flush(stderr()); flush(stdout())

### Name: trimdupes.by.raster
### Title: Takes a set of points and a raster mask and returns a data frame
###   trimmed so that only one point is returned per grid cell in the mask
###   raster.
### Aliases: trimdupes.by.raster

### ** Examples

pts <- iberolacerta.clade$species$monticola$presence.points
trimdupes.by.raster(pts, euro.worldclim)



cleanEx()
nameEx("visualize.enm")
### * visualize.enm

flush(stderr()); flush(stdout())

### Name: visualize.enm
### Title: visualize.enm, Makes a heatmap of suitability of habitat in
###   environment space according to a given model
### Aliases: visualize.enm
### Keywords: enm niche plot sdm

### ** Examples

aurelioi.glm <- enmtools.glm(iberolacerta.clade$species$aurelioi, euro.worldclim,
f = pres ~ poly(bio1, 4) + poly(bio12, 4))
visualize.enm(aurelioi.glm, euro.worldclim, layers = c("bio1", "bio12"))



cleanEx()
nameEx("visualize.overlap")
### * visualize.overlap

flush(stderr()); flush(stdout())

### Name: visualize.overlap
### Title: visualize.overlap, Makes a contour map of suitability of habitat
###   in environment space for two models
### Aliases: visualize.overlap
### Keywords: enm niche overlap plot sdm

### ** Examples




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
