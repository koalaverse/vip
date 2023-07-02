pkgname <- "SAEforest"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('SAEforest')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("MERFranger")
### * MERFranger

flush(stderr()); flush(stdout())

### Name: MERFranger
### Title: Main function for unit-level MERF
### Aliases: MERFranger

### ** Examples




cleanEx()
nameEx("SAEforestObject")
### * SAEforestObject

flush(stderr()); flush(stdout())

### Name: SAEforestObject
### Title: Fitted 'SAEforest' object
### Aliases: SAEforestObject

### ** Examples




cleanEx()
nameEx("SAEforest_model")
### * SAEforest_model

flush(stderr()); flush(stdout())

### Name: SAEforest_model
### Title: Main function for the estimation of domain-level (nonlinear)
###   indicators with MERFs
### Aliases: SAEforest_model

### ** Examples





cleanEx()
nameEx("map_indicators")
### * map_indicators

flush(stderr()); flush(stdout())

### Name: map_indicators
### Title: Visualizes disaggregated estimates on a map
### Aliases: map_indicators

### ** Examples





cleanEx()
nameEx("plot.SAEforest")
### * plot.SAEforest

flush(stderr()); flush(stdout())

### Name: plot.SAEforest
### Title: Plot function for a 'SAEforest' object
### Aliases: plot.SAEforest

### ** Examples





cleanEx()
nameEx("summarize_indicators")
### * summarize_indicators

flush(stderr()); flush(stdout())

### Name: summarize_indicators
### Title: Presents point, MSE and CV estimates
### Aliases: summarize_indicators

### ** Examples




cleanEx()
nameEx("summary.SAEforest")
### * summary.SAEforest

flush(stderr()); flush(stdout())

### Name: summary.SAEforest
### Title: Summarizes an 'SAEforest' object
### Aliases: summary.SAEforest

### ** Examples




cleanEx()
nameEx("tune_parameters")
### * tune_parameters

flush(stderr()); flush(stdout())

### Name: tune_parameters
### Title: Tuning and cross-validation of MERF parameters
### Aliases: tune_parameters

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
