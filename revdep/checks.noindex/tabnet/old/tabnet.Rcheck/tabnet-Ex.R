pkgname <- "tabnet"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('tabnet')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("autoplot.tabnet_explain")
### * autoplot.tabnet_explain

flush(stderr()); flush(stdout())

### Name: autoplot.tabnet_explain
### Title: Plot tabnet_explain mask importance heatmap
### Aliases: autoplot.tabnet_explain

### ** Examples





cleanEx()
nameEx("autoplot.tabnet_fit")
### * autoplot.tabnet_fit

flush(stderr()); flush(stdout())

### Name: autoplot.tabnet_fit
### Title: Plot tabnet_fit model loss along epochs
### Aliases: autoplot.tabnet_fit autoplot.tabnet_pretrain

### ** Examples




cleanEx()
nameEx("tabnet")
### * tabnet

flush(stderr()); flush(stdout())

### Name: tabnet
### Title: Parsnip compatible tabnet model
### Aliases: tabnet

### ** Examples

if (torch::torch_is_installed()) {
library(parsnip)
data("ames", package = "modeldata")
model <- tabnet() %>%
  set_mode("regression") %>%
  set_engine("torch")
model %>%
  fit(Sale_Price ~ ., data = ames)
}




cleanEx()
nameEx("tabnet_explain")
### * tabnet_explain

flush(stderr()); flush(stdout())

### Name: tabnet_explain
### Title: Interpretation metrics from a TabNet model
### Aliases: tabnet_explain tabnet_explain.default
###   tabnet_explain.tabnet_fit tabnet_explain.tabnet_pretrain
###   tabnet_explain.model_fit

### ** Examples


if (torch::torch_is_installed()) {

set.seed(2021)

n <- 1000
x <- data.frame(
  x = rnorm(n),
  y = rnorm(n),
  z = rnorm(n)
)

y <- x$x

fit <- tabnet_fit(x, y, epochs = 20,
                  num_steps = 1,
                  batch_size = 512,
                  attention_width = 1,
                  num_shared = 1,
                  num_independent = 1)


 ex <- tabnet_explain(fit, x)

}




cleanEx()
nameEx("tabnet_fit")
### * tabnet_fit

flush(stderr()); flush(stdout())

### Name: tabnet_fit
### Title: Tabnet model
### Aliases: tabnet_fit tabnet_fit.default tabnet_fit.data.frame
###   tabnet_fit.formula tabnet_fit.recipe

### ** Examples

if (torch::torch_is_installed()) {

# regression using formula specification
data("ames", package = "modeldata")
fit <- tabnet_fit(Sale_Price ~ ., data = ames, epochs = 1)

# classification using data-frame specification
data("attrition", package = "modeldata")
attrition_x <- attrition[,-which(names(attrition) == "Attrition")]
fit <- tabnet_fit(attrition_x, attrition$Attrition, epochs = 1)
}




cleanEx()
nameEx("tabnet_pretrain")
### * tabnet_pretrain

flush(stderr()); flush(stdout())

### Name: tabnet_pretrain
### Title: Tabnet model
### Aliases: tabnet_pretrain tabnet_pretrain.default
###   tabnet_pretrain.data.frame tabnet_pretrain.formula
###   tabnet_pretrain.recipe

### ** Examples

if (torch::torch_is_installed()) {
data("ames", package = "modeldata")
pretrained <- tabnet_pretrain(Sale_Price ~ ., data = ames, epochs = 1)
}




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
