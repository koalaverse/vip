pkgname <- "workboots"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('workboots')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("predict_boots")
### * predict_boots

flush(stderr()); flush(stdout())

### Name: predict_boots
### Title: Fit and predict from a workflow using many bootstrap resamples.
### Aliases: predict_boots

### ** Examples

## Not run: 
##D library(tidymodels)
##D 
##D # setup a workflow without fitting
##D wf <-
##D   workflow() %>%
##D   add_recipe(recipe(qsec ~ wt, data = mtcars)) %>%
##D   add_model(linear_reg())
##D 
##D # fit and predict 2000 bootstrap resampled models to mtcars
##D set.seed(123)
##D wf %>%
##D   predict_boots(n = 2000, training_data = mtcars, new_data = mtcars)
## End(Not run)



cleanEx()
nameEx("summarise_importance")
### * summarise_importance

flush(stderr()); flush(stdout())

### Name: summarise_importance
### Title: Append a tibble of variable importances returned by 'vi_boots()'
###   with upper and lower bounds.
### Aliases: summarise_importance summarize_importance

### ** Examples

## Not run: 
##D library(tidymodels)
##D 
##D # setup a workflow without fitting
##D wf <-
##D   workflow() %>%
##D   add_recipe(recipe(qsec ~ wt, data = mtcars)) %>%
##D   add_model(linear_reg())
##D 
##D # evaluate variable importance from 2000 models fit to mtcars
##D set.seed(123)
##D importances <-
##D   wf %>%
##D   vi_boots(n = 2000, training_data = mtcars, new_data = mtcars)
##D 
##D # append with lower and upper bound importance summary columns
##D importances %>%
##D   summarise_importance(interval_width = 0.95)
## End(Not run)



cleanEx()
nameEx("summarise_predictions")
### * summarise_predictions

flush(stderr()); flush(stdout())

### Name: summarise_predictions
### Title: Append a tibble of predictions returned by 'predict_boots()'
###   with upper and lower bounds.
### Aliases: summarise_predictions `summarize_predictions()`
###   summarize_predictions

### ** Examples

## Not run: 
##D library(tidymodels)
##D 
##D # setup a workflow without fitting
##D wf <-
##D   workflow() %>%
##D   add_recipe(recipe(qsec ~ wt, data = mtcars)) %>%
##D   add_model(linear_reg())
##D 
##D # fit and predict 2000 bootstrap resampled models to mtcars
##D set.seed(123)
##D preds <-
##D   wf %>%
##D   predict_boots(n = 2000, training_data = mtcars, new_data = mtcars)
##D 
##D # append with prediction interval summary columns
##D preds %>%
##D   summarise_predictions(conf = 0.95)
## End(Not run)



cleanEx()
nameEx("vi_boots")
### * vi_boots

flush(stderr()); flush(stdout())

### Name: vi_boots
### Title: Fit and estimate variable importance from a workflow using many
###   bootstrap resamples.
### Aliases: vi_boots

### ** Examples

## Not run: 
##D library(tidymodels)
##D 
##D # setup a workflow without fitting
##D wf <-
##D   workflow() %>%
##D   add_recipe(recipe(qsec ~ wt, data = mtcars)) %>%
##D   add_model(linear_reg())
##D 
##D # fit and estimate variable importance from 125 bootstrap resampled models
##D set.seed(123)
##D wf %>%
##D   vi_boots(n = 2000, training_data = mtcars)
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
