## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  digits = 3,
  collapse = TRUE,
  comment = "#>"
  )
options(digits = 3)
library(recipes)

## ----iris-base-rec------------------------------------------------------------
library(recipes)

# make a copy for use below
iris <- iris %>% mutate(original = Species)

iris_rec <- recipe( ~ ., data = iris)
summary(iris_rec)

## ----iris-ref-cell------------------------------------------------------------
ref_cell <- 
  iris_rec %>% 
  step_dummy(Species) %>%
  prep(training = iris)
summary(ref_cell)

# Get a row for each factor level
bake(ref_cell, new_data = NULL, original, starts_with("Species")) %>% distinct()

## ----defaults-----------------------------------------------------------------
param <- getOption("contrasts")
param

## ----iris-helmert-------------------------------------------------------------
# change it:
go_helmert <- param
go_helmert["unordered"] <- "contr.helmert"
options(contrasts = go_helmert)

# now make dummy variables with new parameterization
helmert <- 
  iris_rec %>% 
  step_dummy(Species) %>%
  prep(training = iris)
summary(helmert)

bake(helmert, new_data = NULL, original, starts_with("Species")) %>% distinct()

# Yuk; go back to the original method
options(contrasts = param)

## ----iris-2int----------------------------------------------------------------
iris_int <- 
  iris_rec %>%
  step_interact( ~ Sepal.Width:Sepal.Length) %>%
  prep(training = iris)
summary(iris_int)

## ----mm-int-------------------------------------------------------------------
model.matrix(~ Species*Sepal.Length, data = iris) %>% 
  as.data.frame() %>% 
  # show a few specific rows
  slice(c(1, 51, 101)) %>% 
  as.data.frame()

## ----nope, eval = FALSE-------------------------------------------------------
#  # Must I do this?
#  iris_rec %>%
#    step_interact( ~ Species_versicolor:Sepal.Length +
#                     Species_virginica:Sepal.Length)

## ----iris-sel-----------------------------------------------------------------
iris_int <- 
  iris_rec %>% 
  step_dummy(Species) %>%
  step_interact( ~ starts_with("Species"):Sepal.Length) %>%
  prep(training = iris)
summary(iris_int)

## ----sel-input, eval = FALSE--------------------------------------------------
#  starts_with("Species")

## ----sel-output, eval = FALSE-------------------------------------------------
#  (Species_versicolor + Species_virginica)

## ----int-form-----------------------------------------------------------------
iris_int

## ----iris-dont----------------------------------------------------------------
iris_int <- 
  iris_rec %>% 
  step_interact( ~ Species:Sepal.Length) %>%
  prep(training = iris)
summary(iris_int)

## ----one-hot------------------------------------------------------------------
iris_rec %>% 
  step_dummy(Species, one_hot = TRUE) %>%
  prep(training = iris) %>%
  bake(original, new_data = NULL, starts_with("Species")) %>%
  distinct()

## ----one-hot-two--------------------------------------------------------------
hot_reference <- 
  iris_rec %>% 
  step_dummy(Species, one_hot = TRUE) %>%
  prep(training = iris) %>%
  bake(original, new_data = NULL, starts_with("Species")) %>%
  distinct()

hot_reference

# from above
options(contrasts = go_helmert)

hot_helmert <- 
  iris_rec %>% 
  step_dummy(Species, one_hot = TRUE) %>%
  prep(training = iris) %>%
  bake(original, new_data = NULL, starts_with("Species")) %>%
  distinct()

hot_helmert

