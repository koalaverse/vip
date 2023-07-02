## ----ex_setup, include=FALSE--------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  digits = 3,
  collapse = TRUE,
  comment = "#>",
  eval = requireNamespace("modeldata", quietly = TRUE)
  )
options(digits = 3)
library(recipes)

## ----formula-roles------------------------------------------------------------
library(recipes)

recipe(Species ~ ., data = iris) %>% summary()

recipe( ~ Species, data = iris) %>% summary()

recipe(Sepal.Length + Sepal.Width ~ ., data = iris) %>% summary()

## ----formula-update-----------------------------------------------------------
library(modeldata)
data(biomass)

recipe(HHV ~ ., data = biomass) %>% 
  update_role(dataset, new_role = "dataset split variable") %>% 
  update_role(sample, new_role = "sample ID") %>% 
  summary()

## ----formula-rm---------------------------------------------------------------
recipe(HHV ~ ., data = biomass) %>% 
  remove_role(sample, old_role = "predictor") %>% 
  summary()

## ----formula-rm-fail, error=TRUE----------------------------------------------
recipe(HHV ~ ., data = biomass) %>% 
  update_role(sample, new_role = NA_character_)

## ----formula-add--------------------------------------------------------------
multi_role <- recipe(HHV ~ ., data = biomass) %>% 
  update_role(dataset, new_role = "dataset split variable") %>% 
  update_role(sample, new_role = "sample ID") %>% 
  # Roles below from https://wordcounter.net/random-word-generator
  add_role(sample, new_role = "jellyfish") 

multi_role %>% 
  summary()

## -----------------------------------------------------------------------------
multi_role %>%
  update_role(sample, new_role = "flounder", old_role = "jellyfish") %>%
  summary()

## -----------------------------------------------------------------------------
multi_role %>% 
  add_role(HHV, new_role = "nocenter") %>% 
  step_center(all_predictors(), -has_role("nocenter")) %>% 
  prep(training = biomass, retain = TRUE) %>% 
  bake(new_data = NULL) %>% 
  head()

## ----x-none-------------------------------------------------------------------
recipe(biomass) %>% 
  summary()

## ----x-none-updated-----------------------------------------------------------
recipe(biomass) %>% 
  update_role(contains("gen"), new_role = "lunchroom") %>% 
  update_role(sample, HHV, new_role = "snail") %>% 
  summary()

## ----dummy--------------------------------------------------------------------
recipe( ~ ., data = iris) %>% 
  step_dummy(Species) %>% 
  prep() %>% 
  bake(new_data = NULL, all_predictors()) %>% 
  dplyr::select(starts_with("Species")) %>% 
  names()

# or something else
recipe( ~ ., data = iris) %>% 
  step_dummy(Species, role = "trousers") %>% 
  prep() %>% 
  bake(new_data = NULL, has_role("trousers")) %>% 
  names()

