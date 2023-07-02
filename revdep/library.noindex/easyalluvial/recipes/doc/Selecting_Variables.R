## ----ex_setup, include=FALSE--------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  digits = 3,
  collapse = TRUE,
  comment = "#>",
  eval = requireNamespace("modeldata", quietly = TRUE)
  )
options(digits = 3)

## ----credit-------------------------------------------------------------------
library(recipes)
library(modeldata)

data("credit_data")
str(credit_data)

rec <- recipe(Status ~ Seniority + Time + Age + Records, data = credit_data)
rec

## ----var_info_orig------------------------------------------------------------
summary(rec, original = TRUE)

## ----dummy_1------------------------------------------------------------------
dummied <- rec %>% step_dummy(all_nominal())

## ----dummy_2------------------------------------------------------------------
dummied <- rec %>% step_dummy(Records) # or
dummied <- rec %>% step_dummy(all_nominal(), - Status) # or
dummied <- rec %>% step_dummy(all_nominal_predictors()) 

## ----dummy_3------------------------------------------------------------------
dummied <- prep(dummied, training = credit_data)
with_dummy <- bake(dummied, new_data = credit_data)
with_dummy

