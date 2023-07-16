pkgname <- "prettyglm"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('prettyglm')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("actual_expected_bucketed")
### * actual_expected_bucketed

flush(stderr()); flush(stdout())

### Name: actual_expected_bucketed
### Title: actual_expected_bucketed
### Aliases: actual_expected_bucketed

### ** Examples


library(dplyr)
library(prettyglm)

data('titanic')

columns_to_factor <- c('Pclass',
                       'Sex',
                       'Cabin',
                       'Embarked',
                       'Cabintype',
                       'Survived')
meanage <- base::mean(titanic$Age, na.rm=TRUE)

titanic  <- titanic  %>%
  dplyr::mutate_at(columns_to_factor, list(~factor(.))) %>%
  dplyr::mutate(Age =base::ifelse(is.na(Age)==TRUE,meanage,Age)) %>%
  dplyr::mutate(Age_0_25 = prettyglm::splineit(Age,0,25),
                Age_25_50 = prettyglm::splineit(Age,25,50),
                Age_50_120 = prettyglm::splineit(Age,50,120)) %>%
  dplyr::mutate(Fare_0_250 = prettyglm::splineit(Fare,0,250),
                Fare_250_600 = prettyglm::splineit(Fare,250,600))

survival_model <- stats::glm(Survived ~
                               Sex:Age +
                               Fare +
                               Embarked +
                               SibSp +
                               Parch +
                               Cabintype,
                             data = titanic,
                             family = binomial(link = 'logit'))

prettyglm::actual_expected_bucketed(target_variable = 'Survived',
                                    model_object = survival_model,
                                    data_set = titanic)




cleanEx()
nameEx("bank_data")
### * bank_data

flush(stderr()); flush(stdout())

### Name: bank_data
### Title: Bank marketing campaigns data set analysis
### Aliases: bank_data
### Keywords: datasets

### ** Examples


data(bank)
head(bank_data)



cleanEx()
nameEx("one_way_ave")
### * one_way_ave

flush(stderr()); flush(stdout())

### Name: one_way_ave
### Title: one_way_ave
### Aliases: one_way_ave

### ** Examples

library(dplyr)
library(prettyglm)
data('titanic')
columns_to_factor <- c('Pclass',
                       'Sex',
                       'Cabin',
                       'Embarked',
                       'Cabintype',
                       'Survived')
meanage <- base::mean(titanic$Age, na.rm=TRUE)

titanic  <- titanic  %>%
  dplyr::mutate_at(columns_to_factor, list(~factor(.))) %>%
  dplyr::mutate(Age =base::ifelse(is.na(Age)==TRUE,meanage,Age)) %>%
  dplyr::mutate(Age_0_25 = prettyglm::splineit(Age,0,25),
                Age_25_50 = prettyglm::splineit(Age,25,50),
                Age_50_120 = prettyglm::splineit(Age,50,120)) %>%
  dplyr::mutate(Fare_0_250 = prettyglm::splineit(Fare,0,250),
                Fare_250_600 = prettyglm::splineit(Fare,250,600))

survival_model <- stats::glm(Survived ~
                               Sex:Age +
                               Fare +
                               Embarked +
                               SibSp +
                               Parch +
                               Cabintype,
                             data = titanic,
                             family = binomial(link = 'logit'))

# Continuous Variable Example
one_way_ave(feature_to_plot = 'Age',
            model_object = survival_model,
            target_variable = 'Survived',
            data_set = titanic,
            number_of_buckets = 20,
            upper_percentile_to_cut = 0.1,
            lower_percentile_to_cut = 0.1)

# Discrete Variable Example
one_way_ave(feature_to_plot = 'Pclass',
            model_object = survival_model,
            target_variable = 'Survived',
            data_set = titanic)

# Custom Predict Function and facet
a_custom_predict_function <- function(target, model_object, dataset){
  dataset <- base::as.data.frame(dataset)
  Actual_Values <- dplyr::pull(dplyr::select(dataset, tidyselect::all_of(c(target))))
  if(class(Actual_Values) == 'factor'){
    Actual_Values <- base::as.numeric(as.character(Actual_Values))
  }
  Predicted_Values <- base::as.numeric(stats::predict(model_object, dataset, type='response'))

  to_return <-  base::data.frame(Actual_Values = Actual_Values,
                                 Predicted_Values = Predicted_Values)

  to_return <- to_return %>%
    dplyr::mutate(Predicted_Values = base::ifelse(Predicted_Values > 0.3,0.3,Predicted_Values))
  return(to_return)
}

one_way_ave(feature_to_plot = 'Age',
            model_object = survival_model,
            target_variable = 'Survived',
            data_set = titanic,
            number_of_buckets = 20,
            upper_percentile_to_cut = 0.1,
            lower_percentile_to_cut = 0.1,
            predict_function = a_custom_predict_function,
            facetby = 'Pclass')





cleanEx()
nameEx("pretty_coefficients")
### * pretty_coefficients

flush(stderr()); flush(stdout())

### Name: pretty_coefficients
### Title: pretty_coefficients
### Aliases: pretty_coefficients

### ** Examples


library(dplyr)
library(prettyglm)
data('titanic')
columns_to_factor <- c('Pclass',
                       'Sex',
                       'Cabin',
                       'Embarked',
                       'Cabintype',
                       'Survived')
meanage <- base::mean(titanic$Age, na.rm=TRUE)

titanic  <- titanic  %>%
 dplyr::mutate_at(columns_to_factor, list(~factor(.))) %>%
 dplyr::mutate(Age =base::ifelse(is.na(Age)==TRUE,meanage,Age)) %>%
 dplyr::mutate(Age_0_25 = prettyglm::splineit(Age,0,25),
               Age_25_50 = prettyglm::splineit(Age,25,50),
               Age_50_120 = prettyglm::splineit(Age,50,120)) %>%
 dplyr::mutate(Fare_0_250 = prettyglm::splineit(Fare,0,250),
               Fare_250_600 = prettyglm::splineit(Fare,250,600))

# A simple example
survival_model <- stats::glm(Survived ~
                              Pclass +
                              Sex +
                              Age +
                              Fare +
                              Embarked +
                              SibSp +
                              Parch +
                              Cabintype,
                             data = titanic,
                             family = binomial(link = 'logit'))
pretty_coefficients(survival_model)

# A more complicated example with a spline and different importance method
survival_model3 <- stats::glm(Survived ~
                                        Pclass +
                                        Age_0_25 +
                                        Age_25_50 +
                                        Age_50_120 +
                                        Sex:Fare_0_250 +
                                        Sex:Fare_250_600 +
                                        Embarked +
                                        SibSp +
                                        Parch +
                                        Cabintype,
                              data = titanic,
                              family = binomial(link = 'logit'))
pretty_coefficients(survival_model3,
                    relativity_transform = 'exp(estimate)-1',
                    spline_seperator = '_',
                    vimethod = 'permute',
                    target = 'Survived',
                    metric = 'auc',
                    pred_wrapper = predict.glm,
                    reference_class = 0)





cleanEx()
nameEx("pretty_relativities")
### * pretty_relativities

flush(stderr()); flush(stdout())

### Name: pretty_relativities
### Title: pretty_relativities
### Aliases: pretty_relativities

### ** Examples

library(dplyr)
library(prettyglm)
data('titanic')

columns_to_factor <- c('Pclass',
                       'Sex',
                       'Cabin',
                       'Embarked',
                       'Cabintype',
                       'Survived')
meanage <- base::mean(titanic$Age, na.rm=TRUE)

titanic  <- titanic  %>%
  dplyr::mutate_at(columns_to_factor, list(~factor(.))) %>%
  dplyr::mutate(Age =base::ifelse(is.na(Age)==TRUE,meanage,Age)) %>%
  dplyr::mutate(Age_0_25 = prettyglm::splineit(Age,0,25),
                Age_25_50 = prettyglm::splineit(Age,25,50),
                Age_50_120 = prettyglm::splineit(Age,50,120)) %>%
  dplyr::mutate(Fare_0_250 = prettyglm::splineit(Fare,0,250),
                Fare_250_600 = prettyglm::splineit(Fare,250,600))

survival_model3 <- stats::glm(Survived ~
                                Pclass:Embarked +
                                Age_0_25  +
                                Age_25_50 +
                                Age_50_120  +
                                Sex:Fare_0_250 +
                                Sex:Fare_250_600 +
                                SibSp +
                                Parch +
                                Cabintype,
                              data = titanic,
                              family = binomial(link = 'logit'))

# categorical factor
pretty_relativities(feature_to_plot = 'Cabintype',
                    model_object = survival_model3)

# continuous factor
pretty_relativities(feature_to_plot = 'Parch',
                    model_object = survival_model3)

# splined continuous factor
pretty_relativities(feature_to_plot = 'Age',
                    model_object = survival_model3,
                    spline_seperator = '_',
                    upper_percentile_to_cut = 0.01,
                    lower_percentile_to_cut = 0.01)

# factor factor interaction
pretty_relativities(feature_to_plot = 'Pclass:Embarked',
                    model_object = survival_model3,
                    iteractionplottype = 'colour',
                    facetorcolourby = 'Pclass')

# Continuous spline and categorical by colour
pretty_relativities(feature_to_plot = 'Sex:Fare',
                    model_object = survival_model3,
                    spline_seperator = '_')

# Continuous spline and categorical by facet
pretty_relativities(feature_to_plot = 'Sex:Fare',
                    model_object = survival_model3,
                    spline_seperator = '_',
                    iteractionplottype = 'facet')



cleanEx()
nameEx("splineit")
### * splineit

flush(stderr()); flush(stdout())

### Name: splineit
### Title: splineit
### Aliases: splineit

### ** Examples

library(dplyr)
library(prettyglm)
data('titanic')

columns_to_factor <- c('Pclass',
                      'Sex',
                      'Cabin',
                      'Embarked',
                      'Cabintype',
                      'Survived')
meanage <- base::mean(titanic$Age, na.rm=TRUE)

titanic  <- titanic  %>%
 dplyr::mutate_at(columns_to_factor, list(~factor(.))) %>%
 dplyr::mutate(Age =base::ifelse(is.na(Age)==TRUE,meanage,Age)) %>%
 dplyr::mutate(Age_0_25 = prettyglm::splineit(Age,0,25),
               Age_25_50 = prettyglm::splineit(Age,25,50),
               Age_50_120 = prettyglm::splineit(Age,50,120)) %>%
 dplyr::mutate(Fare_0_250 = prettyglm::splineit(Fare,0,250),
               Fare_250_600 = prettyglm::splineit(Fare,250,600))




cleanEx()
nameEx("titanic")
### * titanic

flush(stderr()); flush(stdout())

### Name: titanic
### Title: Titanic Data
### Aliases: titanic
### Keywords: datasets

### ** Examples


data(titanic)
head(titanic)



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
