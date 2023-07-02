pkgname <- "easyalluvial"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('easyalluvial')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_imp_plot")
### * add_imp_plot

flush(stderr()); flush(stdout())

### Name: add_imp_plot
### Title: add bar plot of important features to model response alluvial
###   plot
### Aliases: add_imp_plot

### ** Examples

## Not run: 
##D df = mtcars2[, ! names(mtcars2) %in% 'ids' ]
##D 
##D train = caret::train( disp ~ .
##D                      , df
##D                      , method = 'rf'
##D                      , trControl = caret::trainControl( method = 'none' )
##D                      , importance = TRUE )
##D 
##D pred_train = caret::predict.train(train, df)
##D 
##D p = alluvial_model_response_caret(train, degree = 4, pred_train = pred_train)
##D 
##D p_grid = add_marginal_histograms(p, data_input = df)
##D 
##D p_grid = add_imp_plot(p_grid, p, data_input = df)
## End(Not run)



cleanEx()
nameEx("add_marginal_histograms")
### * add_marginal_histograms

flush(stderr()); flush(stdout())

### Name: add_marginal_histograms
### Title: add marginal histograms to alluvial plot
### Aliases: add_marginal_histograms

### ** Examples

## Not run: 
##D p = alluvial_wide(mtcars2, max_variables = 3)
##D p_grid = add_marginal_histograms(p, mtcars2)
## End(Not run)



cleanEx()
nameEx("alluvial_long")
### * alluvial_long

flush(stderr()); flush(stdout())

### Name: alluvial_long
### Title: alluvial plot of data in long format
### Aliases: alluvial_long

### ** Examples


## Not run: 
##D  data = quarterly_flights
##D 
##D  alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'last_variable' )
##D 
##D  # more flow coloring variants ------------------------------------
##D 
##D  alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'first_variable' )
##D  alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'all_flows' )
##D  alluvial_long( data, key = qu, value = mean_arr_delay, id = tailnum, fill_by = 'value' )
##D 
##D  # color by additional variable carrier ---------------------------
##D 
##D  alluvial_long( data, key = qu, value = mean_arr_delay, fill = carrier, id = tailnum )
##D 
##D  # use same color coding for flows and y levels -------------------
##D 
##D  palette = c('green3', 'tomato')
##D 
##D  alluvial_long( data, qu, mean_arr_delay, tailnum, fill_by = 'value'
##D                 , col_vector_flow = palette
##D                 , col_vector_value = palette )
##D 
##D 
##D  # reorder levels ------------------------------------------------
##D 
##D  alluvial_long( data, qu, mean_arr_delay, tailnum, fill_by = 'first_variable'
##D                , order_levels_value = c('on_time', 'late') )
##D 
##D  alluvial_long( data, qu, mean_arr_delay, tailnum, fill_by = 'first_variable'
##D                , order_levels_key = c('Q4', 'Q3', 'Q2', 'Q1') )
##D 
##D require(dplyr)
##D require(magrittr)
##D 
##D  order_by_carrier_size = data %>%
##D    group_by(carrier) %>%
##D    count() %>%
##D    arrange( desc(n) ) %>%
##D    .[['carrier']]
##D 
##D  alluvial_long( data, qu, mean_arr_delay, tailnum, carrier
##D                 , order_levels_fill = order_by_carrier_size )
##D 
## End(Not run)



cleanEx()
nameEx("alluvial_model_response")
### * alluvial_model_response

flush(stderr()); flush(stdout())

### Name: alluvial_model_response
### Title: create model response plot
### Aliases: alluvial_model_response

### ** Examples

df = mtcars2[, ! names(mtcars2) %in% 'ids' ]
m = randomForest::randomForest( disp ~ ., df)
imp = m$importance
dspace = get_data_space(df, imp, degree = 3)
pred = predict(m, newdata = dspace)
alluvial_model_response(pred, dspace, imp, degree = 3)

# partial dependency plotting method
## Not run: 
##D  pred = get_pdp_predictions(df, imp
##D                             , .f_predict = randomForest:::predict.randomForest
##D                             , m
##D                             , degree = 3
##D                             , bins = 5)
##D 
##D 
##D  alluvial_model_response(pred, dspace, imp, degree = 3, method = 'pdp')
##D  
## End(Not run)



cleanEx()
nameEx("alluvial_model_response_caret")
### * alluvial_model_response_caret

flush(stderr()); flush(stdout())

### Name: alluvial_model_response_caret
### Title: create model response plot for caret models
### Aliases: alluvial_model_response_caret

### ** Examples


if(check_pkg_installed("caret", raise_error = FALSE)) {
  df = mtcars2[, ! names(mtcars2) %in% 'ids' ]

  train = caret::train( disp ~ .,
                        df,
                        method = 'rf',
                        trControl = caret::trainControl( method = 'none' ),
                        importance = TRUE )

  alluvial_model_response_caret(train, df, degree = 3)
}
# partial dependency plotting method
## Not run: 
##D future::plan("multisession")
##D alluvial_model_response_caret(train, df, degree = 3, method = 'pdp', parallel = TRUE)
##D  
## End(Not run)



cleanEx()
nameEx("alluvial_model_response_parsnip")
### * alluvial_model_response_parsnip

flush(stderr()); flush(stdout())

### Name: alluvial_model_response_parsnip
### Title: create model response plot for parsnip models
### Aliases: alluvial_model_response_parsnip

### ** Examples


if(check_pkg_installed("parsnip", raise_error = FALSE)) {
  df = mtcars2[, ! names(mtcars2) %in% 'ids' ]

  m = parsnip::rand_forest(mode = "regression") %>%
     parsnip::set_engine("randomForest") %>%
     parsnip::fit(disp ~ ., data = df)

  alluvial_model_response_parsnip(m, df, degree = 3)
}
## Not run: 
##D # workflow --------------------------------- 
##D m <- parsnip::rand_forest(mode = "regression") %>%
##D   parsnip::set_engine("randomForest")
##D 
##D rec_prep = recipes::recipe(disp ~ ., df) %>%
##D   recipes::prep()
##D 
##D wf <- workflows::workflow() %>%
##D   workflows::add_model(m) %>%
##D   workflows::add_recipe(rec_prep) %>%
##D   parsnip::fit(df)
##D 
##D alluvial_model_response_parsnip(wf, df, degree = 3)
##D 
##D # partial dependence plotting method -----
##D future::plan("multisession")
##D alluvial_model_response_parsnip(m, df, degree = 3, method = 'pdp', parallel = TRUE)
## End(Not run)



cleanEx()
nameEx("alluvial_wide")
### * alluvial_wide

flush(stderr()); flush(stdout())

### Name: alluvial_wide
### Title: alluvial plot of data in wide format
### Aliases: alluvial_wide

### ** Examples

## Not run: 
##D alluvial_wide( data = mtcars2, id = ids
##D                 , max_variables = 3
##D                 , fill_by = 'first_variable' )#'
##D # more coloring variants----------------------
##D alluvial_wide( data = mtcars2, id = ids
##D                 , max_variables = 5
##D                 , fill_by = 'last_variable' )
##D 
##D alluvial_wide( data = mtcars2, id = ids
##D                 , max_variables = 5
##D                 , fill_by = 'all_flows' )
##D 
##D alluvial_wide( data = mtcars2, id = ids
##D                 , max_variables = 5
##D                 , fill_by = 'first_variable' )
##D 
##D # manually order variable values and colour by stratum value
##D 
##D alluvial_wide( data = mtcars2, id = ids
##D                  , max_variables = 5
##D                  , fill_by = 'values'
##D                  , order_levels = c('4', '8', '6') )
## End(Not run)



cleanEx()
nameEx("check_pkg_installed")
### * check_pkg_installed

flush(stderr()); flush(stdout())

### Name: check_pkg_installed
### Title: check if package is installed
### Aliases: check_pkg_installed

### ** Examples

check_pkg_installed("easyalluvial")



cleanEx()
nameEx("get_data_space")
### * get_data_space

flush(stderr()); flush(stdout())

### Name: get_data_space
### Title: calculate data space
### Aliases: get_data_space

### ** Examples

df = mtcars2[, ! names(mtcars2) %in% 'ids' ]
m = randomForest::randomForest( disp ~ ., df)
imp = m$importance
dspace = get_data_space(df, imp)



cleanEx()
nameEx("get_pdp_predictions")
### * get_pdp_predictions

flush(stderr()); flush(stdout())

### Name: get_pdp_predictions
### Title: get predictions compatible with the partial dependence plotting
###   method
### Aliases: get_pdp_predictions

### ** Examples

 df = mtcars2[, ! names(mtcars2) %in% 'ids' ]
 m = randomForest::randomForest( disp ~ ., df)
 imp = m$importance

 pred = get_pdp_predictions(df, imp
                            , m
                            , degree = 3
                            , bins = 5)

# parallel processing --------------------------
## Not run: 
##D  future::plan("multisession")
##D  
##D  # note that we have to pass the predict method via .f_predict otherwise
##D  # it will not be available in the worker's environment.
##D  
##D  pred = get_pdp_predictions(df, imp
##D                             , m
##D                             , degree = 3
##D                             , bins = 5,
##D                             , parallel = TRUE
##D                             , .f_predict = randomForest:::predict.randomForest)
## End(Not run)



cleanEx()
nameEx("manip_bin_numerics")
### * manip_bin_numerics

flush(stderr()); flush(stdout())

### Name: manip_bin_numerics
### Title: bin numerical columns
### Aliases: manip_bin_numerics

### ** Examples

summary( mtcars2 )
summary( manip_bin_numerics(mtcars2) )
summary( manip_bin_numerics(mtcars2, bin_labels = 'mean'))
summary( manip_bin_numerics(mtcars2, bin_labels = 'cuts'
  , scale = FALSE, center = FALSE, transform = FALSE))



cleanEx()
nameEx("manip_factor_2_numeric")
### * manip_factor_2_numeric

flush(stderr()); flush(stdout())

### Name: manip_factor_2_numeric
### Title: converts factor to numeric preserving numeric levels and order
###   in character levels.
### Aliases: manip_factor_2_numeric

### ** Examples

fac_num = factor( c(1,3,8) )
fac_chr = factor( c('foo','bar') )
fac_chr_ordered = factor( c('a','b','c'), ordered = TRUE )

manip_factor_2_numeric( fac_num )
manip_factor_2_numeric( fac_chr )
manip_factor_2_numeric( fac_chr_ordered )
# does not work for decimal numbers
manip_factor_2_numeric(factor(c("A12", "B55", "10e4")))
manip_factor_2_numeric(factor(c("1.56", "4.56", "8.4")))



cleanEx()
nameEx("palette_filter")
### * palette_filter

flush(stderr()); flush(stdout())

### Name: palette_filter
### Title: color filters for any vector of hex color values
### Aliases: palette_filter

### ** Examples


require(magrittr)

palette_qualitative() %>%
  palette_filter(thresh_similar = 0) %>%
  palette_plot_intensity()

## Not run: 
##D # more examples---------------------------
##D 
##D palette_qualitative() %>%
##D   palette_filter(thresh_similar = 25) %>%
##D   palette_plot_intensity()
##D 
##D palette_qualitative() %>%
##D   palette_filter(thresh_similar = 0, blues = FALSE) %>%
##D   palette_plot_intensity()
## End(Not run)



cleanEx()
nameEx("palette_increase_length")
### * palette_increase_length

flush(stderr()); flush(stdout())

### Name: palette_increase_length
### Title: increases length of palette by repeating colours
### Aliases: palette_increase_length

### ** Examples


require(magrittr)

length(palette_qualitative())

palette_qualitative() %>%
  palette_increase_length(100) %>%
  length()



cleanEx()
nameEx("palette_plot_intensity")
### * palette_plot_intensity

flush(stderr()); flush(stdout())

### Name: palette_plot_intensity
### Title: plot colour intensity of palette
### Aliases: palette_plot_intensity

### ** Examples

## Not run: 
##D if(interactive()){
##D palette_qualitative() %>%
##D   palette_filter( thresh = 25) %>%
##D   palette_plot_intensity()
##D  }
## End(Not run)



cleanEx()
nameEx("palette_plot_rgp")
### * palette_plot_rgp

flush(stderr()); flush(stdout())

### Name: palette_plot_rgp
### Title: plot rgb values of palette
### Aliases: palette_plot_rgp

### ** Examples

## Not run: 
##D if(interactive()){
##D palette_qualitative() %>%
##D   palette_filter( thresh = 50) %>%
##D   palette_plot_rgp()
##D  }
## End(Not run)



cleanEx()
nameEx("palette_qualitative")
### * palette_qualitative

flush(stderr()); flush(stdout())

### Name: palette_qualitative
### Title: compose palette from qualitative RColorBrewer palettes
### Aliases: palette_qualitative

### ** Examples

palette_qualitative()



cleanEx()
nameEx("plot_all_hists")
### * plot_all_hists

flush(stderr()); flush(stdout())

### Name: plot_all_hists
### Title: plot marginal histograms of alluvial plot
### Aliases: plot_all_hists

### ** Examples

## Not run: 
##D p = alluvial_wide(mtcars2, max_variables = 3)
##D plot_all_hists(p, mtcars2)
## End(Not run)



cleanEx()
nameEx("plot_condensation")
### * plot_condensation

flush(stderr()); flush(stdout())

### Name: plot_condensation
### Title: Plot dataframe condensation potential
### Aliases: plot_condensation

### ** Examples


 plot_condensation(mtcars2)

 plot_condensation(mtcars2, first = 'disp')




cleanEx()
nameEx("plot_imp")
### * plot_imp

flush(stderr()); flush(stdout())

### Name: plot_imp
### Title: plot feature importance
### Aliases: plot_imp

### ** Examples

## Not run: 
##D df = mtcars2[, ! names(mtcars2) %in% 'ids' ]
##D 
##D train = caret::train( disp ~ .
##D                      , df
##D                      , method = 'rf'
##D                      , trControl = caret::trainControl( method = 'none' )
##D                      , importance = TRUE )
##D 
##D pred_train = caret::predict.train(train, df)
##D 
##D p = alluvial_model_response_caret(train, degree = 3, pred_train = pred_train)
##D 
##D plot_imp(p, mtcars2)
##D 
## End(Not run)



cleanEx()
nameEx("tidy_imp")
### * tidy_imp

flush(stderr()); flush(stdout())

### Name: tidy_imp
### Title: tidy up dataframe containing model feature importance
### Aliases: tidy_imp

### ** Examples

# randomforest
df = mtcars2[, ! names(mtcars2) %in% 'ids' ]
m = randomForest::randomForest( disp ~ ., df)
imp = m$importance
tidy_imp(imp, df)




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
