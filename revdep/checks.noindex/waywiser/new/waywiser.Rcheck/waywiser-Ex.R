pkgname <- "waywiser"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('waywiser')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("global_geary_c")
### * global_geary_c

flush(stderr()); flush(stdout())

### Name: ww_global_geary_c
### Title: Global Geary's C statistic
### Aliases: ww_global_geary_c ww_global_geary_c_vec ww_global_geary_pvalue
###   ww_global_geary_pvalue_vec

### ** Examples

guerry_model <- guerry
guerry_lm <- lm(Crm_prs ~ Litercy, guerry_model)
guerry_model$predictions <- predict(guerry_lm, guerry_model)

ww_global_geary_c(guerry_model, Crm_prs, predictions)
ww_global_geary_pvalue(guerry_model, Crm_prs, predictions)

wt <- ww_build_weights(guerry_model)

ww_global_geary_c_vec(
  guerry_model$Crm_prs,
  guerry_model$predictions,
  wt = wt
)
ww_global_geary_pvalue_vec(
  guerry_model$Crm_prs,
  guerry_model$predictions,
  wt = wt
)




cleanEx()
nameEx("global_moran_i")
### * global_moran_i

flush(stderr()); flush(stdout())

### Name: ww_global_moran_i
### Title: Global Moran's I statistic
### Aliases: ww_global_moran_i ww_global_moran_i_vec ww_global_moran_pvalue
###   ww_global_moran_pvalue_vec

### ** Examples

guerry_model <- guerry
guerry_lm <- lm(Crm_prs ~ Litercy, guerry_model)
guerry_model$predictions <- predict(guerry_lm, guerry_model)

ww_global_moran_i(guerry_model, Crm_prs, predictions)
ww_global_moran_pvalue(guerry_model, Crm_prs, predictions)

wt <- ww_build_weights(guerry_model)

ww_global_moran_i_vec(
  guerry_model$Crm_prs,
  guerry_model$predictions,
  wt = wt
)
ww_global_moran_pvalue_vec(
  guerry_model$Crm_prs,
  guerry_model$predictions,
  wt = wt
)




cleanEx()
nameEx("guerry")
### * guerry

flush(stderr()); flush(stdout())

### Name: guerry
### Title: Guerry "Moral Statistics" (1830s)
### Aliases: guerry
### Keywords: datasets

### ** Examples

if (requireNamespace("sf", quietly = TRUE)) {
  library(sf)
  data(guerry)

  plot(guerry["Donatns"])
}



cleanEx()
nameEx("local_geary_c")
### * local_geary_c

flush(stderr()); flush(stdout())

### Name: ww_local_geary_c
### Title: Local Geary's C statistic
### Aliases: ww_local_geary_c ww_local_geary_c_vec ww_local_geary_pvalue
###   ww_local_geary_pvalue_vec

### ** Examples

guerry_model <- guerry
guerry_lm <- lm(Crm_prs ~ Litercy, guerry_model)
guerry_model$predictions <- predict(guerry_lm, guerry_model)

ww_local_geary_c(guerry_model, Crm_prs, predictions)
ww_local_geary_pvalue(guerry_model, Crm_prs, predictions)

wt <- ww_build_weights(guerry_model)

ww_local_geary_c_vec(
  guerry_model$Crm_prs,
  guerry_model$predictions,
  wt = wt
)
ww_local_geary_pvalue_vec(
  guerry_model$Crm_prs,
  guerry_model$predictions,
  wt = wt
)




cleanEx()
nameEx("local_getis_ord_g")
### * local_getis_ord_g

flush(stderr()); flush(stdout())

### Name: ww_local_getis_ord_g
### Title: Local Getis-Ord G and G* statistic
### Aliases: ww_local_getis_ord_g ww_local_getis_ord_g_vec
###   ww_local_getis_ord_g_pvalue ww_local_getis_ord_g_pvalue_vec

### ** Examples

guerry_model <- guerry
guerry_lm <- lm(Crm_prs ~ Litercy, guerry_model)
guerry_model$predictions <- predict(guerry_lm, guerry_model)

ww_local_getis_ord_g(guerry_model, Crm_prs, predictions)
ww_local_getis_ord_g_pvalue(guerry_model, Crm_prs, predictions)

wt <- ww_build_weights(guerry_model)

ww_local_getis_ord_g_vec(
  guerry_model$Crm_prs,
  guerry_model$predictions,
  wt = wt
)
ww_local_getis_ord_g_pvalue_vec(
  guerry_model$Crm_prs,
  guerry_model$predictions,
  wt = wt
)




cleanEx()
nameEx("local_moran_i")
### * local_moran_i

flush(stderr()); flush(stdout())

### Name: ww_local_moran_i
### Title: Local Moran's I statistic
### Aliases: ww_local_moran_i ww_local_moran_i_vec ww_local_moran_pvalue
###   ww_local_moran_pvalue_vec

### ** Examples

guerry_model <- guerry
guerry_lm <- lm(Crm_prs ~ Litercy, guerry_model)
guerry_model$predictions <- predict(guerry_lm, guerry_model)

ww_local_moran_i(guerry_model, Crm_prs, predictions)
ww_local_moran_pvalue(guerry_model, Crm_prs, predictions)

wt <- ww_build_weights(guerry_model)

ww_local_moran_i_vec(
  guerry_model$Crm_prs,
  guerry_model$predictions,
  wt = wt
)
ww_local_moran_pvalue_vec(
  guerry_model$Crm_prs,
  guerry_model$predictions,
  wt = wt
)




cleanEx()
nameEx("predict.ww_area_of_applicability")
### * predict.ww_area_of_applicability

flush(stderr()); flush(stdout())

### Name: predict.ww_area_of_applicability
### Title: Predict from a 'ww_area_of_applicability'
### Aliases: predict.ww_area_of_applicability

### ** Examples

## Don't show: 
if (rlang::is_installed("vip")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(vip)
train <- gen_friedman(1000, seed = 101) # ?vip::gen_friedman
test <- train[701:1000, ]
train <- train[1:700, ]
pp <- stats::ppr(y ~ ., data = train, nterms = 11)
importance <- vi_permute(
  pp,
  target = "y",
  metric = "rsquared",
  pred_wrapper = predict
)

aoa <- ww_area_of_applicability(y ~ ., train, test, importance = importance)
predict(aoa, test)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("print.ww_area_of_applicability")
### * print.ww_area_of_applicability

flush(stderr()); flush(stdout())

### Name: print.ww_area_of_applicability
### Title: Print number of predictors and area-of-applicability threshold
### Aliases: print.ww_area_of_applicability
### Keywords: internal

### ** Examples

## Don't show: 
if (rlang::is_installed("vip")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(vip)
trn <- gen_friedman(500, seed = 101) # ?vip::gen_friedman
pp <- ppr(y ~ ., data = trn, nterms = 11)
importance <- vi_permute(
  pp,
  target = "y",
  metric = "rsquared",
  pred_wrapper = predict
)


ww_area_of_applicability(trn[2:11], importance = importance)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ww_agreement_coefficient")
### * ww_agreement_coefficient

flush(stderr()); flush(stdout())

### Name: ww_agreement_coefficient
### Title: Agreement coefficients and related methods
### Aliases: ww_agreement_coefficient ww_agreement_coefficient.data.frame
###   ww_agreement_coefficient_vec ww_systematic_agreement_coefficient
###   ww_systematic_agreement_coefficient.data.frame
###   ww_systematic_agreement_coefficient_vec
###   ww_unsystematic_agreement_coefficient
###   ww_unsystematic_agreement_coefficient.data.frame
###   ww_unsystematic_agreement_coefficient_vec ww_unsystematic_mpd
###   ww_unsystematic_mpd.data.frame ww_unsystematic_mpd_vec
###   ww_systematic_mpd ww_systematic_mpd.data.frame ww_systematic_mpd_vec
###   ww_unsystematic_rmpd ww_unsystematic_rmpd.data.frame
###   ww_unsystematic_rmpd_vec ww_systematic_rmpd
###   ww_systematic_rmpd.data.frame ww_systematic_rmpd_vec

### ** Examples

# Calculated values match Ji and Gallo 2006:
x <- c(6, 8, 9, 10, 11, 14)
y <- c(2, 3, 5, 5, 6, 8)

ww_agreement_coefficient_vec(x, y)
ww_systematic_agreement_coefficient_vec(x, y)
ww_unsystematic_agreement_coefficient_vec(x, y)
ww_systematic_mpd_vec(x, y)
ww_unsystematic_mpd_vec(x, y)
ww_systematic_rmpd_vec(x, y)
ww_unsystematic_rmpd_vec(x, y)

example_df <- data.frame(x = x, y = y)
ww_agreement_coefficient(example_df, x, y)
ww_systematic_agreement_coefficient(example_df, x, y)
ww_unsystematic_agreement_coefficient(example_df, x, y)
ww_systematic_mpd(example_df, x, y)
ww_unsystematic_mpd(example_df, x, y)
ww_systematic_rmpd(example_df, x, y)
ww_unsystematic_rmpd(example_df, x, y)




cleanEx()
nameEx("ww_area_of_applicability")
### * ww_area_of_applicability

flush(stderr()); flush(stdout())

### Name: ww_area_of_applicability
### Title: Find the area of applicability
### Aliases: ww_area_of_applicability ww_area_of_applicability.data.frame
###   ww_area_of_applicability.matrix ww_area_of_applicability.formula
###   ww_area_of_applicability.recipe ww_area_of_applicability.rset

### ** Examples

## Don't show: 
if (rlang::is_installed("vip")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
train <- vip::gen_friedman(1000, seed = 101) # ?vip::gen_friedman
test <- train[701:1000, ]
train <- train[1:700, ]
pp <- stats::ppr(y ~ ., data = train, nterms = 11)
importance <- vip::vi_permute(
  pp,
  target = "y",
  metric = "rsquared",
  pred_wrapper = predict
)

aoa <- ww_area_of_applicability(y ~ ., train, test, importance = importance)
predict(aoa, test)

# Equivalent methods for calculating AOA:
ww_area_of_applicability(train[2:11], test[2:11], importance)
ww_area_of_applicability(
  as.matrix(train[2:11]),
  as.matrix(test[2:11]),
  importance
)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ww_build_neighbors")
### * ww_build_neighbors

flush(stderr()); flush(stdout())

### Name: ww_build_neighbors
### Title: Make 'nb' objects from sf objects
### Aliases: ww_build_neighbors

### ** Examples

ww_build_neighbors(guerry)




cleanEx()
nameEx("ww_build_weights")
### * ww_build_weights

flush(stderr()); flush(stdout())

### Name: ww_build_weights
### Title: Build "listw" objects of spatial weights
### Aliases: ww_build_weights

### ** Examples

ww_build_weights(guerry)




cleanEx()
nameEx("ww_make_point_neighbors")
### * ww_make_point_neighbors

flush(stderr()); flush(stdout())

### Name: ww_make_point_neighbors
### Title: Make 'nb' objects from point geometries
### Aliases: ww_make_point_neighbors

### ** Examples

ww_make_point_neighbors(ny_trees)




cleanEx()
nameEx("ww_make_polygon_neighbors")
### * ww_make_polygon_neighbors

flush(stderr()); flush(stdout())

### Name: ww_make_polygon_neighbors
### Title: Make 'nb' objects from polygon geometries
### Aliases: ww_make_polygon_neighbors

### ** Examples

ww_make_polygon_neighbors(guerry)




cleanEx()
nameEx("ww_multi_scale")
### * ww_multi_scale

flush(stderr()); flush(stdout())

### Name: ww_multi_scale
### Title: Evaluate metrics at multiple scales of aggregation
### Aliases: ww_multi_scale

### ** Examples

## Don't show: 
if (rlang::is_installed("modeldata")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
data(ames, package = "modeldata")
ames_sf <- sf::st_as_sf(ames, coords = c("Longitude", "Latitude"), crs = 4326)
ames_model <- lm(Sale_Price ~ Lot_Area, data = ames_sf)
ames_sf$predictions <- predict(ames_model, ames_sf)

ww_multi_scale(
  ames_sf,
  Sale_Price,
  predictions,
  n = list(
    c(10, 10),
    c(1, 1)
  ),
  square = FALSE
)

# or, mostly equivalently
# (there will be a slight difference due to `autoexpand_grid = TRUE`)
grids <- list(
  sf::st_make_grid(ames_sf, n = c(10, 10), square = FALSE),
  sf::st_make_grid(ames_sf, n = c(1, 1), square = FALSE)
)
ww_multi_scale(ames_sf, Sale_Price, predictions, grids = grids)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ww_willmott_d")
### * ww_willmott_d

flush(stderr()); flush(stdout())

### Name: ww_willmott_d
### Title: Willmott's d and related values
### Aliases: ww_willmott_d ww_willmott_d.data.frame ww_willmott_d_vec
###   ww_willmott_d1 ww_willmott_d1.data.frame ww_willmott_d1_vec
###   ww_willmott_dr ww_willmott_dr.data.frame ww_willmott_dr_vec
###   ww_systematic_mse ww_systematic_mse.data.frame ww_systematic_mse_vec
###   ww_unsystematic_mse ww_unsystematic_mse.data.frame
###   ww_unsystematic_mse_vec ww_systematic_rmse
###   ww_systematic_rmse.data.frame ww_systematic_rmse_vec
###   ww_unsystematic_rmse ww_unsystematic_rmse.data.frame
###   ww_unsystematic_rmse_vec

### ** Examples

x <- c(6, 8, 9, 10, 11, 14)
y <- c(2, 3, 5, 5, 6, 8)

ww_willmott_d_vec(x, y)
ww_willmott_d1_vec(x, y)
ww_willmott_dr_vec(x, y)
ww_systematic_mse_vec(x, y)
ww_unsystematic_mse_vec(x, y)
ww_systematic_rmse_vec(x, y)
ww_unsystematic_rmse_vec(x, y)

example_df <- data.frame(x = x, y = y)
ww_willmott_d(example_df, x, y)
ww_willmott_d1(example_df, x, y)
ww_willmott_dr(example_df, x, y)
ww_systematic_mse(example_df, x, y)
ww_unsystematic_mse(example_df, x, y)
ww_systematic_rmse(example_df, x, y)
ww_unsystematic_rmse(example_df, x, y)




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
