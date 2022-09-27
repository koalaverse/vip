# vip 0.3.3

## Bug fixes

* Fix model-based VI support for [mlr](https://cran.r-project.org/package=mlr), [mlr3](https://cran.r-project.org/package=mlr3), [parsnip](https://cran.r-project.org/package=parsnip), and [workflows](https://cran.r-project.org/package=workflows) model fits.

# vip 0.3.2

## Miscellaneous

* Add `tools/` to .Rbuildignore.

# vip 0.3.1

## Miscellaneous

* Change http://spark.rstudio.com/mlib/ to https://spark.rstudio.com/mlib/ in NEWS.md.

* Remove unnecessary codecov.yml file.

# vip 0.3.0

## User-visable changes

* Removed deprecated arguments from `vip()`; in particular, `bar`, `width`, `alpha`, `color`, `fill`, `size`, and `shape`. Users should instead rely on the `mapping` and `aesthetics` arguments; see `?vip::vip` for details.

## Bug fixes

* Fixed a couple bugs that occurred when using `vi_model()` with the [glmnet](https://cran.r-project.org/package=glmnet) package. In particular, we added a new `lamnda` parameter for specifying the value of the penalty term to use when extracting the estimated coefficients. This is equivalent to the `s` argument in `glmnet::coef()`; the name `lambda` was chosen to not conflict with other arguments in `vi()`. Additionally, `vi_model()` did not return the absolute value of the estimated coefficients for [glmnet](https://cran.r-project.org/package=glmnet) models like advertised, but is now fixed in this version [(#103)](https://github.com/koalaverse/vip/issues/103).

## Miscellaneous

* Switched from Travis-CI to GitHub Actions for continuous integration.

* Added a CITATION file and PDF-based vignette based off of the published article in [The R Journal](https://journal.r-project.org/archive/2020/RJ-2020-013/index.html) [(#109)](https://github.com/koalaverse/vip/issues/109).

* Switch from `tibble::as.tibble()`---which was deprecated in [tibble](https://github.com/tidyverse/tibble) 2.0.0---to `tibble::as_tibble()` in a few function calls [(#101)](https://github.com/koalaverse/vip/issues/101).

# vip 0.2.2

## User-visible changes

* The `Importance` column from `vi_model()` no longer contains "inner" names; in accordance with breaking changes in [tibble](https://github.com/tidyverse/tibble) 3.0.0.

# vip 0.2.1

## Enhancements

* Added support for SHAP-based feature importance which makes use of the recent [fastshap](https://cran.r-project.org/package=fastshap) package on CRAN. To use, simply call `vi()` or `vip()` and specify `method = "shap"`, or you can just call `vi_shap()` directly [(#87)](https://github.com/koalaverse/vip/issues/87).

* Added support for the [parsnip](https://cran.r-project.org/package=parsnip), [mlr](https://cran.r-project.org/package=mlr), and [mlr3](https://cran.r-project.org/package=mlr3) packages [(#94)](https://github.com/koalaverse/vip/issues/94).

* Added support for `"mvr"` objects from the [pls](https://cran.r-project.org/package=pls) package (currently just calls `caret::varImp()`) [(#35)](https://github.com/koalaverse/vip/issues/35).

* The `"lm"` method for `vi_model()` gained a new `type` argument that allows users
to use either (1) the raw coefficients if the features were properly standardized (`type = "raw"`), or (2) the absolute value of the corresponding *t*- or *z*-statistic (`type = "stat"`, the default) [(#77)](https://github.com/koalaverse/vip/issues/77).

* New function `gen_friedman()` for simulating data from the Friedman 1 benchmark problem; see `?vip::gen_friedman` for details.

## User-visible changes

* The `vi_pdp()` and `vi_ice()` functions have been deprecated and merged into a single new function called `vi_firm()`. Consequently, setting `method = "pdp"` and `method = "ice"` has also been deprecated; use `method = "firm"` instead.

* The `metric` and `pred_wrapper` arguments to `vi_permute()` are no longer optional.

* The `vip()` function gained a new argument, `geom`, for specifying which type of plot to construct. Current options are `geom = "col"` (the default), `geom = "point"`, `geom = "boxplot"`, or `geom = "violin"` (the latter two only work for permutation-based importance with `nsim > 1`) [(#79)](https://github.com/koalaverse/vip/issues/79). Consequently, the `bar` argument has been removed.

* The `vip()` function gained two new arguments for specifying aesthetics: `mapping` and `aesthetics` (for fixed aesthetics like `color = "red"`). Consequently, the arguments `color`, `fill`, etc. have been removed [(#80)](https://github.com/koalaverse/vip/issues/80).

An example illustrating the above two changes is given below:
```r
# Load required packages
library(ggplot2)  # for `aes_string()` function

# Load the sample data
data(mtcars)

# Fit a linear regression model
model <- lm(mpg ~ ., data = mtcars)

# Construct variable importance plots
p1 <- vip(model)
p2 <- vip(model, mapping = aes_string(color = "Sign"))
p3 <- vip(model, type = "dotplot")
p4 <- vip(model, type = "dotplot", mapping = aes_string(color = "Variable"),
          aesthetics = list(size = 3))
grid.arrange(p1, p2, p3, p4, nrow = 2)
```

* The `vip()` function gained a new argument, `include_type`, which defaults to `FALSE`. If `TRUE`, the type of variable importance that was computed is included in the appropriate axis label. Set `include_type = TRUE` to revert to the old behavior.

## Miscellaneous

* Removed dependency on [ModelMetrics](https://cran.r-project.org/package=ModelMetrics) and the built-in family of performance metrics (`metric_*()`) are now documented and exported. See, for example, `?vip::metric_rmse` [(#93)](https://github.com/koalaverse/vip/issues/93).

* Switched to the [tinytest](https://cran.r-project.org/package=tinytest) framework [(#82)](https://github.com/koalaverse/vip/issues/82).

* Minor documentation improvements.

## Bug fixes

* The internal (i.e., not exported) `get_feature_names()` function does a better job with `"nnet"` objects containing factors. It also does a better job at extracting feature names from model objects containing a `"formula"` component.

* `vi_model()` now works correctly for `"glm"` objects with non-Gaussian families (e.g., logistic regression) [(#74)](https://github.com/koalaverse/vip/issues/74).

* Added appropriate **sparklyr** version dependency [(#59)](https://github.com/koalaverse/vip/issues/59).


# vip 0.1.3

## New functions

* Removed warnings from experimental functions.

* `vi_permute()` gained a type argument (i.e., `type = "difference"` or `type = "ratio"`); this argument can be passed via `vi()` or `vip()` as well.

* `add_sparklines()` creates an HTML widget to display variable importance scores with a sparkline representation of each features effect (i.e., its partial dependence function) [(#64)](https://github.com/koalaverse/vip/issues/64).

* Added support for the Olden and Garson algorithms with neural networks fit using the __neuralnet__, __nnet__, and __RSNNS__ packages [(#28)](https://github.com/koalaverse/vip/issues/28).

* Added support for GLMNET models fit using the __glmnet__ package (with and without cross-validation).

## Breaking changes

* The `pred_fun` argument in `vi_permute()` has been changed to `pred_wrapper`.

* The `FUN` argument to `vi()`, `vi_pdp()`, and `vi_ice()` has been changed to `var_fun`.

* Only the predicted class probabilities for the reference class are required (as a numeric vector) for binary classification when `metric = "auc"` or `metric = "logloss"`.


## Minor changes

* `vi_permute()` gained a new logical `keep` argument. If `TRUE` (the default), the raw permutation scores from all `nsim` repetitions (provided `nsim > 1`) will be stored in an attribute called `"raw_scores"`.

* `vip()` gained new logical arguments `all_permutations` and `jitter ` which help to visualize the raw permutation scores for all `nsim` repetitions (provided `nsim > 1`).

* You can now pass a `type` argument to `vi_permute()` specifying how to compare the baseline and permuted performance metrics. Current choices are `"difference"` (the default) and `"ratio"`.

* Improved documentation (especially for `vi_permute()` and `vi_model()`).

* Results from `vi_model()`, `vi_pdp()`, `vi_ice()`, and `vi_permute()` now have class `"vi"`, making them easier to plot with `vip()`.


# vip 0.1.2

* Added `nsim` argument to `vi_permute()` for reducing the sampling variability induced by permuting each predictor [(#36)](https://github.com/koalaverse/vip/issues/36).

* Added `sample_size` and `sample_frac` arguments to `vi_permute()` for reducing the size of the training sample for every Monte Carlo repetition [(#41)](https://github.com/koalaverse/vip/issues/41).

* Greatly improved the documentation for `vi_model()` and the various objects it supports.

* New argument `rank`, which defaults to `FALSE`, available in `vi()` [(#55)](https://github.com/koalaverse/vip/issues/55).

* Added support for Spark (G)LMs.

* `vi()` is now a generic which makes adding new methods easier (e.g., to support [DataRobot](https://www.datarobot.com/) models).

* Bug fixes.


# vip 0.1.1

* Fixed bug in `get_feature_names.ranger()` s.t. it never returns `NULL`; it either returns the feature names or throws an error if they cannot be recovered from the model object [(#43)](https://github.com/koalaverse/vip/issues/43).

* Added `pkgdown` site: https://github.com/koalaverse/vip.

* Changed `truncate_feature_names` argument of `vi()` to `abbreviate_feature_names` which abbreviates all feature names, rather than just truncating them.

* Added CRAN-related badges [(#32)](https://github.com/koalaverse/vip/issues/32).

* New generic `vi_permute()` for constructing permutation-based variable importance scores [(#19)](https://github.com/koalaverse/vip/issues/19).

* Fixed bug and unnecessary error check in `vint()` [(#38)](https://github.com/koalaverse/vip/issues/38).

* New vignette on using `vip` with unsupported models (using the Keras API to TensorFlow as an example).

* Added basic [sparklyr](http://spark.rstudio.com/mlib/) support.


# vip 0.1.0

* Added support for XGBoost models (i.e., objects of class `"xgb.booster"`).

* Added support for ranger models (i.e., objects of class `"ranger"`).

* Added support for random forest models from the `party` package (i.e., objects of class `"RandomForest"`).

* `vip()` gained a new argument, `num_features`, for specifying how many variable importance scores to plot. The default is set to `10`.

* `.` was changed to `_` in all argument names.

* `vi()` gained three new arguments: `truncate_feature_names` (for truncating feature names in the returned tibble), `sort` (a logical argument specifying whether or not the resulting variable importance scores should be sorted), and `decreasing` (a logical argument specifying whether or not the variable importance scores should be sorted in decreasing order).

* `vi_model.lm()`, and hence `vi()`, contains an additional column called `Sign` that contains the sign of the original coefficients [(#27)](https://github.com/koalaverse/vip/issues/27).

* `vi()` gained a new argument, `scale`, for scaling the variable importance scores so that the largest is 100. Default is `FALSE` [(#24)](https://github.com/koalaverse/vip/issues/24). 

* `vip()` gained two new arguments, `size` and `shape`, for controlling the size and shape of the points whenever `bar = FALSE` [(#9)](https://github.com/koalaverse/vip/issues/9).

* Added support for `"H2OBinomialModel"`, `"H2OMultinomialModel"`, and, `"H2ORegressionModel"` objects [(#8)](https://github.com/koalaverse/vip/issues/8).


# vip 0.0.1

* Initial release.
