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
