# vip 0.1.1

* Changed `truncate_feature_names` argument of `vi()` to `abbreviate_feature_names` which abbreviates all feature names to at least the length `abbreviate_feature_names`.

* Added CRAN badge [(#32)](https://github.com/koalaverse/vip/issues/32).


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
