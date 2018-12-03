# vip 0.1.2

* New argument `rank`, which default to `FALSE`, available in `vi()` and `vip()`.

* Added support for Spark (G)LMs.

* Bux fixes.


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
