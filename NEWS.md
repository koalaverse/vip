# NEWS for vip package

### Changes for version 0.0.2

* Added support for XGBoost models (i.e., objects of class `"xgb.booster"`).

* Added support for ranger models (i.e., objects of class `"ranger"`).

* Added support for random forest models from the `party` package (i.e., objects of class `"RandomForest"`).

* `vip()` gained a new argument, `num_features`, for specifying how many variable importance scores to plot. The default is set to `10`.

* `.` was changed to `_` in all argument names.

* `vi()` gained three new arguments: `truncate_feature_names` (for truncating feature names in the returned tibble), `sort` (a logical argument specifying whether or not the resulting variable importance scores should be sorted), and `decreasing` (a logical argument specifying whether or not the variable importance scores should be sorted in decreasing order).

### Changes for version 0.0.1

* Initial release.b
