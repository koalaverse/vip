# moreparty

<details>

* Version: 0.3.1
* GitHub: NA
* Source code: https://github.com/cran/moreparty
* Date/Publication: 2023-04-02 13:20:02 UTC
* Number of recursive dependencies: 164

Run `revdepcheck::revdep_details(, "moreparty")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘moreparty-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: EasyTreeVarImp
    > ### Title: Variable importance for conditional inference trees.
    > ### Aliases: EasyTreeVarImp
    > ### Keywords: tree
    > 
    > ### ** Examples
    > 
    >   data(iris)
    >   iris2 = iris
    >   iris2$Species = factor(iris$Species == "versicolor")
    >   iris.ct = partykit::ctree(Species ~ ., data = iris2)
    >   EasyTreeVarImp(iris.ct, nsim = 1)
    Error: `metric()` must be a function with arguments `truth` and `estimate`; consider using one of the vector metric functions from the `yardstick` package (e.g., `metric = yardstick::huber_loss_vec`).
    Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘vip::vint’
    ```

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'GetInteractionStrength.Rd':
      ‘vint’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# prettyglm

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/prettyglm
* Date/Publication: 2022-12-13 13:20:02 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::revdep_details(, "prettyglm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘prettyglm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pretty_coefficients
    > ### Title: pretty_coefficients
    > ### Aliases: pretty_coefficients
    > 
    > ### ** Examples
    > 
    > 
    ...
    > pretty_coefficients(survival_model3,
    +                     relativity_transform = 'exp(estimate)-1',
    +                     spline_seperator = '_',
    +                     vimethod = 'permute',
    +                     target = 'Survived',
    +                     metric = 'auc',
    +                     pred_wrapper = predict.glm,
    +                     reference_class = 0)
    Error: Metric "auc" is not supported; use `vip::list_metrics()` to print a list of currently supported metrics. Alternatively, you can pass in a `yardstick` vector function directly (e.g., `metric = yardstick::poisson_log_loss_vec` (just be sure to also set the `smaller_is_better` argument.
    Execution halted
    ```

# SAEforest

<details>

* Version: 1.0.0
* GitHub: https://github.com/krennpa/SAEforest
* Source code: https://github.com/cran/SAEforest
* Date/Publication: 2022-09-07 17:50:06 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::revdep_details(, "SAEforest")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘vip::grid.arrange’
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 37 marked Latin-1 strings
      Note: found 172 marked UTF-8 strings
    ```

# waywiser

<details>

* Version: 0.4.0
* GitHub: https://github.com/ropensci/waywiser
* Source code: https://github.com/cran/waywiser
* Date/Publication: 2023-05-17 16:10:02 UTC
* Number of recursive dependencies: 172

Run `revdepcheck::revdep_details(, "waywiser")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘waywiser-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: predict.ww_area_of_applicability
    > ### Title: Predict from a 'ww_area_of_applicability'
    > ### Aliases: predict.ww_area_of_applicability
    > 
    > ### ** Examples
    > 
    > ## Don't show: 
    ...
    
        vi
    
    > train <- gen_friedman(1000, seed = 101)
    > test <- train[701:1000, ]
    > train <- train[1:700, ]
    > pp <- stats::ppr(y ~ ., data = train, nterms = 11)
    > importance <- vi_permute(pp, target = "y", metric = "rsquared", pred_wrapper = predict)
    Error: Metric "rsquared" is not supported; use `vip::list_metrics()` to print a list of currently supported metrics. Alternatively, you can pass in a `yardstick` vector function directly (e.g., `metric = yardstick::poisson_log_loss_vec` (just be sure to also set the `smaller_is_better` argument.
    Execution halted
    ```

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       2. └─vip:::vi_permute.default(...)
       3.   └─vip:::get_metric(metric)
      ── Error ('test-tidy_importance.R:6:3'): tidy_importance is idempotent ─────────
      Error: Metric "rsquared" is not supported; use `vip::list_metrics()` to print a list of currently supported metrics. Alternatively, you can pass in a `yardstick` vector function directly (e.g., `metric = yardstick::poisson_log_loss_vec` (just be sure to also set the `smaller_is_better` argument.
      Backtrace:
          ▆
       1. ├─vip::vi_permute(pp, target = "y", metric = "rsquared", pred_wrapper = predict) at test-tidy_importance.R:6:2
       2. └─vip:::vi_permute.default(...)
       3.   └─vip:::get_metric(metric)
      
      [ FAIL 3 | WARN 0 | SKIP 136 | PASS 251 ]
      Deleting unused snapshots:
      • srr-ww_local_getis_ord_pvalue.md
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

