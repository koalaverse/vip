
# vip: Variable Importance Plots <img src="man/figures/logo-vip.png" align="right" width="130" height="150" />

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/vip)](https://cran.r-project.org/package=vip)
[![Travis-CI Build
Status](https://travis-ci.org/koalaverse/vip.svg?branch=master)](https://travis-ci.org/koalaverse/vip)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/koalaverse/vip?branch=master&svg=true)](https://ci.appveyor.com/project/koalaverse/vip)
[![Coverage
Status](https://img.shields.io/codecov/c/github/koalaverse/vip/master.svg)](https://codecov.io/github/koalaverse/vip?branch=master)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

## Overview

[**vip**](https://koalaverse.github.io/vip/index.html) is an R package
for constructing **v**ariable **i**mportance **p**lots (VIPs). VIPs are
part of a larger framework referred to as *interpretable machine
learning* (IML), which includes (but not limited to): partial dependence
plots (PDPs) and individual conditional expectation (ICE) curves. While
PDPs and ICE curves (available in the R package
[pdp](https://cran.r-project.org/package=pdp)) help visualize feature
effects, VIPs help visualize feature impact (either locally or
globally). An in-progress, but comprehensive, overview of IML can be
found here: <https://github.com/christophM/interpretable-ml-book>.

Many supervised learning algorithms can naturally emit some measure of
importance for the features used in the model, and these approaches are
embedded in many different packages. The downside, however, is that each
package uses a different function and interface and it can be
challenging (and distracting) to have to remember each one (e.g.,
remembering to use `xgb.importance()` for
[xgboost](https://cran.r-project.org/package=xgboost) models and
`gbm.summary()` for [gbm](https://cran.r-project.org/package=gbm)
models). With [vip](https://cran.r-project.org/package=vip) you get one
consistent interface to computing variable importance for many types of
supervised learning models across a number of packages. Additionally,
[**vip** package website](https://koalaverse.github.io/vip/index.html)
offers a number of *model-agnostic* procedures for computing feature
importance (see the next section) as well an experimental function for
quantifying the strength of potential interaction effects. For details
and example usage, visit the [**vip** package
website](https://koalaverse.github.io/vip/index.html).

<img src="man/figures/one-pkg.png" width="50%" style="display: block; margin: auto;" />

## Features

  - **Model-based variable importance** - Compute variable importance
    specific to a particular model (like a *random forest*, *gradient
    boosted decision trees*, or *multivariate adaptive regression
    splines*) from a wide range of package (e.g.,
    [randomForest](https://cran.r-project.org/package=randomForest),
    [ranger](https://cran.r-project.org/package=ranger),
    [xgboost](https://cran.r-project.org/package=xgboost), and many
    more). Also supports the
    [caret](https://cran.r-project.org/package=caret) and
    [parsnip](https://cran.r-project.org/package=parsnip) (starting with
    version 0.0.4) packages.

  - **Permutation-based variable importance** - An efficient
    implementation of the permutation feature importance algorithm
    discussed in [this
    chapter](https://christophm.github.io/interpretable-ml-book/feature-importance.html)
    from [Christoph Molnar’s *Interpretable Machine Learning*
    book](https://christophm.github.io/interpretable-ml-book/).

  - **SHAP-based variable importance** - An efficient implementation of
    feature importance based on the popular [SHAP
    values](https://github.com/slundberg/shap) via the
    [fastshap](https://cran.r-project.org/package=fastshap) package.

  - **PDP/ICE-based variable importance** - Compute variable importance
    by quantifying the variability in marginal effect plots like
    [partial dependence plots and individual conditional
    expectations](https://arxiv.org/abs/1805.04755) via the
    [pdp](https://cran.r-project.org/package=pdp) package.

## Installation

``` r
# The easiest way to get vip is to install it from CRAN:
install.packages("vip")

# Alternatively, you can install the development version from GitHub:
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}
remotes::install_github("koalaverse/vip")
```
