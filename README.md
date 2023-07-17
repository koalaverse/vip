
# vip: Variable Importance Plots <img src="man/figures/logo-vip.png" align="right" width="130" height="150" />

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/vip)](https://cran.r-project.org/package=vip)
[![R-CMD-check](https://github.com/koalaverse/vip/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/koalaverse/vip/actions/workflows/R-CMD-check.yaml)
[![Coverage
Status](https://codecov.io/gh/koalaverse/vip/graph/badge.svg)](https://app.codecov.io/github/koalaverse/vip?branch=master)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Downloads](https://cranlogs.r-pkg.org/badges/vip)](https://cran.r-project.org/package=vip/)
[![The R
Journal](https://img.shields.io/badge/The%20R%20Journal-10.32614%2FRJ--2020--013-brightgreen)](https://doi.org/10.32614/RJ-2020-013)
<!-- badges: end -->

## Overview

[vip](https://koalaverse.github.io/vip/index.html) is an R package for
constructing **v**ariable **i**mportance **p**lots (VIPs). VIPs are part
of a larger framework referred to as *interpretable machine learning*
(IML), which includes (but not limited to): partial dependence plots
(PDPs) and individual conditional expectation (ICE) curves. While PDPs
and ICE curves (available in the R package
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
[vip](https://koalaverse.github.io/vip/index.html) offers a number of
*model-agnostic* procedures for computing feature importance (see the
next section) as well an experimental function for quantifying the
strength of potential interaction effects. For details and example
usage, visit the [vip package
website](https://koalaverse.github.io/vip/index.html).

<img src="man/figures/one-pkg.png" width="50%" style="display: block; margin: auto;" />

## Features

- **Model-based variable importance** - Compute variable importance
  specific to a particular model (like a *random forest*, *gradient
  boosted decision trees*, or *multivariate adaptive regression
  splines*) from a wide range of R packages (e.g.,
  [randomForest](https://cran.r-project.org/package=randomForest),
  [ranger](https://cran.r-project.org/package=ranger),
  [xgboost](https://cran.r-project.org/package=xgboost), and many more).
  Also supports the [caret](https://cran.r-project.org/package=caret)
  and [parsnip](https://cran.r-project.org/package=parsnip) (starting
  with version 0.0.4) packages.

- **Permutation-based variable importance** - An efficient
  implementation of the permutation feature importance algorithm
  discussed in [this
  chapter](https://christophm.github.io/interpretable-ml-book/feature-importance.html)
  from [Christoph Molnar’s *Interpretable Machine Learning*
  book](https://christophm.github.io/interpretable-ml-book/).

- **Shapley-based variable importance** - An efficient implementation of
  feature importance based on the popular [Shapley
  values](https://github.com/slundberg/shap) via the
  [fastshap](https://cran.r-project.org/package=fastshap) package.

- **Variance-based variable importance** - Compute variable importance
  using a simple *feature importance ranking measure* (FIRM) approach.
  For details, see see [Greenwell et
  al. (2018)](https://arxiv.org/abs/1805.04755) and [Scholbeck et
  al. (2019)](https://arxiv.org/abs/1904.03959).

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
