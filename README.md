<!-- README.md is generated from README.Rmd. Please edit that file -->
vip: Variable Importance Plots <img src="tools/vip-logo.png" align="right" width="120" height="139" />
======================================================================================================

Variable importance plots.

Linear model example
--------------------

``` r
library(vip)
# install.packages("pdp")
data(boston, package = "pdp")  # load the boston housing data
boston.lm <- lm(cmedv ~ ., data = boston)
vip(boston.lm)
```

![](tools/README-example-lm-1.png)
