<!-- README.md is generated from README.Rmd. Please edit that file -->
vip
===

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
