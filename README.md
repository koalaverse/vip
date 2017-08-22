<!-- README.md is generated from README.Rmd. Please edit that file -->
vip: Variable Importance Plots <img src="tools/vip-logo.png" align="right" />
=============================================================================

Variable importance plots.

Installation
------------

The `vip` package is currently only available from GitHub, but can easily be installed using the [devtools](https://CRAN.R-project.org/package=devtools) package:

``` r
if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("AFIT-R/vip")
```

Example usage
-------------

For illustration, we use one of the regression problems described in Friedman (1991) and Breiman (1996). Inputs are 10 independent variables uniformly distributed on the interval \[0,1\]; only 5 out of these 10 are actually used. Outputs are created according to the formula

𝒴 = 10sin(*π**x*<sub>1</sub>*x*<sub>2</sub>) + 20(*x*<sub>3</sub>−0.5)<sup>2</sup> + 10*x*<sub>4</sub> + 5*x*<sub>5</sub> + *ϵ*,

where *ϵ* ∼ *N*(0,*σ*). These data are available in the [mlbench](https://CRAN.R-project.org/package=mlbench) package. The code chunk below simulates 500 observations from the above model with $\\simga = 1$.

``` r
# Load required packages
library(mlbench)

# Simulate training data
set.seed(101)  # for reproducibility
trn <- as.data.frame(mlbench.friedman1(500))  # ?mlbench.friedman1
tibble::glimpse(trn)  # take a peek
#> Observations: 500
#> Variables: 11
#> $ x.1  <dbl> 0.37219838, 0.04382482, 0.70968402, 0.65769040, 0.2498557...
#> $ x.2  <dbl> 0.405543776, 0.602277006, 0.361999719, 0.291215631, 0.793...
#> $ x.3  <dbl> 0.1016229, 0.6022517, 0.2536424, 0.5419870, 0.3834077, 0....
#> $ x.4  <dbl> 0.32248033, 0.99866400, 0.54841191, 0.32741994, 0.9474793...
#> $ x.5  <dbl> 0.69258669, 0.77643413, 0.01797597, 0.22965950, 0.4623621...
#> $ x.6  <dbl> 0.757968756, 0.532993932, 0.764821812, 0.300911111, 0.004...
#> $ x.7  <dbl> 0.5178155773, 0.5094877817, 0.7150814026, 0.1767543119, 0...
#> $ x.8  <dbl> 0.53039420, 0.48748739, 0.84445523, 0.34578953, 0.1141428...
#> $ x.9  <dbl> 0.87786519, 0.11769849, 0.33436272, 0.47444784, 0.4886461...
#> $ x.10 <dbl> 0.76275126, 0.17556921, 0.11837389, 0.28301930, 0.3106974...
#> $ y    <dbl> 14.871525, 15.265025, 15.058655, 10.734693, 17.599652, 18...
```

Next, we fit a random forest to the simulated data and construct variable importance plots using the two methods provided by the random forest algorithm (left and middle plots) and the partial dependence approach (right plot).

``` r
# Load required packages
library(ggplot2)
library(magrittr)
library(randomForest)  
library(vip)

# Fit a random forest
set.seed(102)
trn.rf <- randomForest(y ~ ., data = trn, importance = TRUE)

# Importance: mean decrease in accuracy
imp1 <- importance(trn.rf, type = 1) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable")
p1 <- ggplot(imp1, aes(x = reorder(Variable, `%IncMSE`), y = `%IncMSE`)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  theme_light()

# Importance: mean decrease node impurity
imp2 <- importance(trn.rf, type = 2) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable")
p2 <- ggplot(imp2, aes(x = reorder(Variable, IncNodePurity), y = IncNodePurity)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  theme_light()

# Importance: partial dependence
p3 <- vip(trn.rf, use.partial = TRUE, pred.var = paste0("x.", 1:10)) +
  ylab("pdVarImp") +
  theme_light()

# Display all three plots together
grid.arrange(p1, p2, p3, ncol = 3)
```

![](tools/README-example-rf-1.png)
