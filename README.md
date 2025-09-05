# vip: Variable Importance Plots 

<div align="center">
  <img src="man/figures/logo-vip.png" width="200" height="230" />
  
  **🔍 Make your ML models more interpretable with beautiful variable importance plots**
  
  [![CRAN Status](https://www.r-pkg.org/badges/version/vip)](https://cran.r-project.org/package=vip)
  [![R-CMD-check](https://github.com/koalaverse/vip/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/koalaverse/vip/actions/workflows/R-CMD-check.yaml)
  [![Codecov](https://codecov.io/gh/koalaverse/vip/graph/badge.svg)](https://app.codecov.io/github/koalaverse/vip?branch=master)
  [![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/vip)](https://cran.r-project.org/package=vip/)
  [![R Journal](https://img.shields.io/badge/The%20R%20Journal-10.32614%2FRJ--2020--013-brightgreen)](https://doi.org/10.32614/RJ-2020-013)
  [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
</div>

---

## 🚀 What is vip?

**vip** provides a unified framework for constructing **variable importance plots** from virtually any machine learning model in R. Stop juggling different `importance()` functions across packages – `vip` gives you **one consistent interface** for interpretable ML.

### ✨ Key features

- **Universal interface**: Works with 40+ model types from different packages
- **Multiple methods**: Model-specific, permutation, SHAP, and variance-based importance
- **Beautiful plots**: Publication-ready visualizations with ggplot2
- **Efficient algorithms**: Optimized with parallel processing support
- **Extensible design**: Easy to add support for new model types
- **Well-documented**: Comprehensive guides and academic backing

## 🛠️ Quick start

### Installation

```r
# Install from CRAN (stable)
install.packages("vip")

# Install development version (latest features)
# install.packages("pak")
pak::pak("koalaverse/vip")
```

### 30-second example

```r
library(vip)
library(randomForest)

# Fit a model
model <- randomForest(Species ~ ., data = iris)

# Get importance scores
vi_scores <- vi(model)
print(vi_scores)
#> # A tibble: 4 × 2
#>   Variable     Importance
#>   <chr>             <dbl>
#> 1 Petal.Length      32.4 
#> 2 Petal.Width       31.3 
#> 3 Sepal.Length       9.51
#> 4 Sepal.Width        6.75

# Create a beautiful plot
vip(model)
```

## 🎯 Supported methods

| Method | Description | Use case | Function |
|--------|-------------|----------|----------|
| **Model-specific** | Extract built-in importance | Fast, model-native | `vi(model, method = "model")` |
| **Permutation** | Shuffle features, measure impact | Model-agnostic, robust | `vi(model, method = "permute")` |
| **Shapley values** | Game theory attribution | Detailed explanations | `vi(model, method = "shap")` |
| **Variance-based** | FIRM approach | Feature ranking | `vi(model, method = "firm")` |

## 🔧 Supported models (40+)

**Tree-based models**
- `randomForest` • `ranger` • `xgboost` • `lightgbm` • `gbm` • `C5.0` • `Cubist` • `rpart` • `party` • `partykit`

**Linear models**
- `lm` • `glm` • `glmnet` • `earth` (MARS)

**Neural networks**
- `nnet` • `neuralnet` • `h2o` • `RSNNS`

**Meta-frameworks**
- `caret` • `tidymodels` • `parsnip` • `workflows` • `mlr` • `mlr3` • `sparklyr`

**Specialized models**
- `pls` • `mixOmics` • And many more...

## 🏃‍♂️ Advanced examples

### Permutation importance with custom metrics

```r
library(ranger)

# Fit model
rf_model <- ranger(mpg ~ ., data = mtcars, importance = "none")

# Permutation importance with custom metric
vi_perm <- vi(
  rf_model, 
  method = "permute",
  train = mtcars,
  target = "mpg",
  metric = "rmse",
  nsim = 50,        # 50 permutations for stability
  parallel = TRUE   # Speed up with parallel processing
)

# Create enhanced plot
vip(vi_perm, num_features = 10, geom = "point") +
  labs(title = "Permutation-based Variable Importance",
       subtitle = "RMSE metric, 50 permutations") +
  theme_minimal()
```

### SHAP values for detailed attribution

```r
library(xgboost)

# Prepare data
X <- data.matrix(subset(mtcars, select = -mpg))
y <- mtcars$mpg

# Fit XGBoost model
xgb_model <- xgboost(data = X, label = y, nrounds = 100, verbose = 0)

# SHAP-based importance
vi_shap <- vi(
  xgb_model, 
  method = "shap",
  train = X,
  nsim = 30
)

# Beautiful SHAP plot
vip(vi_shap, geom = "col", aesthetics = list(fill = "steelblue", alpha = 0.8)) +
  labs(title = "SHAP-based Variable Importance") +
  theme_light()
```

## 🤝 Contributing and development

We welcome contributions! Here's how to get involved:

### Development setup

```bash
# Clone the repo
git clone https://github.com/koalaverse/vip.git
cd vip

# Open in RStudio or your favorite editor
# The project includes a comprehensive CLAUDE.md development guide
```

### Testing framework

We use `tinytest` for lightweight, reliable testing:

```r
# Run all tests
tinytest::test_package("vip")

# Test specific functionality
tinytest::run_test_file("inst/tinytest/test_vip.R")
```

### Development workflow

1. **Check issues**: Look for [good first issues](https://github.com/koalaverse/vip/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22)
2. **Create branch**: `git checkout -b feature/awesome-feature`
3. **Write tests**: Follow TDD principles (see `CLAUDE.md`)
4. **Run checks**: `R CMD check` and tests
5. **Submit PR**: With clear description

### Adding model support

Adding support for new models is straightforward:

```r
# Add S3 method to R/vi_model.R
vi_model.your_model_class <- function(object, ...) {
  # Extract importance from your model
  importance_scores <- your_model_importance_function(object)
  
  # Return as tibble
  tibble::tibble(
    Variable = names(importance_scores),
    Importance = as.numeric(importance_scores)
  )
}
```

See `CLAUDE.md` for detailed instructions!

## 📚 Learning resources

- **[Package website](https://koalaverse.github.io/vip/)** - Comprehensive documentation
- **[R Journal paper](https://doi.org/10.32614/RJ-2020-013)** - Academic foundation
- **[IML book](https://christophm.github.io/interpretable-ml-book/)** - Theory background
- **[Development guide](CLAUDE.md)** - Contributing guidelines

## 🏆 Recognition

- **Featured in [The R Journal](https://doi.org/10.32614/RJ-2020-013)** (peer-reviewed)
- **400+ GitHub stars** and growing
- **Used by 100+ CRAN packages** in reverse dependencies
- **Thousands of monthly downloads** worldwide

## ✨ What's new in v0.4.1

- ✅ **ggplot2 S7 compatibility** - Future-proof plotting
- **lightgbm support** - Popular gradient boosting
- **Enhanced yardstick integration** - Better metrics
- **Improved documentation** - Clearer examples

See [NEWS.md](NEWS.md) for complete version history and migration notes.

## 🆘 Getting help

- **Bug reports**: [GitHub Issues](https://github.com/koalaverse/vip/issues)
- **Feature requests**: [GitHub Discussions](https://github.com/koalaverse/vip/discussions)
- **Questions**: [Stack Overflow](https://stackoverflow.com/questions/tagged/vip) (tag: `vip`)

## 📄 Citation

If you use vip in your research, please cite:

```bibtex
@article{greenwell2020variable,
  title={Variable Importance Plots—An Introduction to the vip Package},
  author={Greenwell, Brandon M and Boehmke, Bradley C},
  journal={The R Journal},
  volume={12},
  number={1},
  pages={343--366},
  year={2020},
  doi={10.32614/RJ-2020-013}
}
```

## 📜 License

GPL (>= 2) © [Brandon M. Greenwell](https://github.com/bgreenwell), [Brad Boehmke](https://github.com/bradleyboehmke)

---

<div align="center">
  
**⭐ Star us on GitHub if vip helps make your models more interpretable! ⭐**

*Built with ❤️ by the [koalaverse](https://github.com/koalaverse) team*

</div>