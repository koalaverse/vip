# These tests are a work in progress!

# Load required packages
library(randomForest)
library(vip)

# Regression -------------------------------------------------------------------



# Classification (binary) -------------------------------------------------


# Classification (multiclass) ---------------------------------------------

set.seed(301)
rfo3 <- randomForest(Species ~ ., data = iris)

class_probs <- function(object, newdata) {
  predict(object, newdata = newdata, type = "prob")
}
class_labels <- function(object, newdata) {
  predict(object, newdata = newdata, type = "response")
}

set.seed(302)
vips <- lapply(c("error", "mauc", "mlogloss"), FUN = function(m) {
  vip(rfo3, method = "permute", response_name = "Species", metric = m,
      pred_fun = if (m == "error") class_labels else class_probs) +
    ggplot2::ggtitle(m)
})
grid.arrange(grobs = vips, ncol = 3)

