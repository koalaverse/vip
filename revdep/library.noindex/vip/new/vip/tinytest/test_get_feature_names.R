# Specify model formulae
form1 <- y ~ x1 + x2 + I(x2 ^ 2) + sin(x2)
form2 <- ~ x1 + x2 + I(x2 ^ 2) + sin(x2)  # no LHS
form3 <- terms(y ~ ., data = data.frame(y = 1:5, x1 = 1:5, x2 = 1:5))

# Expectations
expect_identical(
  current = vip:::get_feature_names.formula(form1),
  target = c("x1", "x2")
)
expect_error(
  current = vip:::get_feature_names.formula(form2)
)
expect_identical(  # check dot expansion
  current = vip:::get_feature_names.formula(form3),
  target = c("x1", "x2")
)

# Exits
if (!requireNamespace("nnet", quietly = TRUE)) {
  exit_file("Package nnet missing")
}

# Load required packages
suppressMessages({
  library(nnet)
})

# Formula interface
fit1 <- nnet::nnet(Sepal.Length ~ . + I(Petal.Width^2), size = 2, data = iris,
                   linout = TRUE, trace = FALSE)

# Matrix interface
mm <- model.matrix(Sepal.Length ~ . - 1, data = iris)
fit2 <- nnet::nnet(x = mm, y = iris$Sepal.Length, size = 2, data = iris,
                   linout = TRUE, trace = FALSE)

# Expectations
expect_identical(
  current = vip:::get_feature_names.nnet(fit1),
  target = setdiff(x = names(iris), y = "Sepal.Length")
)
expect_error(
  current = vip:::get_feature_names.nnet(fit2)
)

