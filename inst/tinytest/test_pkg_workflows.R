# Exits
if (!requireNamespace("workflows", quietly = TRUE)) {
  exit_file("Package workflows missing")
}

# Load required packages
suppressMessages({
  library(workflows)
})

# Generate Friedman benchmark data
friedman1 <- gen_friedman(seed = 101)

# Fit a linear model
lin <- parsnip::linear_reg() %>%
  parsnip::set_engine("lm")

wf <- workflows::workflow() %>%
  workflows::add_model(lin) %>%
  workflows::add_formula(y ~ .)

lin_fit <- wf %>%
  parsnip::fit(data = friedman1)

# Compute model-based VI scores
vis <- vi(lin_fit, scale = TRUE)

# Expect `vi()` and `vi_model()` to both work
expect_identical(
  current = vi(lin_fit, sort = FALSE),
  target = vi_model(lin_fit)
)

# Check class
expect_identical(class(vis), target = c("vi", "tbl_df", "tbl", "data.frame"))

# Check dimensions (should be one row for each feature)
expect_identical(ncol(friedman1) - 1L, target = nrow(vis))

# Display VIP
vip(vis, geom = "point")

# Try permutation importance
set.seed(953)  # for reproducibility
p <- vip(
  object = lin_fit,
  method = "permute",
  train = friedman1,
  target = "y",
  pred_wrapper = predict,
  metric = "rmse",
  nsim = 30,
  geom = "violin",
  jitter = TRUE,
  all_permutation = TRUE,
  mapping = ggplot2::aes(color = Variable)
)
expect_true(inherits(p, what = "ggplot"))
p  # display VIP
