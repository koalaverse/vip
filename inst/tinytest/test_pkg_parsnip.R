# Generate Friedman benchmark data
friedman1 <- gen_friedman(seed = 101)

# Tests for package parsnip
if (require(parsnip, quietly = TRUE) && require(ggplot2, quietly = TRUE)) {

  # Fit a linear model
  lin_mod <- parsnip::linear_reg() %>%
    set_engine("lm") %>%
    fit(y ~ ., data = friedman1)

  # Compute model-based VI scores
  vis <- vi(lin_mod, scale = TRUE)

  # Check class
  expect_identical(class(vis), target = c("vi", "tbl_df", "tbl", "data.frame"))

  # Check dimensions (should be one row for each feature)
  expect_identical(ncol(friedman1) - 1L, target = nrow(vis))

  # Display VIP
  vip(vis, geom = "point")

  # Try permutation importance
  set.seed(953)  # for reproducibility
  p <- vip(
    object = lin_mod,
    method = "permute",
    train = friedman1,
    target = "y",
    pred_wrapper = predict,
    metric = "rmse",
    nsim = 30,
    geom = "violin",
    jitter = TRUE,
    all_permutation = TRUE,
    mapping = aes(color = Variable)
  )
  expect_true(inherits(p, what = "ggplot"))
  p  # display VIP

}
