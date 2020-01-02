# Generate Friedman benchmark data
friedman2 <- gen_friedman(seed = 101, n_bins = 2)

# Tests for package C50
if (require(C50, quietly = TRUE)) {

  # Fit model(s)
  fit1 <- C50::C5.0(y ~ ., friedman2)
  fit2 <- C50::C5.0(x = friedman2[, paste0("x", 1L:10L)], y = friedman2$y)

  # Compute VI scores
  vis1 <- vi_model(fit1)
  vis2 <- vi_model(fit2)

  # Expectations for `vi_model()`
  expect_identical(
    current = vis1,
    target = vis2
  )
  expect_identical(
    current = vis1$Importance,
    C50::C5imp(fit1, metric = "usage")$Overall
  )
  expect_identical(
    current = vi_model(fit1, type = "splits", pct = FALSE)$Importance,
    C50::C5imp(fit1, metric = "splits", pct = FALSE)$Overall
  )

  # Expectations for `get_training_data()`
  expect_identical(
    current = vip:::get_training_data.C5.0(fit1),
    target = friedman2
  )
  expect_identical(
    current = vip:::get_training_data.C5.0(fit2),
    target = subset(friedman2, select = -y)
  )

  # Expectations for `get_feature_names()`
  expect_identical(
    current = vip:::get_feature_names.C5.0(fit1),
    target = paste0("x", 1L:10L)
  )
  expect_identical(
    current = vip:::get_feature_names.C5.0(fit2),
    target = paste0("x", 1L:10L)
  )

  # Call `vip::vip()` directly
  p <- vip(fit2, method = "model", type = "splits", pct = FALSE,
           include_type = TRUE)

  # Expect `p` to be a `"gg" "ggplot"` object
  expect_identical(
    current = class(p),
    target = c("gg", "ggplot")
  )

  # Display VIPs side by side
  grid.arrange(
    vip(vis1, include_type = TRUE),
    vip(vis2, include_type = TRUE),
    p,
    nrow = 1
  )

}
