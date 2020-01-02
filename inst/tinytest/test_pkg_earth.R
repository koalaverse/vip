# Generate Friedman benchmark data
friedman1 <- gen_friedman(seed = 101, n_bins = 2)

# Tests for package C50
if (require(earth, quietly = TRUE)) {

  # Fit model(s)
  fit <- earth::earth(y ~ ., degree = 2, data = friedman1)

  # Compute VI scores
  vis_nsubsets <- vi_model(fit)
  vis_rss <- vi_model(fit, type = "rss")
  vis_gcv <- vi_model(fit, type = "gcv")
  vis_earth <- earth::evimp(fit)

  # Expectations for `vi_model()`
  expect_identical(
    current = vis_nsubsets[seq_len(nrow(vis_earth)), ]$Importance,
    target = unname(vis_earth[, "nsubsets", drop = TRUE])
  )
  expect_identical(
    current = vis_rss[seq_len(nrow(vis_earth)), ]$Importance,
    target = unname(vis_earth[, "rss", drop = TRUE])
  )
  expect_identical(
    current = vis_gcv[seq_len(nrow(vis_earth)), ]$Importance,
    target = unname(vis_earth[, "gcv", drop = TRUE])
  )

  # Expectations for `get_training_data()`
  expect_identical(
    current = vip:::get_training_data.earth(fit),
    target = friedman1
  )

  # Expectations for `get_feature_names()`
  expect_identical(
    current = vip:::get_feature_names.earth(fit),
    target = paste0("x", 1L:10L)
  )

  # Call `vip::vip()` directly
  p <- vip(fit, method = "model", include_type = TRUE)

  # Expect `p` to be a `"gg" "ggplot"` object
  expect_identical(
    current = class(p),
    target = c("gg", "ggplot")
  )

  # Display VIPs side by side
  grid.arrange(
    vip(vis_nsubsets, include_type = TRUE),
    vip(vis_rss, include_type = TRUE),
    vip(vis_gcv, include_type = TRUE),
    p,
    nrow = 2
  )

}
