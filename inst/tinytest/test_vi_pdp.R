# Generate Friedman benchmark data
friedman1 <-  gen_friedman(seed = 101)

# Fit model
fit <- stats::lm(y ~ sin(pi * x1 * x2) + I((x3 - 0.5) ^ 2) + x4 + x5 + x6 + x7 +
                   x8 + x9 + x10, data = friedman1)

# Compute PDP-based importance
vis <- vi_pdp(fit, train = friedman1, feature_names = paste0("x", 1L:10L))
vis_mad <- vi_pdp(fit, train = friedman1, feature_names = paste0("x", 1L:10L),
                  var_fun = list(
                    "cat" = stats::mad,
                    "con" = stats::mad
                  ))

# Expectations for `vi_pdp()`
expect_identical(
  current = class(vis),
  target = c("vi", "tbl_df", "tbl", "data.frame")
)
expect_identical(
  current = paste0("x", 1L:5L),
  target = sort(vis$Variable[1L:5L])
)
expect_identical(
  current = class(vis_mad),
  target = c("vi", "tbl_df", "tbl", "data.frame")
)
expect_identical(
  current = paste0("x", 1L:5L),
  target = sort(vis_mad$Variable[1L:5L])
)

# Load requiuired packages
library(ggplot2)

# Display VIPs
grid.arrange(
  vip(vis, include_type = TRUE) + ggtitle("stats::sd()"),
  vip(vis_mad, include_type = TRUE) + ggtitle("stats::mad()"),
  nrow = 1
)

# Displayy PDPs in a grid
grid.arrange(grobs = lapply(attr(vis, which = "pdp"), FUN = function(x) {
  autoplot(x, alpha = 0.1) + ylim(0, 30)
}), nrow = 2)
