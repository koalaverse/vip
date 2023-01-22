# Check dependencies
if (!requireNamespace("pdp", quietly = TRUE)) {
  exit_file("Package 'pdp' missing")
}

# Generate Friedman benchmark data
friedman1 <-  gen_friedman(seed = 101)

# Fit model
fit <- stats::lm(y ~ sin(pi * x1 * x2) + I((x3 - 0.5) ^ 2) + x4 + x5 + x6 + x7 +
                   x8 + x9 + x10, data = friedman1)

# Function to run expectations
expectations <- function(object) {

  # Check class
  expect_identical(class(object),
                   target = c("vi", "tbl_df", "tbl", "data.frame"))

  # Check dimensions (should be one row for each feature)
  expect_identical(ncol(friedman1) - 1L, target = nrow(object))

  # Check top five predictors
  expect_true(all(paste0("x", 1L:5L) %in% object$Variable[1L:5L]))

  # Check attributes
  expect_true("effects" %in% names(attributes(object)))

}

# Compute PDP-based importance
vis_firm <- vi(fit, method = "firm", train = friedman1,
               feature_names = paste0("x", 1L:10L))
expectations(vis_firm)

# Compute ICE-based importance
vis_firm_ice <- vi(fit, method = "firm", ice = TRUE, train = friedman1,
                   feature_names = paste0("x", 1L:10L))
expectations(vis_firm_ice)

# Compute PDP-based importance with custom variance function
vfuns <- list("cat" = stats::mad, "con" = stats::mad)
vis_firm_mad <- vi(fit, method = "firm", train = friedman1,
                   feature_names = paste0("x", 1L:10L), var_fun = vfuns)
expectations(vis_firm_mad)

# Compute PDP-based importance with custom variance function
vis_firm_ice_mad <- vi(fit, method = "firm", ice = TRUE, train = friedman1,
                       feature_names = paste0("x", 1L:10L), var_fun = vfuns)
expectations(vis_firm_ice_mad)
