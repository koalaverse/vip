# Run tests in local environment
if (requireNamespace("tinytest", quietly = TRUE)) {
  home <- length(unclass(packageVersion("pdp"))[[1]]) == 4
  tinytest::test_package("pdp", at_home = home)
}
