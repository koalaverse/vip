
if ( requireNamespace("tinytest", quietly = TRUE) ){
  home <- length(unclass(packageVersion("gbm"))[[1L]]) == 4
  tinytest::test_package("gbm", at_home = home)
}

