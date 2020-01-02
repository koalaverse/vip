
if ( requireNamespace("tinytest", quietly=TRUE) ){
  home <- length(unclass(packageVersion("vip"))[[1L]]) == 4
  tinytest::test_package("vip", at_home = home)
}
