## use shift-cmd-t to run all tests
library(testthat)
test_check("radiant.model")
# if (interactive() && !exists("coverage_test")) devtools::run_examples()
# devtools::run_examples(start = "single_prop")
