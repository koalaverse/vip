context("check-output")  # Our file is called "test-check_output.R"
library(testthat)        # load testthat package
library(prettyglm)       # load our package
data("bank")

# Easiest way to convert multiple columns to a factor.
columns_to_factor <- c('job',
                       'marital',
                       'education',
                       'default',
                       'housing',
                       'loan')
bank_data  <- bank_data  %>%
  dplyr::mutate_at(columns_to_factor, list(~factor(.))) # multiple columns to factor

# Build model
deposit_model <- stats::glm(y ~ job +
                              marital +
                              education +
                              default +
                              loan,
                            data = bank_data,
                            family = binomial)

# Test whether the output is a data frame
test_that("overview_tab() returns a data frame", {
  output_table <- pretty_coefficients(deposit_model, type_iii = 'Wald', return_data = T)
  expect_is(output_table, "data.frame")
})
