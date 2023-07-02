# library(radiant.model)
# library(testthat)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

######### tests ########
context("Linear regression (regress)")

test_that("regress", {
  result <- regress(diamonds, "price", c("carat", "clarity"))
  res1 <- capture.output(summary(result))[10] %>% trim()
  # cat(paste0(res1, "\n"))
  res2 <- "carat           8438.030    51.101 165.125  < .001 ***"
  expect_equal(res1, res2)

  result <- regress(diamonds, "price", "carat:clarity")
  res1 <- capture.output(summary(result))[10] %>% trim()
  expect_equal(res1, res2)

  res1 <- capture.output(summary(result)) %>% trim()
  # cat(paste0(res1,"\n"), file = "~/GitHub/radiant/tests/testthat/output/regression1.txt")
  ## full output - cannot open file when testing the tests
  res2 <- paste0(readLines("output/regress1.txt")) %>% trim()
  expect_equal(res1, res2)
})

test_that("regress - predict", {
  result <- regress(diamonds, "price", c("carat", "clarity"))
  res1 <- capture.output(predict(result, pred_cmd = "carat = 1:10"))[17] %>% trim()
  # cat(paste0(res1, "\n"))
  res2 <- "SI1     9  72769.811 71948.301 73591.322 821.511"
  expect_equal(res1, res2)

  result <- regress(diamonds, "price", "carat:clarity")
  res1 <- capture.output(predict(result, pred_cmd = "carat = 1:10"))[17] %>% trim()
  expect_equal(res1, res2)
})

test_that("regress - predict with quadratic term", {
  result <- regress(diamonds, "price", c("carat", "clarity"), int = "I(carat^2)")
  res1 <- capture.output(predict(result, pred_cmd = "carat = 1:10"))[17] %>% trim()
  cat(paste0(res1, "\n"))
  res2 <- "SI1     9 114304.420 104924.680 123684.159  9379.739"
  expect_equal(res1, res2)

  result <- regress(diamonds, "price", "carat:clarity", int = "I(carat^2)")
  res1 <- capture.output(predict(result, pred_cmd = "carat = 1:10"))[17] %>% trim()
  expect_equal(res1, res2)
})

test_that("regress - predict with date", {
  result <- regress(diamonds, "price", c("carat", "clarity", "date"))
  res1 <- capture.output(predict(result, pred_cmd = "carat = 1:10"))[17] %>% trim()
  res2 <- "SI1 2012-03-19     9  72719.464 71896.008 73542.920 823.456"
  expect_equal(res1, res2)
  res1 <- capture.output(predict(result, pred_cmd = "date = '2012-1-1'"))[9] %>% trim()
  res2 <- "0.794     SI1 2012-01-01   3471.070 3357.438 3584.701 113.631"
  expect_equal(res1, res2)
})


context("Logistic regression (logistic)")

test_that("logistic", {
  result <- logistic(titanic, "survived", c("pclass", "sex"))
  res1 <- capture.output(summary(result))[13] %>% trim()
  cat(paste0(res1, "\n"))
  res2 <- "sex|male    0.080 -92.0%      -2.522     0.163 -15.447  < .001 ***"
  expect_equal(res1, res2)
  result <- logistic(titanic, "survived", "pclass:sex")
  res1 <- capture.output(summary(result))[13] %>% trim()
  expect_equal(res1, res2)
})

test_that("logistic - predict", {
  result <- logistic(titanic, "survived", c("pclass", "sex"))
  res1 <- capture.output(predict(result, pred_cmd = "pclass = levels(pclass); sex = 'female'"))[11] %>% trim()
  cat(paste0(res1, "\n"))
  res2 <- "2nd female      0.779 0.712 0.833"
  expect_equal(res1, res2)

  result <- logistic(titanic, "survived", "pclass:sex")
  res1 <- capture.output(predict(result, pred_cmd = "pclass = levels(pclass); sex = 'female'"))[11] %>% trim()
  expect_equal(res1, res2)

  res1 <- capture.output(predict(result, pred_data = titanic))[11] %>% trim()
  cat(paste0(res1, "\n"))
  res2 <- "1st female      0.896 0.856 0.926"
  expect_equal(res1, res2)
})

test_that("logistic - predict with quadratic term", {
  result <- logistic(titanic, "survived", c("pclass", "sex", "age"), int = "I(age^2)")
  res1 <- capture.output(predict(result, pred_cmd = "pclass = levels(pclass); sex = 'female'; age = 1:100"))[11] %>% trim()
  # cat(paste0(res1, "\n"))
  res2 <- "1st female   1      0.976 0.952 0.988"
  expect_equal(res1, res2)

  result <- logistic(titanic, "survived", "pclass:age", int = "I(age^2)")
  res1 <- capture.output(predict(result, pred_cmd = "pclass = levels(pclass); sex = 'female'; age = 1:100"))[11] %>% trim()
  expect_equal(res1, res2)

  res1 <- capture.output(predict(result, pred_data = titanic))[11] %>% trim()
  cat(paste0(res1, "\n"))
  res2 <- "1st female 29.000      0.919 0.880 0.945"
  expect_equal(res1, res2)
})

context("Neural Network (nn)")

test_that("Neural Network - predict for classification", {
  result <- nn(titanic, "survived", c("pclass", "sex"), seed = 1234)
  res1 <- capture.output(predict(result, pred_cmd = "pclass = levels(pclass); sex = 'female'", dec = 1))[10] %>% trim()
  # cat(paste0(res1, "\n"))
  res2 <- "2nd female        0.8"
  expect_equal(res1, res2)

  result <- nn(titanic, "survived", "pclass:sex", seed = 1234)
  res1 <- capture.output(predict(result, pred_cmd = "pclass = levels(pclass); sex = 'female'", dec = 1))[10] %>% trim()
  expect_equal(res1, res2)

  res1 <- capture.output(predict(result, pred_data = titanic, dec = 1))[10] %>% trim()
  # cat(paste0(res1, "\n"))
  res2 <- "1st female        0.9"
  expect_equal(res1, res2)
})

test_that("Neural Network - predict for regression", {
  result <- nn(diamonds, "price", c("carat", "clarity"), type = "regression", seed = 1234)
  res1 <- capture.output(predict(result, pred_cmd = "carat = 1:10", dec = 1))[16] %>% trim()
  # cat(paste0(res1, "\n"))
  res2 <- "SI1     9    18466.7"
  expect_equal(res1, res2)

  result <- nn(diamonds, "price", "carat:clarity", type = "regression", seed = 1234)
  res1 <- capture.output(predict(result, pred_cmd = "carat = 1:10", dec = 1))[16] %>% trim()
  expect_equal(res1, res2)

  res1 <- capture.output(predict(result, pred_data = diamonds, dec = 1))[16] %>% trim()
  # cat(paste0(res1, "\n"))
  res2 <- "0.9     SI1     3997.9"
  expect_equal(res1, res2)
})

test_that("Neural Network - predict with date", {
  result <- nn(diamonds, "price", c("carat", "clarity", "date"), type = "regression", seed = 1234)
  res1 <- capture.output(predict(result, pred_cmd = "carat = 1:10"))[17] %>% trim()
  res2 <- "SI1 2012-03-19    10   3907.186"
  expect_equal(res1, res2)
  res1 <- capture.output(predict(result, pred_cmd = "date = '2012-1-1'"))[8] %>% trim()
  res2 <- "0.794     SI1 2012-01-01   3907.186"
  expect_equal(res1, res2)
})

context("Gradient Boosted Trees (gbt)")

test_that("Gradient Boosting - NoLD test", {
  result <- gbt(titanic, "survived", c("pclass", "sex"), lev = "Yes", early_stopping_rounds = 0)
  res1 <- round(result$model$importance$Gain, 3)
  res2 <- c(0.758, 0.210, 0.032)
  expect_equal(res1, res2, tolerance = 1e-3)
})

# context("Linear regression (plot.regress)")

# test_that("regress - plots", {
#   result <- regress(diamonds, "price", c("carat", "clarity"))
#   grb <- plot(result, plots = "dashboard", shiny = TRUE)
#   expect_true(all(c("patchwork", "gg", "ggplot") %in% class(grb)))
#   unlink("Rplots.pdf")
# })
